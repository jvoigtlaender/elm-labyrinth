import String
import Maybe exposing (..)
import Maybe.Extra exposing (..)
import List exposing (..)
import Set
import Random
import Keyboard
import Text exposing (monospace)
import Mouse
import Touch
import Graphics.Input
import Signal exposing (..)
import Time exposing (..)
import AnimationFrame
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

maze =
   [
     "XXXXXXXXXXX"
   , "X         X"
   , "X XX XXXX X"
   , "X XX   XX X"
   , "X X  X  X X"
   , "X X XXX X X"
   , "X   X     X"
   , "XXX   XXXXX"
   , "X X X XXX X"
   , "X   X     X"
   , "XXXXXXXXXXX"
   ]

player = (1,1)

hunters = [(4,4),(4,7),(5,6),(9,1)]

--------------------------------------

frame = AnimationFrame.frame

makeInputElement : a -> (Signal.Mailbox a -> Element) -> (Element, Signal a)
makeInputElement a f = let mbx = Signal.mailbox a
                       in (f mbx, mbx.signal)

player_color = Signal.map (\t -> filled (hsl (t / 750) 1 0.5)) (every 100)

head' xs = case head xs of
             Just x -> x
             _ -> Debug.crash "This cannot happen!"

floatList ns =
  let next n seed = let (l, seed') = Random.generate (Random.list n (Random.float 0 1)) seed
                    in (l, Just seed')
  in Signal.map fst <| foldp (\(n,seed0) (_, mseed) -> mapDefault (next n seed0) (next n) mseed)
                             ([], Nothing)
                             (Signal.map2 (\n -> (,) n << Random.initialSeed << truncate) ns timeStampAtStart)

sgn x = if x<0 then -1 else if x>0 then 1 else 0

timeStampAtStart = Signal.map fst (timestamp (constant ()))

frame' = delay 0 frame

mi = String.length (head' maze)
mj = length maze

type alias XY = (Float,Float)
type alias IJ = (Int,Int)

xy2ij (x,y) = (round(x+0.5), round(0.5-y))
ij2xy (i,j) = (-0.5+toFloat(i), 0.5-toFloat(j))

(ox,oy) = ij2xy (-1,-1)

screen2xy unit (x,y) = (ox+toFloat(x)/unit,oy-toFloat(y)/unit)
xy2screen unit (x,y) = ((x-ox-0.5-toFloat(mi)/2)*unit,(y-oy+0.5+toFloat(mj)/2)*unit)

closer_than d (x1,y1) (x2,y2) = (x1-x2)^2+(y1-y2)^2 < d^2

maze_list =
   concatMap
   (\(j,l) -> List.filterMap (\(i,c) -> if c==' ' then Nothing else Just (i,j)) (indexedMap (,) (String.toList l)))
   (indexedMap (,) maze)

maze_set = Set.fromList maze_list

hunter_steps =
  foldp
  (\ds (b,s) -> if s+ds>1 then (True,s+ds-1) else (False,s+ds)) 
  (False, 0)
  (Signal.map2 (/) frame (sampleOn frame slowness_hunters))

type alias Dir = (Int,Int) -- only -1, 0, or 1

type HunterState = Stalled IJ | Moving (IJ,Dir)

initially : { player : (XY,Dir), hunters : List HunterState, gems : Set.Set IJ }
initially = 
   { player = (ij2xy player, (0,0))
   , hunters = List.map Stalled hunters
   , gems = Set.diff (Set.fromList (concatMap (\j -> List.map (\i -> (i,j)) [0..mi-1]) [0..mj-1])) maze_set }

game_state : Signal { player : XY, hunters : List XY, gems : Set.Set IJ }
game_state = 
  let
    smoothen s h = case h of
                     Stalled p          -> ij2xy p
                     Moving (p,(di,dj)) -> let (x,y) = ij2xy p
                                           in (x+toFloat(di)*s,y-toFloat(dj)*s)
    n = 2*(length hunters)
  in
   sampleOn frame'
   <|
   Signal.map2 (\(_,s) state -> { state
                                | player = fst (state.player)
                                , hunters = List.map (smoothen (clamp 0 1 s)) state.hunters })
   hunter_steps
   <|
   foldp (<|)
   initially
   (merge
    (Signal.map2 move_player frame' (sampleOn frame' (Signal.map3 (,,) walking player_input slowness_player)))
    (Signal.map move_hunters <| floatList (Signal.filterMap (\(b,_) -> if b then Just n else Nothing) n hunter_steps)))

player_input : Signal (XY -> Maybe Dir)
player_input = Signal.map4
               (\{x,y} u ts m ->
                let ((ix,iy),keep) = head' (List.filter (\(xy,_) -> let (i,j) = xy2ij xy
                                                                    in 0<=i && i<mi && 0<=j && j<mj)
                                            (List.map (\{x,y} -> (screen2xy u (x,y),\_ _ -> True)) ts)
                                            ++ [(screen2xy u m,closer_than 2)])
                in
                 \(px,py) ->
                 if x==0 && y==0
                 then
                   if closer_than 0.25 (ix,iy) (px,py)
                   then Just (0,0)
                   else if keep (ix,iy) (px,py)
                        then let dx = ix-px
                                 dy = iy-py
                             in Just (if abs dx > 0.1875 then sgn dx else 0,
                                      if abs dy > 0.1875 then sgn dy else 0)
                        else Nothing
                 else Just (x,y))
               Keyboard.arrows
               unit
               Touch.touches
               Mouse.position

move_player : Time -> (Bool, XY -> Maybe Dir, Float) -> { a | player : (XY,Dir), gems : Set.Set IJ }
                                                     -> { a | player : (XY,Dir), gems : Set.Set IJ }
move_player dt (walk,input,sl) state =
  let
    ((x,y),(dx,dy)) = state.player
    dir = input (x,y)
    (ndx,ndy) = if walk && isNothing dir then (dx,dy) else withDefault (0,0) dir
    f = clamp 0 1 (dt/sl)
    x' = x+f*ndx
    y' = y+f*ndy
  in
   { state
   | player =
       if Set.isEmpty <| Set.intersect maze_set <| Set.fromList 
          <| List.map (xy2ij << (\(u,v) -> (x'+u,y'+v)))
          <| [(0.25,0.25),(0,0.25),(-0.25,0.25),(-0.25,0),(-0.25,-0.25),(0,-0.25),(0.25,-0.25),(0.25,0)]
       then ((x',y'),(ndx,ndy))
       else ((x,y),(0,0))
   , gems =
       let g = xy2ij (x',y')
       in if closer_than 0.1875 (x',y') (ij2xy g)
          then Set.remove g state.gems
          else state.gems }

move_hunters : List Float -> { a | player : (XY,b), hunters : List HunterState }
                          -> { a | player : (XY,b), hunters : List HunterState }
move_hunters rnd state =
  let
    l = length hunters
    rs = List.map (\r -> truncate (8*r)) rnd
    (pi,pj) = xy2ij (fst state.player)
    dir r (i,j) =
      if abs (pi-i) + abs (pj-j) < 2
      then (pi-i,pj-j)
      else
        case r of
          0 -> (0,1)
          1 -> (1,0)
          2 -> (0,-1)
          3 -> (-1,0)
          _ -> if r % 2 == 0
               then (0,if j<pj then 1 else if j>pj then -1 else 0) 
               else (if i<pi then 1 else if i>pi then -1 else 0,0)
    walk_on r h = case h of
                      Stalled (i,j)          -> ((i,j),dir r (i,j))
                      Moving ((i,j),(di,dj)) -> ((i+di,j+dj),(di,dj))
    hunters' = List.map2 walk_on (take l rs) state.hunters
    forbidden = Set.union maze_set (Set.fromList (List.map fst hunters'))
    check_and_patch r ((i',j'),(di,dj)) =
      if Set.member (i'+di,j'+dj) forbidden
      then
        let (di',dj') = dir r (i',j')
        in if Set.member (i'+di',j'+dj') maze_set
           then Stalled (i',j')
           else Moving ((i',j'),(di',dj'))
      else
        Moving ((i',j'),(di,dj))
  in
   { state | hunters = List.map2 check_and_patch (drop l rs) hunters' }

outcome : Signal { caught : Bool, won : Maybe Time }
outcome = foldp
          (\((t, { player, hunters, gems }), t0) { caught, won }
           -> { caught = any (closer_than 0.75 player) hunters || caught
              , won = mapDefault (if Set.isEmpty gems then Just (t-t0) else Nothing) Just won })
          { caught = False, won = Nothing }
          (Signal.map2 (,) (timestamp game_state) timeStampAtStart) 

(boxWalking,walking) = makeInputElement True (\c -> Graphics.Input.checkbox (Signal.message c.address) True)

myMenu : (number -> number) -> List number -> (Element, Signal number)
myMenu f ns = let options = List.map (\i -> (if i>0 then "+" ++ toString i else toString i,f i)) ns
              in makeInputElement (snd (head' options)) (\c -> Graphics.Input.dropDown (Signal.message c.address) options)

(menuPlayer,slowness_player)   = myMenu (\i -> (18-i)*20) [0,1,2,3,-1,-2,-3]
(menuHunters,slowness_hunters) = myMenu (\i -> (22-i)*20) [0,1,2,3,-1,-2,-3]
(menuUnit,unit)                = myMenu (\i -> (5+i)*8) [0,1,2,3,-1,-2,-3]

display { caught, won } { player, hunters, gems } player_color unit -- frequ
  =
  let
    wi = (mi+1)*unit
    he = (mj+1)*unit
    unit' = toFloat(unit)
    xy2sc = xy2screen unit'
  in
   if caught || isJust won
   then
     flow down <|
     List.map (uncurry <| container wi (he // 2))
     [ (midBottom, case won of
                   Just t ->
                     let after = toFloat(truncate t // 100)/10
                     in leftAligned <| monospace <| Text.height (0.75*unit') <| Text.fromString <| "YOU WON (" ++ toString after ++ "s)!"
                   _ -> leftAligned <| monospace <| Text.height unit' <| Text.fromString <| "GAME OVER!")
     , (midTop, leftAligned <| monospace <| Text.height (unit'/2) <| Text.fromString <| "(reload to start again)")
     ]
   else
     flow down
     [ collage wi he <|
       List.map (\g -> move (xy2sc (ij2xy g)) (filled yellow (ngon 5 (unit'/8)))) (Set.toList gems)
       ++
       [move (xy2sc player) (player_color (circle (unit'/4)))]
       ++
       List.map (\h -> move (xy2sc h) (filled black (circle (unit'/2)))) hunters
       ++
       List.map (\b -> move (xy2sc (ij2xy b)) (filled blue (square unit'))) maze_list
     , flow right [ spacer unit unit
                  , container (11*unit) (3*unit // 4) midLeft
                    (leftAligned <| Text.color red <| monospace <| Text.height (unit'/2) <| Text.fromString <| "use arrows, mouse or touch to steer") ]
     , flow right [ spacer unit unit
                  , container (7*unit) (3*unit // 4) midLeft
                    (leftAligned <| monospace <| Text.height (unit'/2) <| Text.fromString <| "keep walking direction:")
                  , let d = max 20 (unit // 2) in container d (3*unit // 4) middle boxWalking ]
     , flow right [ spacer unit unit
                  , container (25*unit // 4) (3*unit // 4) midLeft
                    (leftAligned <| monospace <| Text.height (unit'/2) <| Text.fromString <| "adjust player speed:")
                  , let d = max 20 (unit // 2) in container (5*d) (3*unit // 4) middle menuPlayer ]
     , flow right [ spacer unit unit
                  , container (25*unit // 4) (3*unit // 4) midLeft
                    (leftAligned <| monospace <| Text.height (unit'/2) <| Text.fromString <| "adjust hunters speed:")
                  , let d = max 20 (unit // 2) in container (5*d) (3*unit // 4) middle menuHunters ]
     , flow right [ spacer unit unit
                  , container (37*unit // 4) (3*unit // 4) midLeft
                    (leftAligned <| monospace <| Text.height (unit'/2) <| Text.fromString <| "adjust graphics scaling factor:")
                  , let d = max 20 (unit // 2) in container (5*d) (3*unit // 4) middle menuUnit ]
     -- , asText frequ
     ]

main = Signal.map4 display
       outcome
       game_state
       (sampleOn frame' player_color)
       unit
       -- <| (\(t,c) -> 1000*c/t) <~ foldp (\dt (t,c) -> (t+dt,c+1)) (0,0) frame'
