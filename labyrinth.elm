import String
import List
import Set
import Random
import Keyboard
import Mouse
import Touch
import Graphics.Input

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

player = (2,2)

hunters = [(5,5),(5,8),(6,7),(10,2)]

frame = fps 100

--------------------------------------

player_color = lift (\t -> filled (hsv (t / 750) 1 0.95)) (every 100)

sgn x = if x<0 then -1 else if x>0 then 1 else 0

timeStampAtStart = fst <~ (timestamp (constant ()))

frame' = delay 0 frame

mi = length (String.toList (head maze))
mj = length maze

type XY = (Float,Float)
type IJ = (Int,Int)

xy2ij (x,y) = (round(x+0.5), round(0.5-y))
ij2xy (i,j) = (-0.5+toFloat(i), 0.5-toFloat(j))

(ox,oy) = ij2xy (0,0)

closer_than d (x1,y1) (x2,y2) = (x1-x2)^2+(y1-y2)^2 < d^2

maze_list =
   concatMap
   (\(j,l) -> map (\(i,_) -> (i,j)) (filter (\(_,c) -> c/=' ') (zip [1..mi] (String.toList l))))
   (zip [1..mj] maze)

maze_set = Set.fromList maze_list

hunter_steps =
  foldp
  (\ds (b,s) -> if s+ds>1 then (True,s+ds-1) else (False,s+ds)) 
  (False, 0)
  (lift2 (/) frame (sampleOn frame slowness_hunters))

type Dir = (Int,Int) -- only -1, 0, or 1

data HunterState = Stalled IJ | Moving (IJ,Dir)

initially : { player : (XY,Dir), hunters : [HunterState], gems : Set.Set IJ }
initially = 
   { player = (ij2xy player, (0,0))
   , hunters = map Stalled hunters
   , gems = Set.diff (Set.fromList (concatMap (\j -> map (\i -> (i,j)) [1..mi]) [1..mj])) maze_set }

game_state : Signal { player : XY, hunters : [XY], gems : Set.Set IJ }
game_state = 
  let
    smoothen s h = case h of
                     Stalled p          -> ij2xy p
                     Moving (p,(di,dj)) -> let (x,y) = ij2xy p
                                           in (x+toFloat(di)*s,y-toFloat(dj)*s)
  in
   sampleOn frame'
   <|
   lift2 (\s state -> { state
                      | player <- fst (state.player)  
                      , hunters <- map (smoothen (clamp 0 1 s)) state.hunters })
   (snd <~ hunter_steps)
   <|
   foldp (<|)
   initially
   (merge
    (move_player <~ frame' ~ sampleOn frame' (lift3 (,,) walking player_input slowness_player))
    (move_hunters <~ Random.floatList (lift (\_ -> 2*(length hunters)) (keepIf id False (fst <~ hunter_steps)))))

player_input : Signal (XY -> Maybe Dir)
player_input = lift2
               (\{x,y} (ix,iy,keep) (px,py) ->
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
               (lift2
                (\ts m -> head (ts ++ [m]))
                (lift
                 (filter (\(x,y,_) -> let (i,j) = xy2ij (x,y) in 1<=i && i<=mi && 1<=j && j<=mj))
                 (lift2 (\unit -> map (\{x,y} -> (ox+toFloat(x)/unit,oy-toFloat(y)/unit,\_ _ -> True))) unit Touch.touches))
                (lift2 (\unit (x,y) -> (ox+toFloat(x)/unit,oy-toFloat(y)/unit,closer_than 2)) unit Mouse.position))

move_player : Time -> (Bool, XY -> Maybe Dir, Float) -> { a | player : (XY,Dir), gems : Set.Set IJ }
                                                     -> { a | player : (XY,Dir), gems : Set.Set IJ }
move_player dt (walk,input,sl) state =
  let
    ((x,y),(dx,dy)) = state.player
    dir = input (x,y)
    (ndx,ndy) = if walk && isNothing dir then (dx,dy) else case dir of {Nothing -> (0,0); Just d -> d}
    x' = x+dt/sl*ndx
    y' = y+dt/sl*ndy
  in
   { state
   | player <-
       if isEmpty . Set.toList . Set.intersect maze_set . Set.fromList 
          . map (xy2ij . (\(u,v) -> (x'+u,y'+v)))
          <| [(0.25,0.25),(0,0.25),(-0.25,0.25),(-0.25,0),(-0.25,-0.25),(0,-0.25),(0.25,-0.25),(0.25,0)]
       then ((x',y'),(ndx,ndy))
       else ((x,y),(0,0))
   , gems <-
       let g = xy2ij (x',y')
       in if closer_than 0.1875 (x',y') (ij2xy g)
          then Set.remove g state.gems
          else state.gems }

move_hunters : [Float] -> { a | player : (XY,b), hunters : [HunterState] }
                       -> { a | player : (XY,b), hunters : [HunterState] }
move_hunters rnd state =
  let
    l = length hunters
    rs = map (\r -> truncate (8*r)) rnd
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
          _ -> if r `mod` 2 == 0
               then (0,if j<pj then 1 else if j>pj then -1 else 0) 
               else (if i<pi then 1 else if i>pi then -1 else 0,0)
    walk_on (r,h) = case h of
                      Stalled (i,j)          -> ((i,j),dir r (i,j))
                      Moving ((i,j),(di,dj)) -> ((i+di,j+dj),(di,dj))
    hunters' = map walk_on (zip (take l rs) state.hunters)
    forbidden = Set.union maze_set (Set.fromList (map fst hunters'))
    check_and_patch (r,((i',j'),(di,dj))) =
      if Set.member (i'+di,j'+dj) forbidden
      then
        let (di',dj') = dir r (i',j')
        in if Set.member (i'+di',j'+dj') maze_set
           then Stalled (i',j')
           else Moving ((i',j'),(di',dj'))
      else
        Moving ((i',j'),(di,dj))
  in
   { state | hunters <- map check_and_patch (zip (drop l rs) hunters') }

outcome : Signal { caught : Bool, won : Maybe Time }
outcome = foldp
          (\((t, { player, hunters, gems }), t0) { caught, won }
           -> { caught = List.any (closer_than 0.75 player) hunters || caught
              , won = maybe (if isEmpty (Set.toList gems) then Just (t-t0) else Nothing) Just won })
          { caught = False, won = Nothing }
          (lift2 (,) (timestamp game_state) timeStampAtStart) 

(boxWalking,walking) = Graphics.Input.checkbox True

myMenu : (number -> number) -> [number] -> (Signal Element, Signal number)
myMenu f = Graphics.Input.dropDown . map (\i -> (if i>0 then "+" ++ show i else show i,f i))

(menuPlayer,slowness_player)   = myMenu (\i -> (18-i)*20) [0,1,2,3,-1,-2,-3]
(menuHunters,slowness_hunters) = myMenu (\i -> (22-i)*20) [0,1,2,3,-1,-2,-3]
(menuUnit,unit)                = myMenu (\i -> (5+i)*8) [0,1,2,3,-1,-2,-3]

display { caught, won } { player, hunters, gems } player_color boxWalking menuPlayer menuHunters menuUnit unit -- frequ
  =
  let
    wi = (mi+1)*unit
    he = (mj+1)*unit
    unit' = toFloat(unit)
  in
   if caught || isJust won
   then
     flow down <|
     map (uncurry <| container wi (he `div` 2))
     [ (midBottom, if isJust won
                   then
                     let after = toFloat(truncate ((\(Just t) -> t) won) `div` 100)/10
                     in text . monospace . Text.height (0.75*unit') . toText <| "YOU WON (" ++ show after ++ "s)!"
                   else text . monospace . Text.height unit' . toText <| "GAME OVER!")
     , (midTop, text . monospace . Text.height (unit'/2) . toText <| "(reload to start again)")
     ]
   else
     let xy2screen (x,y) = ((x-ox-0.5-toFloat(mi)/2)*unit',(y-oy+0.5+toFloat(mj)/2)*unit')
     in
     flow down
     [ collage wi he <|
       map (\g -> move (xy2screen (ij2xy g)) (filled yellow (ngon 5 (unit'/8)))) (Set.toList gems)
       ++
       [move (xy2screen player) (player_color (circle (unit'/4)))]
       ++
       map (\h -> move (xy2screen h) (filled black (circle (unit'/2)))) hunters
       ++
       map (\b -> move (xy2screen (ij2xy b)) (filled blue (square unit'))) maze_list
     , flow right [ spacer unit unit
                  , container (11*unit) (3*unit `div` 4) midLeft
                    (text . Text.color red . monospace . Text.height (unit'/2) . toText <| "use arrows, mouse or touch to steer") ]
     , flow right [ spacer unit unit
                  , container (7*unit) (3*unit `div` 4) midLeft
                    (text . monospace . Text.height (unit'/2) . toText <| "keep walking direction:")
                  , let d = max 20 (unit `div` 2) in container d (3*unit `div` 4) middle boxWalking ]
     , flow right [ spacer unit unit
                  , container (25*unit `div` 4) (3*unit `div` 4) midLeft
                    (text . monospace . Text.height (unit'/2) . toText <| "adjust player speed:")
                  , let d = max 20 (unit `div` 2) in container (3*d) (3*unit `div` 4) middle menuPlayer ]
     , flow right [ spacer unit unit
                  , container (25*unit `div` 4) (3*unit `div` 4) midLeft
                    (text . monospace . Text.height (unit'/2) . toText <| "adjust hunters speed:")
                  , let d = max 20 (unit `div` 2) in container (3*d) (3*unit `div` 4) middle menuHunters ]
     , flow right [ spacer unit unit
                  , container (37*unit `div` 4) (3*unit `div` 4) midLeft
                    (text . monospace . Text.height (unit'/2) . toText <| "adjust graphics scaling factor:")
                  , let d = max 20 (unit `div` 2) in container (3*d) (3*unit `div` 4) middle menuUnit ]
     -- , asText frequ
     ]

main = lift8 display
       outcome
       game_state
       (sampleOn frame' player_color)
       boxWalking
       menuPlayer
       menuHunters
       menuUnit
       unit
       -- <| (\(t,c) -> 1000*c/t) <~ foldp (\dt (t,c) -> (t+dt,c+1)) (0,0) frame'
