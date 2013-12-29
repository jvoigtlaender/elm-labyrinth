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
xy2ij (x,y) = (round ((1+x/2+toFloat mi)/2), round ((1-y/2+toFloat mj)/2))
ij2xy (i,j) = (2*toFloat(2*i-mi-1), 2*toFloat(mj-2*j+1))

closer_than d (x1,y1) (x2,y2) = (x1-x2)^2+(y1-y2)^2 < d^2

maze_list =
   concatMap
   (\(j,l) -> map (\(i,_) -> (i,j)) (filter (\(_,c) -> c/=' ') (zip [1..mi] (String.toList l))))
   (zip [1..mj] maze)

maze_set = Set.fromList maze_list

data HunterState = Stalled (Int,Int) | Moving ((Int,Int),(Int,Int))

hunter_steps =
  foldp
  (\ds (b,s) -> if s+ds>1 then (True,s+ds-1) else (False,s+ds)) 
  (False, 0)
  (lift2 (\dt sl -> dt/4/sl) frame (sampleOn frame slowness_hunters))

game_state = 
  let
    smoothen s = map (\h -> case h of
                              Stalled p          -> ij2xy p
                              Moving (p,(di,dj)) -> let (x,y) = ij2xy p
                                                    in (x+toFloat(di)*s*4,y-toFloat(dj)*s*4))
  in
   sampleOn frame'
   <|
   lift2 (\s state -> { state
                      | player <- fst (state.player)  
                      , hunters <- smoothen (clamp 0 1 s) state.hunters })
   (snd <~ hunter_steps)
   <|
   foldp (<|)
   { player = (ij2xy player, (0,0))
   , hunters = map Stalled hunters
   , gems = Set.diff (Set.fromList (concatMap (\j -> map (\i -> (i,j)) [1..mi]) [1..mj])) maze_set }
   (merge
    (move_player <~ frame' ~ sampleOn frame' (lift3 (,,) walking player_input slowness_player))
    (move_hunters <~ Random.floatList (lift (\_ -> 2*(length hunters)) (keepIf id False (fst <~ hunter_steps)))))

move_player dt (walk,input,sl) state =
  let
    ((x,y),(dx,dy)) = state.player
    dir = input (x,y)
    (ndx,ndy) = if walk && isNothing dir then (dx,dy) else case dir of {Nothing -> (0,0); Just {x,y} -> (x,y)}
    x' = x + dt / sl * ndx
    y' = y + dt / sl * ndy
  in
   { state
   | player <-
       if isEmpty . Set.toList . Set.intersect maze_set . Set.fromList 
          . map (xy2ij . (\(u,v) -> (x'+u,y'+v)))
          <| [(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1),(1,0)]
       then ((x',y'),(ndx,ndy))
       else ((x,y),(0,0))
   , gems <-
       let g = xy2ij (x',y')
       in if closer_than (3/4) (x',y') (ij2xy g)
          then Set.remove g state.gems
          else state.gems }

player_input = lift2
               (\{x,y} (ix,iy,keep) (px,py) ->
                                          if x==0 && y==0
                                          then
                                            if closer_than 1 (ix,iy) (px,py)
                                            then Just {x = 0, y = 0}
                                            else if keep (ix,iy) (px,py)
                                                 then let dx = ix-px
                                                          dy = iy-py
                                                      in Just {x = if abs dx > 3/4 then sgn dx else 0,
                                                               y = if abs dy > 3/4 then sgn dy else 0}
                                                 else Nothing
                                          else Just {x = x, y = y})
               Keyboard.arrows
               (lift2
                (\ts (mx,my) -> let (ox,oy) = ij2xy (0,0)
                                in head (map (\(x,y) -> (ox+x,oy-y,\_ _ -> True)) ts
                                         ++ [(ox+mx,oy-my,closer_than 8)]))
                (lift
                 (filter (\(x,y) -> 2<x && x<4*mi+2 && 2<y && y<4*mj+2))
                 (lift2 (\unit -> map (\{x,y} -> ((toFloat x)/unit,(toFloat y)/unit))) unit Touch.touches))
                (lift2 (\unit (x,y) -> ((toFloat x)/unit,(toFloat y)/unit)) unit Mouse.position))

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

outcome = foldp
          (\((t, { player, hunters, gems }), t0) { caught, won }
           -> { caught = List.any (closer_than 3 player) hunters || caught
              , won = maybe (if isEmpty (Set.toList gems) then Just (t-t0) else Nothing) Just won })
          { caught = False, won = Nothing }
          (lift2 (,) (timestamp game_state) timeStampAtStart) 

(boxWalking,walking) = Graphics.Input.checkbox True

(menuPlayer,slowness_player) = Graphics.Input.dropDown (map (\i -> (if i>0 then "+" ++ show i else show i,(18-i)*5)) [0,1,2,3,-1,-2,-3])

(menuHunters,slowness_hunters) = Graphics.Input.dropDown (map (\i -> (if i>0 then "+" ++ show i else show i,(22-i)*5)) [0,1,2,3,-1,-2,-3])

(menuUnit,unit) = Graphics.Input.dropDown (map (\i -> (if i>0 then "+" ++ show i else show i,(5+i)*2)) [0,1,2,3,-1,-2,-3])

display { caught, won } { player, hunters, gems } player_color boxWalking menuPlayer menuHunters menuUnit unit -- frequ
  =
  let
    wi = 4*unit*(mi+1)
    he = 4*unit*(mj+1)
    unit' = toFloat unit
  in
   if caught || isJust won
   then
     flow down <|
     map (uncurry <| container wi (he `div` 2))
     [ (midBottom, if isJust won
                   then
                     let after = toFloat(truncate ((\(Just t) -> t) won) `div` 100)/10
                     in text . monospace . Text.height (3*unit') . toText <| "YOU WON (" ++ show after ++ "s)!"
                   else text . monospace . Text.height (4*unit') . toText <| "GAME OVER!")
     , (midTop, text . monospace . Text.height (2*unit') . toText <| "(reload to start again)")
     ]
   else
     flow down
     [ collage wi he <|
       map (\g -> move (let (x,y) = ij2xy g in (x*unit',y*unit')) (filled yellow (ngon 5 (unit'/2)))) (Set.toList gems)
       ++
       [move (let (x,y) = player in (x*unit',y*unit')) (player_color (circle unit'))]
       ++
       map (\(x,y) -> move (x*unit',y*unit') (filled black (circle (2*unit')))) hunters
       ++
       map (\b -> move (let (x,y) = ij2xy b in (x*unit',y*unit')) (filled blue (square (4*unit')))) maze_list
     , flow right [ spacer unit unit
                  , container (42*unit) (3*unit) midLeft
                    (text . Text.color red . monospace . Text.height (2*unit') . toText <| "use arrows, mouse or touch to steer") ]
     , flow right [ spacer unit unit
                  , container (28*unit) (3*unit) midLeft
                    (text . monospace . Text.height (2*unit') . toText <| "keep walking direction:")
                  , let d = max 20 (2*unit) in container d (3*unit) middle boxWalking ]
     , flow right [ spacer unit unit
                  , container (25*unit) (3*unit) midLeft
                    (text . monospace . Text.height (2*unit') . toText <| "adjust player speed:")
                  , let d = max 20 (2*unit) in container (3*d) (3*unit) middle menuPlayer ]
     , flow right [ spacer unit unit
                  , container (25*unit) (3*unit) midLeft
                    (text . monospace . Text.height (2*unit') . toText <| "adjust hunters speed:")
                  , let d = max 20 (2*unit) in container (3*d) (3*unit) middle menuHunters ]
     , flow right [ spacer unit unit
                  , container (37*unit) (3*unit) midLeft
                    (text . monospace . Text.height (2*unit') . toText <| "adjust graphics scaling factor:")
                  , let d = max 20 (2*unit) in container (3*d) (3*unit) middle menuUnit ]
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
