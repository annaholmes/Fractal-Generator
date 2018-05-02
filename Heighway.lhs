> {-# LANGUAGE TypeFamilies              #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts          #-}
> 
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.Prelude
> import Diagrams.TwoD.Layout.Grid
> nextDragon trail = (trail # rotateBy (-1/8) 
>                     <> trail # rotateBy (5/8) # reverseTrail) 
>                    # scale (1/sqrt 2)
> dragonCurves = map (trailLike . (`at` origin)) (iterate nextDragon initialTrail)
>   where
>     initialTrail = hrule 1
> withPrevious diagrams = zipWith (<>) diagrams (mempty : diagrams # opacity 0.2)
> rememberOrder :: [Diagram B] -> [Diagram B]
> rememberOrder = zipWith named [0::Int ..]
>
> showOrder :: Diagram B -> Diagram B
> showOrder diagram 
>   = diagram # applyAll (map addArrow [0 .. length (names diagram)])
>   where
>     addArrow n = connectOutside' opts n (n + 1)
>     opts = with & gaps .~ normalized 0.005 
>                 & headLength .~ (tiny :: Measure Double)
>
> example = dragonCurves # withPrevious 
>                        # take 12
>                        # sameBoundingRect
>                        # rememberOrder 
>                        # map (frame 0.1) 
>                        # gridSnake
>                        # showOrder
>                        # lw ultraThin
> 
> main = mainWith (example :: Diagram B)