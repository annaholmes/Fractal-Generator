> {-# LANGUAGE FlexibleInstances         #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts          #-}
> {-# LANGUAGE TypeFamilies              #-}
>
> import Diagrams.Prelude
> import Diagrams.Coordinates
> import Data.Time
> import Diagrams.Backend.SVG.CmdLine
>
> clock :: UTCTime -> Diagram B
> clock t = circle 0.35 # fc silver # lwG 0
>        <> bigHand # f 12 h <> littleHand # f 60 m
>        <> circle 1  # fc black # lwG 0
>        <> circle 11 # lwG 1.5 # lc slategray # fc lightsteelblue
>   where
>     s = realToFrac $ utctDayTime t :: Double
>     m = s / 60
>     h = m / 60
>
>     bigHand    = (0 ^& (-1.5)) ~~ (0 ^& 7.5) # lwG 0.5
>     littleHand = (0 ^& (-2))   ~~ (0 ^& 9.5) # lwG 0.2
>     f n v = rotate (- v / n @@ turn)
>
> main = mainWith (clock <$> getLine')
>
> instance Mainable (Flippable (Diagram SVG V2 Double)) where
>   type MainOpts (Flippable (Diagram SVG V2 Double)) = (MainOpts (Diagram SVG V2 Double), FlipOpts)
>
>   mainRender (opts, FlipOpts f) (Flippable d) = mainRender opts ((if f then reflectX else id) d)
>
> timeFormat = "%H:%M:%S"
> understandTime = parseTimeOrError True defaultTimeLocale timeFormat

> time :: UTCTime
> time = understandTime "10:30:20"
> --instance Mainable (Flippable (Diagram SVG V2 Double)) where
> --  type MainOpts (Flippable (Diagram SVG V2 Double)) = (MainOpts (Diagram SVG V2 Double), FlipOpts)
> --
> --  mainRender (opts, FlipOpts f) (Flippable d) = mainRender opts ((if f then reflectX else id) d)
