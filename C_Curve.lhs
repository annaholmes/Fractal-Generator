> {-# LANGUAGE TypeFamilies              #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts          #-}
> {-# LANGUAGE FlexibleInstances         #-}

> 
> import Diagrams.Backend.SVG.CmdLine
> import Options.Applicative
> import Diagrams.Backend.CmdLine
> import Diagrams.Prelude
> import Diagrams.TwoD.Layout.Grid
> import System.Environment




          

>
> nextFract trail = ((trail # rotateBy (-1/4) 
>                     <> trail 
>                     <> trail 
>                     <> trail # rotateBy (1/4))
>                     # scale .5)
> curves = map (trailLike . (`at` origin)) (iterate nextFract initialTrail)
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

> data RecurseOpts = RecurseOpts Int

> instance Parseable RecurseOpts where
>    parser = RecurseOpts <$> Options.Applicative.option auto ( long "numReplacements" <> short 'n' <> help "Number of replacements. " ) 


> example (RecurseOpts n) = curves
> --                        # withPrevious 
>                        # take n
>                        # map (frame 0.1) 
>                        # sameBoundingRect
> --                       # rememberOrder 
>                        # lw medium
> --                       # showOrder
> 
> main = mainWith (example :: RecurseOpts -> Diagram B)   --do x <- getArgs
> --          putStr (head x)
> --          mainWith (example (read (head (reverse x))::Int) :: Diagram B)