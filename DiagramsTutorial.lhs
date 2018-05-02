> {-# LANGUAGE FlexibleInstances         #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE TypeFamilies              #-}
> {-# LANGUAGE FlexibleContexts          #-}
>
> import           Diagrams.Backend.CmdLine
> import           Diagrams.Backend.SVG.CmdLine
> import           Diagrams.Prelude             hiding ((<>))
> import           Options.Applicative

> newtype Flippable a = Flippable a

> data FlipOpts = FlipOpts Bool
>
> instance Parseable FlipOpts where
>   parser = FlipOpts <$> switch (long "flipped" <> help "Flip the diagram L-R")

> instance Mainable (Flippable (Diagram SVG)) where
>   type MainOpts (Flippable (Diagram SVG)) = (MainOpts (Diagram SVG), FlipOpts)
>
>   mainRender (opts, FlipOpts f) (Flippable d) = mainRender opts ((if f then reflectX else id) d)

> d :: Diagram SVG 
> d = square 1 # fc red ||| square 1 # fc blue
>
> main = mainWith (Flippable d)