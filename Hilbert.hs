
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
 
import Diagrams.Backend.SVG.CmdLine
import Options.Applicative
import Diagrams.Backend.CmdLine
import Diagrams.Prelude
import System.Environment

data RecurseOpts = RecurseOpts Int

instance Parseable RecurseOpts where
    parser = RecurseOpts <$> Options.Applicative.option auto ( long "numReplacements" <> short 'n' <> help "Number of replacements. " ) 


hilbert 0 = mempty
hilbert n = hilbert (n-1) # rotateBy (1/4) # reflectY <> vrule 1
          <> hilbert  (n-1) <> hrule 1
          <> hilbert  (n-1) <> vrule (-1)
          <> hilbert (n-1) # rotateBy (1/4) # reflectX
 
example (RecurseOpts n) = frame 1 . lw medium 
                  . strokeT $ hilbert n

main = mainWith (example :: RecurseOpts -> Diagram B)

