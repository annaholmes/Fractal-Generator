
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances         #-}
 
import Diagrams.Backend.SVG.CmdLine
import Options.Applicative hiding ((<|>))
import Diagrams.Backend.CmdLine
import Diagrams.Prelude
import System.Environment
import Data.Char
import Control.Monad
import Data.Either
import Data.Maybe
import AParser
import System.IO
import Diagrams.TwoD.Layout.Grid

data FractOpts = FractOpts
   { 
    optRecurse :: Int,
    allOrOne :: Bool,
    instructions :: String
   }

instance Parseable FractOpts where
    parser = fractOpts

fractOpts :: Options.Applicative.Parser FractOpts 
fractOpts = FractOpts <$> recurse <*> howMany <*> instruct

howMany :: Options.Applicative.Parser Bool
howMany = switch ( long "showAll" <> short 'a' <> help "Show all fractal steps rather than only last iteration. " )

recurse :: Options.Applicative.Parser Int 
recurse = Options.Applicative.option auto ( long "numReplacements" <> short 'n' <> help "Number of replacements. " ) 

fileName :: Options.Applicative.Parser String 
fileName = strOption ( long "filename" <> short 'f' <> help "Filename with instructions. " ) 

instruct :: Options.Applicative.Parser String
instruct = strOption ( long "instructions" <> short 'x' <> help "Instructions for constructing fractal." ) 

cCurve curves = scale 0.5 (rotate (-90 @@ deg) 
                            curves 
                            <> curves 
                            <> rotate (90 @@ deg) curves)

cCurve2 curves =  (curves # rotate ((-90) @@ deg) 
                                <> curves 
                                <> curves
                                <> curves # rotate (90 @@ deg)) # scale 0.5
-- )  (curveTest curves
--              <> curves 
--              <> curves 
--              <> curves # rotateBy (1/4)) # scale (1/2) 

dragon trail = (trail # rotate (-45.0 @@ deg)
               <> trail # rotate (225.0 @@ deg)
               # reverseTrail)
               # scale (1/1.41421356237)

curveTest curves = curves # rotateBy (-1/4)
        
koch trail = (trail 
                     <> trail # rotateBy (1/6) 
                     <> trail # rotateBy (-1/6) 
                     <> trail)
                     # scale (1/3)

genFract expr trail = case expr of 
                        ((Rotate x) : []) -> trail # rotate (x @@ deg) 
                        ((Scale x) : []) -> trail # scale x
                        (Reverse : []) -> trail # reverseTrail
                        (Continue : []) -> trail
                        ((Rotate x) : xs) -> trail # rotate (x @@ deg) <> genFract xs trail 
                        ((Scale x) : xs) -> (genFract xs trail) # scale x
                        (Reverse : xs) -> (genFract xs trail) # reverseTrail
                        (Continue : xs) -> trail <> (genFract xs trail)


fractal instructions = map (trailLike . (`at` origin)) (iterate (genFract instructions) initialTrail)
   where
     initialTrail = hrule 1


data Expr where 
    Rotate  :: Double -> Expr
    Scale   :: Double -> Expr 
    Continue :: Expr 
    Reverse :: Expr 
    deriving Show 

toEvalList :: String -> [Expr]
toEvalList [] = []
toEvalList s = case runParser parse s of 
                Nothing -> []
                Just (x,s2) -> x : toEvalList s2


parseRotate :: AParser.Parser Expr 
parseRotate = string "rotate" *> spaces *> (Rotate <$> parseNum) <* spaces

parseScale :: AParser.Parser Expr
parseScale = string "scale" *> spaces *> (Scale <$> parseNum) <* spaces

parseReverse :: AParser.Parser Expr
parseReverse = string "reverse" *> spaces *> pure Reverse <* spaces 

parseContinue :: AParser.Parser Expr
parseContinue = string "continue" *> spaces *> pure Continue <* spaces 

parse :: AParser.Parser Expr
parse = parseRotate <|> parseScale <|> parseReverse <|> parseContinue

test :: String -> Maybe Double
test a = case runParser parseScale a of 
    Nothing -> Nothing
    (Just (Scale x, s)) -> Just x

fract (FractOpts n showAll instructs)= case showAll of 
                                True -> (fractal (toEvalList instructs)
                                        # take n
                                        # sameBoundingRect
                                        # gridSnake
                                        # lw ultraThin) # pad 1.5
                                False -> head (reverse (fractal (toEvalList instructs)
                                        # take n 
                                        # lw medium)) # pad 1.5

-- d :: FilePath -> FractOpts -> IO (Diagram B)
-- d file opts = do
--     f <- handleFile file  
--     fract opts f       

-- handleFile :: String -> IO String 
-- handleFile f = do
--      withFile f ReadMode (\handle -> do 
--          contents <- hGetContents handle    
--          return contents)

main = mainWith (fract :: FractOpts -> Diagram B)

