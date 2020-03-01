import Control.Monad (sequence, (<=<))
import Data.Bifunctor (Bifunctor, bimap, first, second)
import Data.Function (flip)
import Data.List (uncons)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Text (Text, pack, splitOn, singleton, unpack)
import System.Environment

data PatternPiece
  = Literal Bool
  | DontCare
  | PPVar Char
  deriving (Show)
data Expression
  = EVar Char
  | Addition Expression Expression
  deriving (Show)
type Pattern = [PatternPiece] 
type Binding = (Pattern, Expression) 

strongRightDist :: Monad f => (a, f b) -> f (a , b)
strongRightDist (x,y) = do
  y' <- y
  return (x, y')

parsePatternPiece :: Char -> PatternPiece
parsePatternPiece '0' = Literal False
parsePatternPiece '1' = Literal True
parsePatternPiece '-' = DontCare
parsePatternPiece x   = PPVar x -- TODO: is '2' a valid variable?

parsePattern :: String -> Pattern
parsePattern = fmap parsePatternPiece

parseExpression :: String -> Maybe Expression
parseExpression = const $ Just $ EVar 'x'

tupleFromList :: [a] -> Maybe (a, a)
tupleFromList xs = do
  (x, ys) <- uncons xs
  (y, _ ) <- uncons ys
  return (x, y)

parseBinding :: Text -> Maybe Binding
parseBinding = (strongRightDist . bimap parsePattern parseExpression)
	     <=< tupleFromList
	       . fmap unpack
	       . splitOn (pack ":")

parse :: Text -> Maybe [Binding]
parse = sequence . fmap parseBinding . splitOn (pack ";")

main :: IO ()
main = do
     args <- getArgs
     putStrLn
	 $ fromMaybe "Parsing Failed"     
	 $ show	
	<$> (parse
	 $ pack
	 $ head args)
