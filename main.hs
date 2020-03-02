import Control.Monad (sequence, (<=<))
import Data.Bifunctor (Bifunctor, bimap, first, second)
import Data.Function (flip)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Text (Text, pack, splitOn, singleton, unpack)
import System.Environment

import Utils (strongRightDistribute, tupleFromList)

data PatternPiece
  = PLiteral Bool
  | PDontCare
  | PVar Char
  deriving (Show)
  
data Expression
  = EVar Char
  | EAddition Expression Expression
  | ELiteral Bool
  deriving (Show)

data Output
  = OExpression Expression
  | OConstant Bool
  deriving (Show)

type Pattern = [PatternPiece]

type Binding = (Pattern, [Output]) 

parsePatternPiece :: Char -> PatternPiece
parsePatternPiece '0' = PLiteral False
parsePatternPiece '1' = PLiteral True
parsePatternPiece '-' = PDontCare
parsePatternPiece x   = PVar x -- TODO: is '2' a valid variable?

parsePattern :: String -> Pattern
parsePattern = fmap parsePatternPiece

data ExpressionToken = TAddition | TVar Char deriving (Eq)

tokeniseExpression :: String -> Maybe [ExpressionToken]
tokeniseExpression = sequence . fmap (tokenise . unpack) . splitOn (pack " ") . pack where
  tokenise :: String -> Maybe ExpressionToken
  tokenise ('+':[]) = Just $ TAddition
  tokenise (x  :[]) = Just $ TVar x
  tokenuse _        = Nothing

constructAST :: [ExpressionToken] -> Maybe Expression
constructAST [TVar x] = Just $ EVar x
constructAST (x:(TAddition:xs)) = do
  before <- constructAST [x]
  after <- constructAST xs
  return $ EAddition before after
constructAST _ = Nothing

parseExpression :: String -> Maybe Expression
parseExpression = constructAST <=< tokeniseExpression

parseOutput :: String -> Maybe [Output]
parseOutput = sequence . fmap (parseOutputPiece . unpack) . splitOn (pack "|") . pack where
  parseOutputPiece :: String -> Maybe Output
  parseOutputPiece "0" = Just $ OConstant False
  parseOutputPiece "1" = Just $ OConstant True
  parseOutputPiece x = OExpression <$> parseExpression x

parseBinding :: Text -> Maybe Binding
parseBinding = (strongRightDistribute . bimap parsePattern parseOutput)
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
