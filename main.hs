import Control.Applicative ((<*>))
import Control.Monad (sequence, (<=<))
import Data.Bifunctor (Bifunctor, bimap, first, second)
import Data.Foldable (fold)
import Data.Function (flip)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (All(..), Any(..), Product(..))
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
  tokenise _        = Nothing

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

parse :: Text -> [Binding]
parse = fmap (fromMaybe (error "Parsing Failed") . parseBinding) . splitOn (pack ";")

verify :: [Binding] -> [Binding]
verify [] = error "No bindings specified"
verify b@(x:xs) =  if (getAll $ fold $ (All . (== width) . length . fst) <$> xs) then b else error "All patterns must have the same width." where
  width = length $ fst x

exp' :: Int -> Int -> Int
exp' a = getProduct . fold . replicate a . Product

type Bit = Bool
type BitVector = [Bit]

intToBitVector :: Int -> BitVector
intToBitVector 0 = [False]
intToBitVector 1 = [True]
intToBitVector x = uncurry (++) $ bimap intToBitVector intToBitVector $ quotRem x 2

pad :: Int -> BitVector -> BitVector
pad 0 _      = []
pad n []     = [False] ++ (pad (n-1) [])
pad n xs     = (pad (n-1) (init xs)) ++ [last xs]

match :: [Binding] -> BitVector -> Bool
match bindings n = getAny $ fold $ (Any . matchesPattern n . fst) <$> bindings where
  matchesPattern :: BitVector -> Pattern -> Bool
  matchesPattern bits bindingPieces = getAll $ fold $ (All . (uncurry matchesPiece)) <$> zip bits bindingPieces where
    matchesPiece :: Bit -> PatternPiece -> Bool
    matchesPiece False (PLiteral True) = False
    matchesPiece True (PLiteral False) = False
    matchesPiece _ _ = True

-- for now, we're just going to check which patterns have a match (those that don't will default to 0xFF)
run :: [Binding] -> [Bool]
run [] = error "Can't happen"
run bindings = (match bindings . pad width . intToBitVector) <$> [0..(exp' 2 width) - 1] where
  width = length $ fst $ head bindings

main :: IO ()
main = do
     args <- getArgs
     bindings <- return $ verify $ parse $ pack $ head args
     putStrLn $ show $ run bindings
