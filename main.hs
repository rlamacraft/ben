import Control.Applicative ((<*>), (<|>))
import Control.Monad (sequence, (<=<))
import Data.Bifunctor (Bifunctor, bimap, first, second)
import Data.Char (digitToInt)
import Data.Foldable (fold)
import Data.Function (flip)
import Data.Maybe (Maybe(..), fromMaybe, listToMaybe)
import Data.Monoid (All(..), Any(..), Product(..))
import Data.Text (Text, pack, splitOn, singleton, unpack)
import System.Environment
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, get, satisfy, many, choice, char, look, pfail, eof, string, skipSpaces)
import Text.Read (readMaybe)

import Utils (both, dropLast, fromEither, ifTrue, maybeToRight, splitOnLast, strongRightDistribute, tupleFromList)

data PatternPiece
  = PLiteral Bool
  | PDontCare
  | PVar Char
  deriving (Show)
  
data Expression
  = EVar Char
  | ELiteral Int
  | EAddition Expression Expression
  | EModulo Expression Expression
  | EEqual Expression Expression
  deriving (Show)

data Output
  = OExpression Expression
  | OConstant Bool
  deriving (Show)

type Pattern = [PatternPiece]

type Binding = (Pattern, [Output]) 

type Bit = Bool
type BitVector = [Bit]

bitToInt :: Bit -> Int
bitToInt False = 0
bitToInt True = 1

showBitVector :: BitVector -> String
showBitVector = fmap showBit where
  showBit :: Bit -> Char
  showBit = head . show . bitToInt

intToBitVector :: Int -> BitVector
intToBitVector 0 = [False]
intToBitVector 1 = [True]
intToBitVector x = uncurry (++) $ both intToBitVector $ quotRem x 2

bitVectorToInt :: BitVector -> Int
bitVectorToInt = bitVectorToInt' . reverse where
  bitVectorToInt' [] = 0
  bitVectorToInt' (x:xs) = bitToInt x + (2 * (bitVectorToInt' xs))

type VariableCapture = (Char, BitVector)
type VariableCaptures = [VariableCapture]

parsePatternPiece :: Char -> PatternPiece
parsePatternPiece '0' = PLiteral False
parsePatternPiece '1' = PLiteral True
parsePatternPiece '-' = PDontCare
parsePatternPiece x   = PVar x -- TODO: is '2' a valid variable?

parsePattern :: String -> Pattern
parsePattern = fmap parsePatternPiece

parseExpression :: String -> Either String Expression
parseExpression = fmap fst . maybeToRight "Parser Failed" . listToMaybe . readP_to_S rootExpressionParser where

  rootExpressionParser :: ReadP Expression
  rootExpressionParser = do
    exp <- expressionParser
    eof
    return exp

  expressionParser :: ReadP Expression
  expressionParser = do
    exp <- choice [
      parseAddition,
      parseModulo,
      parseEqual,
      parseLiteral,
      parseVariable
      ]
    return exp

  parseVariable :: ReadP Expression
  parseVariable = EVar <$> satisfy (\_ -> True)

  parseLiteral :: ReadP Expression
  parseLiteral = ELiteral <$> parseNumber where

    parseNumber :: ReadP Int
    parseNumber = foldl (\acc -> (+) (acc * 10)) 0 <$> many parseDigit

    parseDigit :: ReadP Int
    parseDigit = digitToInt <$> (satisfy $ (\char -> any (char ==) "0123456789"))

  parseAddition :: ReadP Expression
  parseAddition = parseBinaryOperation "+" EAddition

  parseModulo :: ReadP Expression
  parseModulo = parseBinaryOperation "%" EModulo

  parseEqual :: ReadP Expression
  parseEqual = parseBinaryOperation "==" EEqual

  parseBinaryOperation :: String -> (Expression -> Expression -> Expression) -> ReadP Expression
  parseBinaryOperation symbol constructor = do
    char '('
    skipSpaces
    before <- expressionParser
    skipSpaces
    string symbol
    skipSpaces
    after <- expressionParser
    skipSpaces
    char ')'
    return $ constructor before after

parseOutput :: String -> Either String [Output]
parseOutput = sequence . fmap (parseOutputPiece . unpack) . splitOn (pack "|") . pack where
  parseOutputPiece :: String -> Either String Output
  parseOutputPiece "0" = Right $ OConstant False
  parseOutputPiece "1" = Right $ OConstant True
  parseOutputPiece x   = OExpression <$> parseExpression x

parseBinding :: Text -> Either String Binding
parseBinding = (strongRightDistribute . bimap parsePattern parseOutput)
	     <=< tupleFromList
	       . fmap unpack
	       . splitOn (pack ":")

parse :: Text -> [Binding]
parse = fmap parseBindingElseError . splitOn (pack ";") where
  parseBindingElseError :: Text -> Binding
  parseBindingElseError = fromEither . first (error . ((++) "Parsing Failed: ")) . parseBinding

verify :: [Binding] -> [Binding]
verify [] = error "No bindings specified"
verify b@(x:xs) =  if (getAll $ fold $ (All . (== width) . length . fst) <$> xs) then b else error "All patterns must have the same width." where
  width = length $ fst x

pad :: Int -> BitVector -> BitVector
pad 0 _      = []
pad n []     = [False] ++ (pad (n-1) [])
pad n xs     = (pad (n-1) (init xs)) ++ [last xs]

match :: [Binding] -> BitVector -> Maybe (VariableCaptures, [Output])
match bindings n = (first $ bitVariableMap n) <$> getFirstMatchedPattern n bindings where
  
  getFirstMatchedPattern :: BitVector -> [Binding] -> Maybe Binding
  getFirstMatchedPattern  n = listToMaybe . filter (matchesPattern n . fst) where
  
    matchesPattern :: BitVector -> Pattern -> Bool
    matchesPattern bits bindingPieces = getAll $ fold $ (All . (uncurry matchesPiece)) <$> zip bits bindingPieces where
    
      matchesPiece :: Bit -> PatternPiece -> Bool
      matchesPiece False (PLiteral True) = False
      matchesPiece True (PLiteral False) = False
      matchesPiece _ _ = True

  bitVariableMap :: BitVector -> Pattern -> VariableCaptures
  bitVariableMap n p = collapse $ zip n $ (patternPieceToVariableChar <$> p) where

    patternPieceToVariableChar :: PatternPiece -> Maybe Char
    patternPieceToVariableChar (PLiteral _) = Nothing
    patternPieceToVariableChar (PDontCare) = Nothing
    patternPieceToVariableChar (PVar c) = Just c

    collapse :: [(Bit, Maybe Char)] -> VariableCaptures
    collapse = foldl collapse' []  where

      collapse' :: VariableCaptures -> (Bit, Maybe Char) -> VariableCaptures
      collapse' vc                   (_, Nothing)  = vc
      collapse' []                   (b, (Just c)) = (c, [b]):[]
      collapse' (dbv@(d, bv):xs) bjc@(b, (Just c)) = if c == d then ((d, bv ++ [b]):xs) else dbv:(collapse' xs bjc)

-- If no pattern matches a value in range, then the output is 0xFF
defaultOutput :: (VariableCaptures, [Output])
defaultOutput = ([], (replicate 8 (OConstant True)))

replicateFirst :: (a, [b]) -> [(a,b)]
replicateFirst (a, []) = []
replicateFirst (a, b:bs) = (a,b):(replicateFirst (a,bs))

run :: [Binding] -> [BitVector]
run [] = error "Can't happen"
run bindings = (fold
               . fmap (uncurry eval)
               . replicateFirst
               . fromMaybe defaultOutput
               . match bindings
               . pad width
               . intToBitVector)
              <$> [0..(exp' 2 width) - 1] where
  width = length $ fst $ head bindings

  exp' :: Int -> Int -> Int
  exp' a = getProduct . fold . flip replicate (Product a)

eval :: VariableCaptures -> Output -> BitVector
eval _  (OConstant   b)        = [b]
eval vc (OExpression (EVar x)) = findVar vc x where

  findVar :: VariableCaptures -> Char -> BitVector
  findVar []          c = error $ "Unknown variable `" ++ [c] ++ "`"
  findVar ((c,bv):xs) d = if c == d then bv else findVar xs d

eval vc (OExpression exp) = intToBitVector $ evalexp vc exp where

  evalexp :: VariableCaptures -> Expression -> Int
  evalexp _  (EVar _)              = error "Can't happen"
  evalexp vc (EAddition exp1 exp2) = recurse vc (+) exp1 exp2
  evalexp vc (EModulo exp1 exp2)   = recurse vc mod exp1 exp2
  evalexp _  (ELiteral x)          = x

  recurse :: VariableCaptures -> (Int -> Int -> Int) -> Expression -> Expression -> Int
  recurse vc f exp1 exp2 = (uncurry f) $ both (bitVectorToInt . eval vc . OExpression) (exp1, exp2)

main :: IO ()
main = do
     args <- getArgs
     bindings <- return $ verify $ parse $ pack $ head args
     putStrLn $ show $ fmap showBitVector $ run bindings
