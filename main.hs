import           Control.Applicative ((<*>), (<|>))
import           Control.Monad (sequence, (<=<))
import           Data.Bifunctor (Bifunctor, bimap, first, second)
import           Data.Char (digitToInt)
import           Data.Foldable (fold)
import           Data.Function (flip)
import           Data.Maybe (Maybe(..), fromMaybe, listToMaybe)
import           Data.Monoid (All(..), Any(..), Product(..))
import           Data.Text (Text, pack, splitOn, unpack)
import           System.Environment
import qualified Text.ParserCombinators.ReadP as ReadP
import           Text.Read (readMaybe)
import qualified Utils

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
intToBitVector x = uncurry (++) $ Utils.both intToBitVector $ quotRem x 2

bitVectorToInt :: BitVector -> Int
bitVectorToInt = bitVectorToInt' . reverse where
  bitVectorToInt' [] = 0
  bitVectorToInt' (x:xs) = bitToInt x + (2 * (bitVectorToInt' xs))

type VariableCapture = (Char, BitVector)
type VariableCaptures = [VariableCapture]

parsePatternPiece :: Char -> Either String PatternPiece
parsePatternPiece '0' = Right $ PLiteral False
parsePatternPiece '1' = Right $ PLiteral True
parsePatternPiece '-' = Right $ PDontCare
parsePatternPiece x   = if Utils.isLowerCaseLetter x then Right (PVar x) else Left ("'" ++ [x] ++ "' is not a single character from the set of lowercase letters")

parsePattern :: String -> Either String Pattern
parsePattern = sequence . fmap parsePatternPiece

parseExpression :: String -> Either String Expression
parseExpression = fmap fst . Utils.maybeToRight "Parser Failed" . listToMaybe . ReadP.readP_to_S rootExpressionParser where

  rootExpressionParser :: ReadP.ReadP Expression
  rootExpressionParser = do
    exp <- expressionParser
    ReadP.eof
    return exp

  expressionParser :: ReadP.ReadP Expression
  expressionParser = ReadP.choice [
      parseBinaryOperation "+" EAddition,
      parseBinaryOperation "%" EModulo,
      parseBinaryOperation "==" EEqual,
      ELiteral <$> parseNumber,
      EVar <$> parseAlphaChar
      ] where

    parseNumber :: ReadP.ReadP Int
    parseNumber = foldl (\acc -> (+) (acc * 10)) 0 <$> ReadP.many parseDigit where

      parseDigit :: ReadP.ReadP Int
      parseDigit = digitToInt <$> (ReadP.satisfy $ (\char -> any (char ==) "0123456789"))

    parseAlphaChar :: ReadP.ReadP Char
    parseAlphaChar = ReadP.satisfy $ Utils.isLowerCaseLetter

    parseBinaryOperation :: String -> (Expression -> Expression -> Expression) -> ReadP.ReadP Expression
    parseBinaryOperation symbol constructor = do
      ReadP.char '('
      ReadP.skipSpaces
      before <- expressionParser
      ReadP.skipSpaces
      ReadP.string symbol
      ReadP.skipSpaces
      after <- expressionParser
      ReadP.skipSpaces
      ReadP.char ')'
      return $ constructor before after
  
parseOutput :: String -> Either String [Output]
parseOutput = sequence . fmap (parseOutputPiece . unpack) . splitOn (pack "|") . pack where
  parseOutputPiece :: String -> Either String Output
  parseOutputPiece "0" = Right $ OConstant False
  parseOutputPiece "1" = Right $ OConstant True
  parseOutputPiece x   = OExpression <$> parseExpression x

parseBinding :: Text -> Either String Binding
parseBinding = (Utils.sequencePair . bimap parsePattern parseOutput)
	     <=< Utils.tupleFromList
	       . fmap unpack
	       . splitOn (pack ":")

parse :: Text -> [Binding]
parse = fmap parseBindingElseError . splitOn (pack ";") where
  parseBindingElseError :: Text -> Binding
  parseBindingElseError = Utils.fromEither . first (error . ((++) "Parsing Failed: ")) . parseBinding

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

run :: [Binding] -> [BitVector]
run [] = error "Can't happen"
run bindings = (fold
               . fmap (uncurry eval)
               . Utils.replicateFirst
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
  evalexp vc (EEqual exp1 exp2)    = recurse vc (\x -> bitVectorToInt . pure . (== x)) exp1 exp2
  evalexp _  (ELiteral x)          = x

  recurse :: VariableCaptures -> (Int -> Int -> Int) -> Expression -> Expression -> Int
  recurse vc f exp1 exp2 = (uncurry f) $ Utils.both (bitVectorToInt . eval vc . OExpression) (exp1, exp2)

main :: IO ()
main = do
     args <- getArgs
     bindings <- return $ verify $ parse $ pack $ head args
     putStrLn $ show $ fmap showBitVector $ run bindings
