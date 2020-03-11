import Control.Applicative ((<*>))
import Control.Monad (sequence, (<=<))
import Data.Bifunctor (Bifunctor, bimap, first, second)
import Data.Foldable (fold)
import Data.Function (flip)
import Data.Maybe (Maybe(..), fromMaybe, listToMaybe)
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

type Bit = Bool
type BitVector = [Bit]

type VariableCapture = (Char, BitVector)
type VariableCaptures = [VariableCapture]

parsePatternPiece :: Char -> PatternPiece
parsePatternPiece '0' = PLiteral False
parsePatternPiece '1' = PLiteral True
parsePatternPiece '-' = PDontCare
parsePatternPiece x   = PVar x -- TODO: is '2' a valid variable?

parsePattern :: String -> Pattern
parsePattern = fmap parsePatternPiece

data ExpressionToken = TAddition | TVar Char | TLiteral Bool deriving (Eq)

tokeniseExpression :: String -> Maybe [ExpressionToken]
tokeniseExpression = sequence . fmap (tokenise . unpack) . splitOn (pack " ") . pack where
  tokenise :: String -> Maybe ExpressionToken
  tokenise ('+':[]) = Just $ TAddition
  tokenise ('0':[]) = Just $ TLiteral False
  tokenise ('1':[]) = Just $ TLiteral True
  tokenise (x  :[]) = Just $ TVar x
  tokenise _        = Nothing

constructAST :: [ExpressionToken] -> Maybe Expression
constructAST [TVar x] = Just $ EVar x
constructAST (x:(TAddition:xs)) = do -- note: associates to the right
  before <- constructAST [x]
  after <- constructAST xs
  return $ EAddition before after
constructAST ((TLiteral b):[]) = Just $ ELiteral b
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

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f

intToBitVector :: Int -> BitVector
intToBitVector 0 = [False]
intToBitVector 1 = [True]
intToBitVector x = uncurry (++) $ both intToBitVector $ quotRem x 2

bitVectorToInt :: BitVector -> Int
bitVectorToInt [] = 0
bitVectorToInt [False] = 0
bitVectorToInt [True] = 1
bitVectorToInt (x:xs) = (bitVectorToInt [x]) + (2 * bitVectorToInt xs)

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
  bitVariableMap n p = collapse $ zip n $ fmap patternPieceToVariableChar p where

    patternPieceToVariableChar :: PatternPiece -> Maybe Char
    patternPieceToVariableChar (PLiteral _) = Nothing
    patternPieceToVariableChar (PDontCare) = Nothing
    patternPieceToVariableChar (PVar c) = Just c

    collapse :: [(Bit, Maybe Char)] -> VariableCaptures
    collapse = foldl collapse' []  where

      collapse' :: VariableCaptures -> (Bit, Maybe Char) -> VariableCaptures
      collapse' vc                   (_, Nothing)  = vc
      collapse' []                   (b, (Just c)) = (c, [b]):[]
      collapse' (dbv@(d, bv):xs) bjc@(b, (Just c)) = if c == d then ((d, b:bv):xs) else dbv:(collapse' xs bjc)

-- If no pattern matches a value in range, then the output is 0xFF
defaultOutput :: (VariableCaptures, [Output])
defaultOutput = ([], (replicate 8 (OConstant True)))

replicateFirst :: (a, [b]) -> [(a,b)]
replicateFirst (a, []) = []
replicateFirst (a, b:bs) = (a,b):(replicateFirst (a,bs))

run :: [Binding] -> [BitVector]
run [] = error "Can't happen"
run bindings = (fold . fmap (uncurry eval) . replicateFirst . fromMaybe defaultOutput . match bindings . pad width . intToBitVector)
     <$> [0..(exp' 2 width) - 1] where
  width = length $ fst $ head bindings

eval :: VariableCaptures -> Output -> BitVector
eval _  (OConstant b) = [b]
eval vc (OExpression (EVar x)) = findVar vc x where

  findVar :: VariableCaptures -> Char -> BitVector
  findVar [] c = error $ "Unknown variable `" ++ [c] ++ "`"
  findVar ((c,bv):xs) d = if c == d then bv else findVar xs d

eval vc (OExpression exp) = intToBitVector $ evalexp vc exp where

  evalexp :: VariableCaptures -> Expression -> Int
  evalexp _  (EVar x) = error "Can't happen"
  evalexp vc (EAddition exp1 exp2) = recurse (+) exp1 exp2
  evalexp _  (ELiteral False) = 0
  evalexp _  (ELiteral True) = 1

  recurse :: (Int -> Int -> Int) -> Expression -> Expression -> Int
  recurse f exp1 exp2 = (uncurry f) $ both (bitVectorToInt . eval vc . OExpression) (exp1, exp2)


main :: IO ()
main = do
     args <- getArgs
     bindings <- return $ verify $ parse $ pack $ head args
     putStrLn $ show $ run bindings
