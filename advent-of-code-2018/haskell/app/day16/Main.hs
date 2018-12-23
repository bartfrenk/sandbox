import           Control.Monad.Except
import           Data.Bits
import           Data.Functor.Identity
import           Data.List
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Vector           (Vector, (!), (//))
import qualified Data.Vector           as Vector
import           Prelude               hiding (readFile)
import           System.IO.Strict      (readFile)
import           Text.Parsec

data OpCode
  = ADDR
  | ADDI
  | MULR
  | MULI
  | BANR
  | BANI
  | BORR
  | BORI
  | SETR
  | SETI
  | GTIR
  | GTRI
  | GTRR
  | EQIR
  | EQRI
  | EQRR
  deriving (Bounded, Enum, Eq, Show)

opCodes :: [OpCode]
opCodes = [minBound .. maxBound]

data Instruction a = Instruction a Int Int Int deriving (Show, Functor)

setOpCode :: b -> Instruction a -> Instruction b
setOpCode b = fmap (\_ -> b)

data Sample = Sample
  { input       :: Vector Int
  , instr :: Instruction Int
  , output      :: Vector Int
  } deriving (Show)

-- |Parser for numbers.
number :: Parsec String () Int
number = read <$> many1 digit

-- |Parser for encoded instructions (with instruction numbers instead of actual
-- op codes).
instruction :: Parsec String () (Instruction Int)
instruction = Instruction <$> number' <*> number' <*> number' <*> number'
  where
    -- TODO: probably less surprising if we don't parse ending newlines
    number' = number <* spaces

-- |Parser for a single sample.
sample :: Parsec String () Sample
sample = Sample <$> before <*> instruction <*> after
  where
    vector = Vector.fromList <$> (wrapSquareBrackets $ number `sepBy` string ", ")
    before = (string' "Before:" *> vector) <* newline
    after = (string' "After:" *> vector) <* newline
    string' s = string s <* spaces
    wrapSquareBrackets p = do char '['; x <- p; char ']'; pure x

-- |Parse a single sample from the string.
parseSample :: Monad m => String -> ExceptT ParseError m Sample
parseSample = liftEither . runParser sample () ""

-- |Parse multiple samples, as given in the input file. Stop when hitting
-- multiple newlines.
parseSampleList :: Monad m => String -> ExceptT ParseError m [Sample]
parseSampleList = liftEither . runParser sampleList () ""
  where
    sampleList = manyTill (sample <* newline) $ do _ <- newline; _ <- newline; pure ()

parseInput :: Monad m => String -> ExceptT ParseError m ([Sample], [Instruction Int])
parseInput = liftEither . runParser parser () ""
  where
    parser = (,) <$> sampleList <*> instructions
    sampleList = manyTill (sample <* newline) $ void (many1 newline)
    instructions = many instruction

exec :: Instruction OpCode -> Vector Int -> Vector Int
exec (Instruction ADDR in1 in2 out) input =
  input // [(out, input ! in1 + input ! in2)]
exec (Instruction ADDI in1 in2 out) input =
  input // [(out, input ! in1 + in2)]
exec (Instruction MULR in1 in2 out) input =
  input // [(out, input ! in1 * input ! in2)]
exec (Instruction MULI in1 in2 out) input =
  input // [(out, input ! in1 * in2)]
exec (Instruction BANR in1 in2 out) input =
  input // [(out, input ! in1 .&. input ! in2)]
exec (Instruction BANI in1 in2 out) input =
  input // [(out, input ! in1 .&. in2)]
exec (Instruction BORR in1 in2 out) input =
  input // [(out, input ! in1 .|. input ! in2)]
exec (Instruction BORI in1 in2 out) input =
  input // [(out, input ! in1 .|. in2)]
exec (Instruction SETR in1 _ out) input =
  input // [(out, input ! in1)]
exec (Instruction SETI in1 _ out) input =
  input // [(out, in1)]
exec (Instruction GTIR in1 in2 out) input =
  input // [(out, if in1 > input ! in2 then 1 else 0)]
exec (Instruction GTRI in1 in2 out) input =
  input // [(out, if input ! in1 > in2 then 1 else 0)]
exec (Instruction GTRR in1 in2 out) input =
  input // [(out, if input ! in1 > input ! in2 then 1 else 0)]
exec (Instruction EQIR in1 in2 out) input =
  input // [(out, if in1 == input ! in2 then 1 else 0)]
exec (Instruction EQRI in1 in2 out) input =
  input // [(out, if input ! in1 == in2 then 1 else 0)]
exec (Instruction EQRR in1 in2 out) input =
  input // [(out, if input ! in1 == input ! in2 then 1 else 0)]

-- |List the op codes that could have resulted in the sample.
possibleOpCodes :: Sample -> [OpCode]
possibleOpCodes (Sample input instr output) = checkOpCode `filter` opCodes
  where
    checkOpCode op = output == exec (setOpCode op instr) input

-- |Select the samples that may be the result of at least 'n' different op
-- codes.
listAmbiguousSamples :: Int -> [Sample] -> [Sample]
listAmbiguousSamples n samples = isAmbiguous `filter` samples
  where
    isAmbiguous s = length (possibleOpCodes s) >= n

-- |Find the possible encodings from number to actual op codes, that match the
-- samples.
findPossibleEncodings :: [Sample] -> Map Int [OpCode]
findPossibleEncodings = foldl' restrict Map.empty
  where
    restrict :: Map Int [OpCode] -> Sample -> Map Int [OpCode]
    restrict enc sample@(Sample _ inst _) =
      let encodings = possibleOpCodes sample
      in Map.insertWith intersect (getOpCode inst) encodings enc
    getOpCode (Instruction op _ _ _) = op

-- |Select the first of possible encodings. Return Nothing if there is at least
-- one instruction number that does not have a an op code that matches with the
-- sample.
selectPossibleEncoding :: [Sample] -> Maybe (Map Int OpCode)
selectPossibleEncoding samples =
  let possibleEncodings = findPossibleEncodings samples
  in flip traverse possibleEncodings $ \case
    [] -> Nothing
    (op:_) -> Just op

main :: IO ()
main = putStrLn "Day 16"

contents :: IO String
contents = readFile "res/input-16.txt"


x :: IO (Either ParseError Sample)
x = runExceptT $ parseSample sampleString1

y :: IO (Either ParseError [OpCode])
y = runExceptT $ possibleOpCodes <$> parseSample sampleString2

z :: IO (Maybe (Map Int OpCode))
z = (runExceptT $ liftIO contents >>= parseSampleList) >>= \case
    Right samples -> pure $ selectPossibleEncoding samples

u :: IO (Either ParseError [Instruction Int])
u = (runExceptT $ snd <$> (liftIO contents >>= parseInput))

answerA :: IO Int
answerA = (runExceptT $ liftIO contents >>= parseSampleList) >>= \case
    Right samples -> pure $ length (listAmbiguousSamples 3 samples)

sampleString1 = unlines
  [ "Before: [1, 1, 1, 0]"
  , "4 1 0 0"
  , "After:  [1, 1, 1, 0]"
  ]

sampleString2 = unlines
  [ "Before: [3, 2, 1, 1]"
  , "9 2 1 2"
  , "After:  [3, 2, 2, 1]"
  ]
