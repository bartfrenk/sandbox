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

main :: IO ()
main = do
  answerA >>= putStrLn . ("Part A: " ++) . show . (\case Right x -> x)
  answerB >>= putStrLn . ("Part B: " ++) . show . (\case Right x -> x)

answerA :: IO (Either ParseError Int)
answerA = runExceptT $ do
  (samples, _) <- liftIO contents >>= parseInput
  pure $ length (listAmbiguousSamples 3 samples)

answerB :: IO (Either ParseError Integer)
answerB = runExceptT $ do
  (samples, instructions) <- liftIO contents >>= parseInput
  case findSingleEncoding samples of
    Just enc -> pure $
      let instructions' = fmap ((Map.!) enc) <$> instructions
      in (foldl' (flip exec) initial instructions') ! 0
  where
    initial = Vector.fromList [0, 0, 0, 0]

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
  { input       :: Vector Integer
  , instr       :: Instruction Int
  , output      :: Vector Integer
  } deriving (Show)

-- |Parser for numbers.
number :: Read a => Parsec String () a
number = read <$> many1 digit

-- |Parser for encoded instructions (with instruction numbers instead of actual
-- op codes).
instruction :: Parsec String () (Instruction Int)
instruction = Instruction <$> number' <*> number' <*> number' <*> number'
  where
    -- It is suprising that spaces also parses ending newlines.
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

-- |Parse the samples and instructions from string. The string should be read
-- directly from the input file.
parseInput :: Monad m => String -> ExceptT ParseError m ([Sample], [Instruction Int])
parseInput = liftEither . runParser parser () ""
  where
    parser = (,) <$> sampleList <*> instructions
    sampleList = manyTill (sample <* newline) $ void (many1 newline)
    instructions = many instruction

exec :: Instruction OpCode -> Vector Integer -> Vector Integer
exec (Instruction ADDR in1 in2 out) input =
  input // [(out, input ! in1 + input ! in2)]
exec (Instruction ADDI in1 in2 out) input =
  input // [(out, input ! in1 + toInteger in2)]
exec (Instruction MULR in1 in2 out) input =
  input // [(out, input ! in1 * input ! in2)]
exec (Instruction MULI in1 in2 out) input =
  input // [(out, input ! in1 * toInteger in2)]
exec (Instruction BANR in1 in2 out) input =
  input // [(out, input ! in1 .&. input ! in2)]
exec (Instruction BANI in1 in2 out) input =
  input // [(out, input ! in1 .&. toInteger in2)]
exec (Instruction BORR in1 in2 out) input =
  input // [(out, input ! in1 .|. input ! in2)]
exec (Instruction BORI in1 in2 out) input =
  input // [(out, input ! in1 .|. toInteger in2)]
exec (Instruction SETR in1 _ out) input =
  input // [(out, input ! in1)]
exec (Instruction SETI in1 _ out) input =
  input // [(out, toInteger in1)]
exec (Instruction GTIR in1 in2 out) input =
  input // [(out, if toInteger in1 > input ! in2 then 1 else 0)]
exec (Instruction GTRI in1 in2 out) input =
  input // [(out, if input ! in1 > toInteger in2 then 1 else 0)]
exec (Instruction GTRR in1 in2 out) input =
  input // [(out, if input ! in1 > input ! in2 then 1 else 0)]
exec (Instruction EQIR in1 in2 out) input =
  input // [(out, if toInteger in1 == input ! in2 then 1 else 0)]
exec (Instruction EQRI in1 in2 out) input =
  input // [(out, if input ! in1 == toInteger in2 then 1 else 0)]
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
-- samples, without taking into account that each instruction number needs to
-- map to a single op code.
findCoherentEncodings :: [Sample] -> Map Int [OpCode]
findCoherentEncodings = foldl' restrict Map.empty
  where
    restrict :: Map Int [OpCode] -> Sample -> Map Int [OpCode]
    restrict enc sample@(Sample _ inst _) =
      let encodings = possibleOpCodes sample
      in Map.insertWith intersect (getOpCode inst) encodings enc
    getOpCode (Instruction op _ _ _) = op

-- |Select the coherent encodings that also takes into account that different
-- instruction numbers need to map to different op codes. Return 'Nothing' if no
-- such encoding exists.
findSingleEncoding :: [Sample] -> Maybe (Map Int OpCode)
findSingleEncoding samples =
  let initial = Map.toList $ findCoherentEncodings samples
  in traverse select $ Map.fromList (recur initial)

  where
    -- If an instruction number maps to a single op code then eliminate that op
    -- code from the candidates of all other instruction numbers.
    recur :: [(Int, [OpCode])] -> [(Int, [OpCode])]
    recur enc =
      let found = ((==) 1 . length . snd) `filter` enc
          exclude = concat (snd <$> found)
      in if length found == length enc
      then found
      else recur $ flip fmap enc $
           \(n, candidates) -> if length candidates == 1
                               then (n, candidates)
                               else (n, candidates \\ exclude)
    select [e] = Just e
    select _ = Nothing

contents :: IO String
contents = readFile "res/input-16.txt"
