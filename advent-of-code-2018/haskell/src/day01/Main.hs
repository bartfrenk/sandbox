import           Control.Monad.Except
import           Data.Functor.Identity
import           Data.List             (foldl')
import qualified Data.Set              as Set
import           Text.Parsec

type Parser s a = Parsec s () a

type CharStream s = Stream s Identity Char

parseChanges :: Monad m => String -> ExceptT ParseError m [Int]
parseChanges s = liftEither $ runParser (change `endBy` eol) () "" s
  where
    change = do
      sign <- oneOf "-+"
      value <- read <$> many1 digit
      pure $ if sign == '-' then -value else value
    eol = do
      string "\n"

readChanges :: FilePath -> ExceptT ParseError IO [Int]
readChanges path = (liftIO $ readFile path) >>= parseChanges

findFirstDuplicate :: [Int] -> Int
findFirstDuplicate ds = recurse Set.empty 0 $ cycle ds
  where
    recurse _ _ [] = error "Should not happen"
    recurse !seen !acc (x:xs) = do
      if acc `Set.member` seen
        then acc
        else recurse (Set.insert acc seen) (acc + x) xs

answer1a :: IO (Either ParseError Int)
answer1a = runExceptT (foldl' (+) 0 <$> readChanges "res/frequency-changes.txt")

answer1b :: IO  (Either ParseError Int)
answer1b = runExceptT (findFirstDuplicate <$> readChanges "res/frequency-changes.txt")

main :: IO ()
main = do
  answer1a >>= putStrLn . ("Part A: " ++) . show
  answer1b >>= putStrLn . ("Part B: " ++) . show
