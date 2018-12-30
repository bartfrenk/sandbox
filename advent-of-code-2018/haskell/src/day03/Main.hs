import           Control.Monad.Except
import           Data.List            (foldl')
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Text.Parsec

readLines :: MonadIO m => FilePath -> m [String]
readLines path = lines <$> (liftIO $ readFile path)

data Claim = Claim !Int !Int !Int !Int !Int deriving (Eq, Show)

parseClaim :: Monad m => String -> ExceptT ParseError m Claim
parseClaim s = liftEither $ runParser claim () "" s
  where
    claim = do
      char '#'
      id <- number
      space
      char '@'
      space
      left <- number
      char ','
      top <- number
      char ':'
      space
      width <- number
      char 'x'
      height <- number
      pure $ Claim id left top width height
    number = read <$> many1 digit

readClaims :: FilePath -> ExceptT ParseError IO [Claim]
readClaims path = do
  lines <- liftIO $ readLines path
  mapM parseClaim lines

claims :: IO (Either ParseError [Claim])
claims = runExceptT (readClaims "res/fabric-claims.txt")

data Covered = Once | Overlap deriving (Eq, Show)

computeOverlap :: [Claim] -> Int
computeOverlap claims = countOverlaps (recur Map.empty claims)
  where
    recur !fabric [] = fabric
    recur !fabric (c:cs) = recur (updateWithClaim fabric c) cs
    updateWithClaim fabric (Claim _ left top width height) =
      let coords = [(x, y) | x <- [left .. left + width - 1], y <- [top .. top + height - 1]]
      in foldl' (\m p -> Map.insertWith (\_ -> const Overlap) p Once m) fabric coords
    countOverlaps fabric = length (filter (\(_, cov) -> cov == Overlap) $ Map.toList fabric)

answer3a :: IO (Either ParseError Int)
answer3a = runExceptT $ computeOverlap <$> readClaims "res/fabric-claims.txt"

overlaps :: Claim -> Claim -> Bool
overlaps (Claim _ left1 top1 width1 height1) (Claim _ left2 top2 width2 height2) =
  overlappingIntervals (left1, left1 + width1) (left2, left2 + width2) &&
  overlappingIntervals (top1, top1 + height1) (top2, top2 + height2)
  where overlappingIntervals (x, y) (u, v) = not (v <= x) && not (y <= u)

nonOverlapping :: [Claim] -> Maybe Claim
nonOverlapping all = recur all
  where
    recur [] = Nothing
    recur (c:cs) =
      let cs' = filter (overlaps c) all
      in if cs' == [c] then Just c else recur $ filter (not . overlaps c) cs

answer3b :: IO (Either ParseError (Maybe Claim))
answer3b = runExceptT $ nonOverlapping <$> readClaims "res/fabric-claims.txt"

main :: IO ()
main = do
  answer3a >>= putStrLn . ("Part A: " ++) . show
  answer3b >>= putStrLn . ("Part B: " ++) . show

