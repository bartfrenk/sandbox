import           Data.Map  (Map)
import           Data.Text


data Schema = Schema

data URN = URN

data ResourceRef

data ResourceSchema


-- |Local names are unique within the scope of a single task
type LocalName = Text

data Task = Task


inputs :: Task -> Map LocalName Schema
inputs = undefined

outputs :: Task -> Map LocalName Schema
outputs = undefined

data TaskArguments = Map LocalName Resource


data Resource = URL Text | Literal Text


task =
  (x, y) <- task1



main :: IO ()
main = putStrLn "Hello"


