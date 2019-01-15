module Scratch where

import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Network.HTTP.Conduit
import           Network.HTTP.Simple  (httpSink)
import           Text.HTML.DOM        (sinkDoc)
import           Text.XML.Cursor      (attributeIs, content, element,
                                       fromDocument, ($//), (&/), (&//))



simple :: IO ()
simple = do
  doc <- httpSink "https://www.wowbagger.com/process.php" $ const sinkDoc
  let cursor = fromDocument doc
  mapM_ (T.putStrLn . T.pack . show)
    $ cursor
    $// attributeIs "class" "customBig"
    &/ content
