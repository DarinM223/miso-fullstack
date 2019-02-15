module Main where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

import qualified Network.Wai.Handler.Warp as Warp

newtype Hello = Hello Text deriving (Generic)
instance FromJSON Hello
instance ToJSON Hello

type API
  =    "api" :> "hello" :> Get '[JSON] Hello
  :<|> "api" :> "hello" :> Capture "name" Text :> Get '[JSON] Hello

api :: Proxy API
api = Proxy

server :: Server API
server = handleEmptyHello :<|> handleHello

handleEmptyHello :: Handler Hello
handleEmptyHello = return $ Hello "Hello"

handleHello :: Text -> Handler Hello
handleHello = return . Hello . ("Hello " <>)

app :: Application
app = serve api server

main :: IO ()
main = void $ Warp.run 3002 app
