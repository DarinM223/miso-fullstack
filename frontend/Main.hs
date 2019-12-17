{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main (main) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHCJS.DOM.Types (liftJSM)
import Miso
import Miso.String (MisoString, fromMisoString, ms)
import qualified GHCJS.DOM.EventM as Event
import qualified GHCJS.DOM.XMLHttpRequest as Http
import qualified Language.Javascript.JSaddle.Warp as JSaddle

#ifdef ghcjs_HOST_OS

run :: ServerPort -> ProxyPort -> JSM () -> IO ()
run _ (ProxyPort port) f = JSaddle.run port f

#else

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Proxy
import Network.WebSockets (defaultConnectionOptions)
import qualified Data.ByteString.Char8 as C
import qualified Network.Wai.Handler.Warp as Warp

serverPath :: ServerPort -> C.ByteString
serverPath (ServerPort port) = C.pack $ show port

run :: ServerPort -> ProxyPort -> JSM () -> IO ()
run serverPort (ProxyPort port) f = do
  proxyApp <- httpProxyApp proxySettings <$> newManager defaultManagerSettings
  JSaddle.jsaddleWithAppOr defaultConnectionOptions (f >> syncPoint) proxyApp
    >>= Warp.runSettings settings
 where
  settings = Warp.setPort port $ Warp.setTimeout 3600 Warp.defaultSettings
  proxySettings = defaultProxySettings
    { proxyPort = port, proxyRequestModifier = rewrite }
  path req = "http://localhost:" <> serverPath serverPort <> requestPath req
  rewrite req = return $ Right req { requestPath = path req }

#endif

newtype ServerPort = ServerPort Int
newtype ProxyPort = ProxyPort Int

data Model = Model
  { helloName :: MisoString
  , helloText :: MisoString
  } deriving (Show, Eq)

data Action = UpdateName MisoString
            | FetchHello
            | SetHello MisoString
            | NoOp
            deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m           = noEff m
updateModel (UpdateName s) m = noEff m { helloName = s }
updateModel (SetHello s) m   = noEff m { helloText = s }
updateModel FetchHello m     = m <#
  (fmap (SetHello . ms) . getHello . fromMisoString $ helloName m)

viewModel :: Model -> View Action
viewModel m = div_ []
  [ input_ [ onChange UpdateName ]
  , button_ [ onClick FetchHello ] [ text "Fetch Hello" ]
  , text (helloText m)
  ]

getHello :: Text -> JSM MisoString
getHello name = do
  var <- liftIO newEmptyMVar
  xhttp <- Http.newXMLHttpRequest
  rec freeCallback <- Event.on xhttp Http.readyStateChange $ liftJSM $ do
        readyState <- Http.getReadyState xhttp
        when (readyState == 4) $ do
          resp <- fromJust <$> Http.getResponseText xhttp
          liftIO $ putMVar var resp
          freeCallback
  Http.open xhttp ("GET" :: Text)
                  ("api/hello/" <> name)
                  True
                  (Nothing :: Maybe Text)
                  (Nothing :: Maybe Text)
  Http.send xhttp
  liftIO $ takeMVar var

main :: IO ()
main = run (ServerPort 3002) (ProxyPort 3003) $ startApp App
  { model         = Model { helloName = "", helloText = "" }
  , initialAction = NoOp
  , update        = updateModel
  , view          = viewModel
  , mountPoint    = Nothing
  , events        = defaultEvents
  , subs          = []
  }
