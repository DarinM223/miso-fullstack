{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

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

run :: Int -> JSM () -> IO ()
run = JSaddle.run

#else

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Proxy
import Network.WebSockets (defaultConnectionOptions)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

run :: Int -> JSM () -> IO ()
run port f = do
  proxyApp <- httpProxyApp proxySettings <$> newManager defaultManagerSettings
  let app req sendResp = case Wai.pathInfo req of
        ("api":_) -> proxyApp req sendResp
        _         -> JSaddle.jsaddleApp req sendResp
  JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) app
    >>= Warp.runSettings settings
 where
  settings = Warp.setPort port . Warp.setTimeout 3600 $ Warp.defaultSettings
  proxySettings = defaultProxySettings
    { proxyPort            = 3003
    , proxyRequestModifier = rewrite
    }
  rewrite req = return $ Right req
    { requestPath = "http://localhost:3002" <> requestPath req }

#endif

data Model = Model
  { _helloName :: MisoString
  , _helloText :: MisoString
  } deriving (Show, Eq)

data Action = UpdateName MisoString
            | FetchHello
            | SetHello MisoString
            | NoOp
            deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m           = noEff m
updateModel (UpdateName s) m = noEff m { _helloName = s }
updateModel (SetHello s) m   = noEff m { _helloText = s }
updateModel FetchHello m     = m <#
  (fmap (SetHello . ms) . getHello . fromMisoString $ _helloName m)

viewModel :: Model -> View Action
viewModel model = div_ []
  [ input_ [ onChange UpdateName ]
  , button_ [ onClick FetchHello ] [ text "Fetch Hello" ]
  , text (_helloText model)
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
main = run 3003 $ startApp App
  { model         = Model { _helloName = "", _helloText = "" }
  , initialAction = NoOp
  , update        = updateModel
  , view          = viewModel
  , mountPoint    = Nothing
  , events        = defaultEvents
  , subs          = []
  }
