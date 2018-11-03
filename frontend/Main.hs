{-# LANGUAGE CPP #-}

module Main where

import Miso
import qualified Language.Javascript.JSaddle.Warp as JSaddle

#ifdef ghcjs_HOST_OS

run :: Int -> JSM () -> IO ()
run = JSaddle.run

#else

import Network.HTTP.Client hiding (Request)
import Network.HTTP.Proxy
import Network.WebSockets hiding (Request, requestPath)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

run :: Int -> JSM () -> IO ()
run port f = do
    manager <- newManager defaultManagerSettings
    let proxyApp = httpProxyApp defaultProxySettings
          { proxyPort            = 3003
          , proxyRequestModifier = rewrite
          } manager
        app req sendResp = do
          case Wai.pathInfo req of
            ("api":_) -> proxyApp req sendResp
            _         -> JSaddle.jsaddleApp req sendResp
    JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) app
      >>= Warp.runSettings settings
  where
    settings = Warp.setPort port . Warp.setTimeout 3600 $ Warp.defaultSettings
    rewrite req = return $ Right req
      { requestPath = "http://localhost:3002" <> requestPath req }

#endif

main :: IO ()
main = run 3003 $ return ()
