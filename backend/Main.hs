{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import System.Directory
import System.FilePath

import qualified Network.Wai.Handler.Warp as Warp

newtype Hello = Hello Text deriving (Generic)
instance FromJSON Hello
instance ToJSON Hello

type API
  =    "api" :> "hello" :> Get '[JSON] Hello
  :<|> "api" :> "hello" :> Capture "name" Text :> Get '[JSON] Hello
  :<|> Raw

api :: Proxy API
api = Proxy

server :: Server API
server = handleEmptyHello :<|> handleHello :<|> serveDirectoryWebApp staticPath
 where
  staticPath = ".." </> ghcjsFolder
    "dist-ghcjs" "x86_64-linux" "8.4.0.1" "0.1.0.0"

ghcjsFolder :: String -- ^ GHCJS output folder
            -> String -- ^ Instruction set/Operating system
            -> String -- ^ GHCJS version
            -> String -- ^ Project version
            -> FilePath
ghcjsFolder outputFolder os ghcjsVersion projectVersion
  =   outputFolder
  </> "build"
  </> os
  </> ("ghcjs-" ++ ghcjsVersion)
  </> ("frontend-" ++ projectVersion)
  </> "x"
  </> "frontend"
  </> "build"
  </> "frontend"
  </> "frontend"
  <.> "jsexe"

handleEmptyHello :: Handler Hello
handleEmptyHello = return $ Hello "Hello"

handleHello :: Text -> Handler Hello
handleHello = return . Hello . ("Hello " <>)

app :: Application
app = serve api server

main :: IO ()
main = do
  getCurrentDirectory >>= putStrLn
  void $ Warp.run 3002 app
