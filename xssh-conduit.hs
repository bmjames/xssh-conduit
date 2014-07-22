{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (foldr)

import Control.Concurrent.STM (atomically)
import Control.Monad          (void)
import Control.Monad.Trans.Resource

import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Process (sourceCmd)
import Data.Conduit.TMChan
import Data.Foldable (foldr)

import Options.Applicative

import qualified Data.Conduit.Combinators as Conduit
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BSC


type UName = String
type Cmds = String
type HostName = String
type SSHOpt = String

data Options = Opts (Maybe UName) [SSHOpt] Cmds [HostName] deriving Show

options :: Parser Options
options = Opts
  <$> optional (strOption ( long "user"
                         <> short 'u'
                         <> metavar "USERNAME" 
                         <> help "Username for ssh"))
  <*> liftA2 (++)
        (flag [] disableKeyCheck ( long "no-strict-key-check"
                                <> short 'S'
                                <> help "Disable strict host key checking"
                                ))
        (many (strOption ( long "ssh-opts"
                        <> metavar "OPTS"
                        <> help "Arbitrary options to pass through to ssh"
                        )))
  <*> argument str (metavar "COMMAND")
  <*> some (argument str $ metavar "HOSTS...")

disableKeyCheck :: [SSHOpt]
disableKeyCheck = ["-o", "StrictHostKeyChecking=no"]

data OutputLine = Ln HostName ByteString

ssh :: Maybe UName
    -> HostName
    -> Cmds
    -> [SSHOpt]
    -> Source (ResourceT IO) ByteString
ssh user host cmd opts =
  sourceCmd (unwords args)
  where
    args = ["ssh", "-T"] ++ opts ++ [foldr prependUser host user, cmd]
    prependUser u h = u ++ "@" ++ h

ssh' :: Maybe UName
     -> HostName
     -> Cmds
     -> [SSHOpt]
     -> Source (ResourceT IO) OutputLine
ssh' user host cmd opts = ssh user host cmd opts $= Conduit.map (Ln host)

dispLine :: Monad m => Conduit OutputLine m ByteString
dispLine = Conduit.map $ \(Ln h l) -> "[" <> BSC.pack h <> "] " <> l

main :: IO ()
main = do
  Opts u os c hs <- execParser $ info (helper <*> options) fullDesc
  chan <- atomically $ newTBMChan 16
  let sources = map (\h -> ssh' u h c os) hs
  runResourceT $ do
    merged <- mergeSources sources 16
    void . resourceForkIO $ merged $$ dispLine =$ sinkTBMChan chan True
    sourceTBMChan chan $$ Conduit.stdout
