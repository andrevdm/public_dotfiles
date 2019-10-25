#!/usr/bin/env stack
-- stack --resolver lts-14.7 script --package turtle --package protolude,text,process,transformers,mtl
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Protolude
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.Char as Chr
import qualified Data.Text as Txt
import qualified System.IO as IO
import qualified System.Process as IO
import qualified System.Environment as Env

run :: Text -> [Text] -> IO Text
run cmd args = do
  (_, Just outp, _, phandle) <- IO.createProcess
                                        (IO.proc (Txt.unpack cmd) (Txt.unpack <$> args))
                                        { IO.std_out = IO.CreatePipe}
  pure . Txt.strip . Txt.pack =<< IO.hGetContents outp


main = do
  a <- getActiveAP
  case a of
    Just active -> do
      q <- runMaybeT getQuality
      let (quality, clr) = case q of
                             Just qual -> do
                               let c = case qual of
                                         x | x < 40 -> "#FF0000"
                                         x | x < 60 -> "#FFF600"
                                         x | x < 80 -> "#FFAE00"
                                         otherwise -> "#00FF00"
                               (show qual, c)
                             Nothing -> do
                               ("?", "#00FF00")
  
      let sts = active <> " " <> quality <> "%"
      putText sts
      putText sts
      putText clr
      
    Nothing -> do
      -- No wifi connected show nothing
      putText "-"


getActiveAP :: IO (Maybe Text)
getActiveAP = do
  ns <- Txt.lines <$> run "netctl-auto" ["list"]
  let sel = filter (\s -> Txt.take 1 s == "*") ns
  case take 1 sel of
    [s] -> pure . Just . Txt.strip . Txt.drop 1 $ s
    _ -> pure Nothing
  
getQuality :: MaybeT IO Int
getQuality = do
  --Get interface passed as env variable from config or wpl8s0 as default
  ifaceM <- liftIO $ Env.lookupEnv "BLOCK_INSTANCE"
  let iface = maybe "wlp8s0" Txt.pack ifaceM

  wireless <- liftIO $ readFile "/proc/net/wireless"
  let ls = Txt.lines wireless
  l <- get1 . filter (Txt.isInfixOf iface) $ ls
  let is = filter (not . Txt.null) $ Txt.split Chr.isSpace l
  valTxt <- Txt.takeWhile Chr.isDigit <$> (get1 . drop 2 $ is)
  case (readMaybe (Txt.unpack valTxt) :: Maybe Int) of
    Just v ->
      pure $ v * 100 `div` 70
    Nothing ->
      mzero

  where
    get1 as = MaybeT . pure $ headMay as
