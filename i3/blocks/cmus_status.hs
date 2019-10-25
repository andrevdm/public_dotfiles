#!/usr/bin/env stack
-- stack --resolver lts-14.7 script --package protolude,text,process,containers
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiWayIf #-}

module Main where
import Protolude
import qualified Data.Map.Strict as Map
import qualified Data.Char as Chr
import qualified Data.Text as Txt
import qualified System.IO as IO
import qualified System.Process as IO

run :: Text -> [Text] -> IO Text
run cmd args = do
  (_, Just outp, _, phandle) <- IO.createProcess
                                        (IO.proc (Txt.unpack cmd) (Txt.unpack <$> args))
                                        { IO.std_out = IO.CreatePipe}
  pure . Txt.strip . Txt.pack =<< IO.hGetContents outp


main = do
  r <- run "cmus-remote" ["-Q"]
  let props = Map.fromList . catMaybes $ getProp <$> Txt.lines r

  let song = 
       case (Map.lookup "artist" props, Map.lookup "title" props) of
         (Just a, Just t) -> a <> " - " <> t 
         (Just a, Nothing) -> a
         (Nothing, Just t) -> t
         (Nothing, Nothing) -> " - "

  let sts =
        case Map.lookup "status" props of
          Just "stopped" -> "\61517" -- 
          Just "playing" -> "\61441" -- 
          Just "paused" -> "\61516" -- 
          _ -> ""

  putText $ sts <> " " <> song


getProp s = 
  if | Txt.isInfixOf "status " s -> Just ("status", Txt.drop 7 s)
     | Txt.isInfixOf "tag artist " s -> Just ("artist", Txt.drop 11 s)
     | Txt.isInfixOf "tag title " s -> Just ("title", Txt.drop 10 s)
     | otherwise -> Nothing

