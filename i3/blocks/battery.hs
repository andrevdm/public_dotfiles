#!/usr/bin/env stack
-- stack --resolver lts-14.7 script --package turtle --package protolude,text,process
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Protolude
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
  bat <- run "acpi" ["-b"]
  --putText . show $ bat
  let (_, as) = Txt.drop 2 <$> Txt.breakOn ":" bat
  let bs = Txt.strip <$> Txt.splitOn "," as

  case take 1 bs of
    ["Full"] -> do
      let sts = "\xf1e6 full" 
      putText sts
      putText sts
      putText "#00FF00"
    ["Discharging"] -> do
      let [perTxt] = take 1 . drop 1 $ bs 
      let [tillEmpty] = Txt.takeWhile (/= ' ') . Txt.strip <$> (take 1 . drop 2 $ bs)

      let per = (readMaybe . Txt.unpack $ Txt.takeWhile Chr.isDigit perTxt) :: Maybe Int
      let (clr, ico) = case per of
                         Just p ->
                           case p of
                             x | x < 40 -> ("#FF0000", "\xf244")
                             x | x < 60 -> ("#FFAE00", "\xf243")
                             otherwise -> ("#FFF600", "\xf241")
                         Nothing ->
                           ("#FF0000", "\xf244")

      let sts = ico <> " " <> perTxt <> " (" <> tillEmpty <> ")"

      putText sts
      putText sts
      putText clr
    ["Charging"] -> do
      let [per] = take 1 . drop 1 $ bs 
      let [tillFull] = Txt.takeWhile (/= ' ') . Txt.strip <$> (take 1 . drop 2 $ bs)
      let sts = "\xf1e6 " <> per <> " (" <> tillFull <> ")"
      putText sts
      putText sts
      putText "#A8FF00"
    [x] -> do
      putText $ "?? " <> x
    y -> do
      putText $ "?? " <> show y

