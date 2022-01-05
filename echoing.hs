#!/usr/bin/env stack
-- stack script --resolver lts-18.18

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Control.Foldl as F
import Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Formatting as Ft
import System.Environment
import qualified Text.Printf as TP
import Turtle as Tu

run cmd args = fold (inproc cmd args empty) F.list
run_ cmd args = fold (proc cmd args empty) F.list

class Ln a where
    ln :: a -> Tu.Line
    ln = unsafeTextToLine . fln
    fln :: a -> T.Text

instance Ln TL.Text where
    fln = TL.toStrict

instance Ln T.Text where
    fln = id

instance Ln String where
    fln = T.pack

defaultCmdL = ["zfs", "list", "backup-tank"]

main = do
    delay :: Int <- read . M.fromMaybe "10" <$> lookupEnv "D"
    n <- lookupEnv "N"
    gotArgs <- map T.pack <$> getArgs
    let (cmd : args) = if null gotArgs then defaultCmdL else gotArgs
    mapM_
        ( \x -> do
            let m :: Int = (x * delay `div` 60) + 1
            let title = Ft.format (Ft.int Ft.% " time(s), " Ft.% Ft.int Ft.% " min, delay: " Ft.% Ft.int) x m delay
            lines <- (ln title :) <$> run cmd args
            mapM_ Tu.echo lines
            Tu.sleep 10
            let nl = length lines
            let n' = T.pack $ fromMaybe (show nl) n
            run_ "tput" ["cuu", n']
        )
        [1 ..]
