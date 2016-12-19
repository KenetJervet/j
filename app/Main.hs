{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict          #-}

module Main where

import           J
import           JAdapter
import           System.Directory
import           System.Environment
import           Text.Printf
import           Util

data Command = JumpCommand { key :: String }
             | ListCommand
             | PrintJumperCommand { key :: String }
             | SetJumperCommand { key :: String, dst :: String }
             | UnsetJumperCommand { key :: String }
             deriving Show

parseCommand :: [String] -> Command
parseCommand [] = parseCommand ["-l"]
parseCommand (cmd:xs)
  | cmd `elem` ["-l", "--list"] = ListCommand
  | cmd `elem` ["-p", "--print"] = let
      key:_ = xs
      in
      PrintJumperCommand key
  | cmd `elem` ["-s", "--set"] = let
      key:dst:_ = xs
      in
      SetJumperCommand key dst
  | cmd `elem` ["-u", "-d", "--unset"] = let
      key:_ = xs
      in
      UnsetJumperCommand key
  | otherwise = JumpCommand cmd

exec :: Command -> IO ()
exec ListCommand            = load >>= listJumpers
exec PrintJumperCommand{..} = load >>= printJumper key
exec JumpCommand{..}        = load >>= jump key
exec SetJumperCommand{..}   = load >>= setJumper key dst
exec UnsetJumperCommand{..} = load >>= unsetJumper key

fmtJumper :: String -> String -> IO ()
fmtJumper key dst = putStrLn $ printf "echo \"%s -> %s\";" key dst

listJumpers :: JStore -> IO ()
listJumpers = mapM_ (uncurry fmtJumper) . listAll

printJumper :: String -> JStore -> IO ()
printJumper key store = let
  maybeDst = query key store
  in
  case maybeDst of
    Just dst -> fmtJumper key dst
    Nothing  -> putStrLn $ printf "echo \"No such jumper: %s\"" key

jump :: String -> JStore -> IO ()
jump key store = let
  maybeDst = query key store
  in
  case maybeDst of
    Just dst -> expandHome dst >>= putStrLn . printf "cd %s"
    Nothing  -> putStrLn $ printf "echo \"No such jumper: %s\"" key

setJumper :: String -> String -> JStore -> IO ()
setJumper key dst = save . update (key, dst)

unsetJumper :: String -> JStore -> IO ()
unsetJumper key = save . delete key

main :: IO ()
main = do
  args <- getArgs
  exec $ parseCommand args
