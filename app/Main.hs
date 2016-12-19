{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           J
import           JAdapter
import           System.Directory
import           System.Environment
import           Text.Printf
import           Util

data Command = JumpCommand { key :: String }
             | ListCommand
             | SetJumperCommand { key :: String, dst :: String }
             | UnsetJumperCommand { key :: String }
             deriving Show

parseCommand :: [String] -> Command
parseCommand [] = parseCommand ["-l"]
parseCommand (cmd:xs)
  | cmd `elem` ["-l", "--list"] = ListCommand
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
exec JumpCommand{..}        = load >>= jump key
exec SetJumperCommand{..}   = load >>= setJumper key dst
exec UnsetJumperCommand{..} = load >>= unsetJumper key

listJumpers :: JStore -> IO ()
listJumpers = mapM_ ( \(key, dst) ->
                        putStrLn $ printf "echo \"%s -> %s\"" key dst
                    ) . listAll

jump :: String -> JStore -> IO ()
jump key store = let
  maybeDst = query key store
  in
  case maybeDst of
    Just dst -> expandHome dst >>= putStrLn . printf "cd %s"
    Nothing -> putStrLn $ printf "echo \"No such target: %s\"" key

setJumper :: String -> String -> JStore -> IO ()
setJumper key dst = save . update (key, dst)

unsetJumper :: String -> JStore -> IO ()
unsetJumper key = save . delete key

main :: IO ()
main = do
  args <- getArgs
  exec $ parseCommand args
