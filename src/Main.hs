module Main (
  main
) where

import Spacerace.Main

main :: IO ()
main = run (Config "192.168.1.92" 5558 5557 5556 "./maps")
