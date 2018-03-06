module Main where

import           Data.VDOM.Client (runApp)

main :: IO ()
main = runApp init where
  init = 0 :: Int
