module Main where

import System.IO (hPutStrLn)
import Data.Text (unpack)

import Language.VFDL

import Banner

testdef = 
    "FACTORY test IS \
    \PORT (\
    \   testPort: IN lane;\
    \)\
    \END FACTORY test;"

main :: IO ()
main = do
    hPutStrLn stdout $ unpack banner
    hPutStrLn stdout $ show $ testdef
