module Main where

import Options.Applicative

import System.IO (hPutStrLn)

import Data.Text (unpack)
import Data.Text.IO (readFile)

import Language.VFDL

import Banner

data Param = Param
    { version :: Bool
    , output :: FilePath
    , inputs :: [FilePath]
    }

params :: Parser Param
params = Param
    <$> switch (long "version" <> short 'v' <> help "Print the version of vfdlc")
    <*> strOption 
        ( long "output"
        <> short 'o'
        <> metavar "FILE"
        <> value "out.json"
        <> help "Write output to FILE"
        )
    <*> many (argument str (metavar "FILE"))

main :: IO ()
main = run =<< execParser opts
  where opts = info (params <**> helper)
            ( fullDesc <> progDesc "Very Factory Description Language Compiler")

run :: Param -> IO ()
run (Param True _ _) = hPutStrLn stdout $ unpack banner
run (Param _ _ [f]) = do
    c <- readFile f
    case parseVFDL f c of
        Left e -> hPutStrLn stdout $ show e
        Right ast -> compile ast
    testLayout
run (Param _ _ _) = hPutStrLn stdout "You need to provide exactly one input file at the moment"
