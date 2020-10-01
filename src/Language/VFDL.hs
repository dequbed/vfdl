module Language.VFDL
    ( parseFDL
    ) where

import Text.Parsec
import qualified Text.Parsec.Token as P

data AST = AST

vfdlDef :: P.LanguageDef
vfdlDef = emptyDef 
        { P.commentStart = "{-"
        , P.commentEnd = "-}"
        , P.commentLine = "--"
        }

parseVFDL :: Text -> Text -> Either ParseError AST
parseVFDL = undefined
