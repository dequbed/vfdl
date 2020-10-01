module Language.VFDL
    ( parseVFDL
    ) where

import Prelude hiding ((<|>))
import Data.Text (Text)

import Text.Parsec
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as L

import Language.VFDL.AST

reservedNames =
    [ "entity"
    , "is"
    , "port"
    , "architecture"
    , "begin"
    , "signal"
    ]

reservedOperators =
    [ '<'
    , '='
    , '/'
    ]

vfdlDef = L.emptyDef 
        { P.commentStart = "{-"
        , P.commentEnd = "-}"
        , P.commentLine = "--"
        , P.nestedComments = False
        , P.identStart = letter <|> char '_'
        , P.identLetter = alphaNum <|> char '_'
        , P.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , P.reservedNames = reservedNames
        , P.reservedOpNames = [reservedOperators]
        , P.caseSensitive = False
        }

lexer = P.makeTokenParser vfdlDef

symbol = P.symbol lexer
identifier = P.identifier lexer
parens = P.parens lexer

parsePortDirection = choice [port_in, port_out]
  where
    port_in = do
        symbol "IN"
        return In
    port_out = do
        symbol "OUT"
        return Out

port = do
    symbol "PORT"
    parens $ do
        pident <- identifier
        P.colon lexer
        pdir <- parsePortDirection
        ptype <- identifier
        P.semi lexer
        return $ Port pident pdir ptype

ports = optionMaybe $ many1 $ port

-- Parse a 'FACTORY' statement
factory = do
    factorySym -- 'FACTORY'
    ident <- identifier -- Get the identifier/name for the factory
    is -- 'IS'
    portM <- ports -- A port definition - optional if the factorio does no item I/O
    end ident

    return $ Factory ident portM
  where
    factorySym = symbol "FACTORY"
    is = symbol "IS"
    end ident = do
        symbol "END"
        factorySym
        symbol ident
        P.semi lexer

parseVFDL :: SourceName -> Text -> Either ParseError Factory
parseVFDL = parse factory
