module Language.VFDL.Parse
    ( parseVFDL
    ) where

import Prelude hiding ((<|>), many)
import Data.Text (Text, pack, unpack)

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
identifier = pack <$> P.identifier lexer
parens = P.parens lexer
semi = P.semi lexer

parsePortDirection = choice [port_in, port_out]
  where
    port_in = do
        symbol "IN"
        return In
    port_out = do
        symbol "OUT"
        return Out

port :: ParsecT Text () Identity Port
port = do
    symbol "PORT"
    p <- (P.parens lexer) $ do
        pident <- identifier
        P.colon lexer
        pdir <- parsePortDirection
        ptype <- identifier
        semi

        return $ Port pident pdir ptype
    return p

ports :: ParsecT Text () Identity (Maybe [Port])
ports = optionMaybe $ many1 $ port

-- Parse a 'FACTORY' statement
factory = do
    factorySym -- 'FACTORY'
    ident <- identifier -- Get the identifier/name for the factory
    symbol "IS"
    portM <- ports -- A port definition - optional if the factorio does no item I/O
    end ident

    return $ Factory ident portM
  where
    factorySym = symbol "FACTORY"
    end ident = do
        symbol "END"
        factorySym
        symbol $ unpack ident
        semi

concurrent = many $ do
    ident <- identifier
    symbol "<="
    op <- choice [functionCall, split, combine]
    semi
    return $ Statement ident op
  where
    functionCall = do
        ident <- identifier
        params <- parens $ many1 identifier
        return $ FunctionCall ident params
    split = do
        a <- identifier
        symbol "|"
        b <- identifier
        return $ Split a b
    combine = do
        a <- identifier
        symbol "&"
        b <- identifier
        return $ Combine a b

architecture = do
    symbol "architecture"
    ident <- identifier
    symbol "IS"
    symbol "begin"
    statements <- concurrent
    end ident

    return $ Architecture ident statements
  where
    end ident = do
        symbol "END"
        symbol $ unpack ident
        semi


parseVFDL :: SourceName -> Text -> Either ParseError Factory
parseVFDL = parse factory
