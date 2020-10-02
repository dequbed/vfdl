module Language.VFDL.Parse
    ( parseVFDL
    ) where

import Prelude hiding ((<|>), many, try)
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

port :: ParsecT Text () Identity [Port]
port = do
    symbol "PORT"
    p <- (P.parens lexer) $ many1 $ do
        pident <- identifier
        P.colon lexer
        pdir <- parsePortDirection
        ptype <- identifier
        semi

        return $ Port pident pdir ptype
    return p

ports :: ParsecT Text () Identity (Maybe [Port])
ports = optionMaybe $ port

-- Parse a 'FACTORY' statement
factory = do
    factorySym -- 'FACTORY'
    ident <- identifier -- Get the identifier/name for the factory
    symbol "IS"
    portM <- ports -- A port definition - optional if the factorio does no item I/O
    end ident

    return $ FactoryDeclaration ident portM
  where
    factorySym = symbol "FACTORY"
    end ident = do
        symbol "END"
        factorySym
        symbol $ unpack ident
        semi

tupleOrSingle :: ParsecT Text () Identity [Text]
tupleOrSingle = choice [ tuple, single ]
  where
    single = do
        a <- identifier
        return [a]
    tuple = parens $ P.commaSep lexer identifier

concurrent :: ParsecT Text () Identity [ConcurrentStatement]
concurrent = manyTill concurrent' (try $ symbol "END")

concurrent' :: ParsecT Text () Identity ConcurrentStatement
concurrent' = do
    ident <- tupleOrSingle
    symbol "<="
    op <- choice [merge, process, balance]
    semi
    return $ ConcurrentStatement ident op
  where
    process= try $ do
        ident <- identifier
        params <- P.parens lexer $ P.commaSep lexer parameter
        return $ CSProcessCall $ ProcessCall ident params
    balance = try $ CSBalance <$> tupleOrSingle
    merge = try $ do
        a <- identifier
        symbol "><"
        b <- identifier
        return $ CSOperator $ Merge a b
    parameter = choice [(Quoted <$> pack <$> P.stringLiteral lexer), (Unquoted <$> identifier)]

declarations = do
    ident <- identifier
    P.colon lexer
    decl_type <- identifier
    semi
    return $ BCTransport $ TransportDeclaration [ident] (Scoped decl_type)

architecture = do
    symbol "ARCHITECTURE"
    ident <- identifier
    symbol "OF"
    name <- identifier
    symbol "IS"
    decls <- many declarations
    symbol "BEGIN"
    statements <- concurrent
    end ident

    return $ ArchitectureBody ident name decls statements
  where
    end ident = do
        symbol $ unpack ident
        semi

defs = do
    s <- many $ choice [ (Left <$> factory), (Right <$> architecture) ]
    return $ foldl' (\(AST fs as) new -> case new of
        (Left f) -> AST (fs ++ [f]) as
        (Right a) -> AST fs (as ++ [a])) (AST [] []) s

parseVFDL :: SourceName -> Text -> Either ParseError AST
parseVFDL = parse defs
