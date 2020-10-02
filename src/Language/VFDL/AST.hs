module Language.VFDL.AST where

type Identifier = Text
type TypeString = Text

data FactoryDeclaration = FactoryDeclaration
    { factory_identifier :: Identifier
    , factory_header :: Maybe PortList
    } deriving (Eq, Show)

type PortList = [Port]

data PortDirection = In | Out
    deriving (Eq, Show)

data Port = Port
    { portIdentifier :: Text
    , portDirection :: PortDirection
    , portType :: Text
    } deriving (Eq, Show)

data ArchitectureBody = ArchitectureBody
    { arch_identifier :: Identifier
    , arch_name :: Identifier
    , arch_declarations :: [BlockDeclaration]
    , arch_statements :: [ConcurrentStatement]
    } deriving (Eq, Show)

data BlockDeclaration
    = BCTransport TransportDeclaration
    | BCComponent ComponentDeclaration
    deriving (Eq, Show)

data TransportDeclaration = TransportDeclaration
    { transport_identifier_list :: [Identifier]
    , transport_type :: ConstructedType
    } deriving (Eq, Show)

data ConstructedType
    = Scoped TypeString -- ^ Type valid in the scope it was defined in
    | Resolved TypeString -- ^ Fully resolved type
    deriving (Eq, Show)

data ComponentDeclaration = ComponentDeclaration
    { component_identifier :: Identifier
    , component_ports :: Maybe PortList
    } deriving (Eq, Show)

data Parameter
    = Quoted Text
    | Unquoted Identifier
    deriving (Eq, Show)

data ConcurrentStatement = ConcurrentStatement
    { cs_target :: [Identifier]
    , cs_statement :: Statement
    } deriving (Eq, Show)

data Statement
    = CSProcessCall ProcessCall
    | CSBalance [Identifier]
    | CSOperator Operator
    deriving (Eq, Show)

data ProcessCall = ProcessCall
    { processName :: Identifier
    , processParams :: [Parameter]
    } deriving (Eq, Show)

data Operator
    = Merge Identifier Identifier
    deriving (Eq, Show)

data AST = AST [FactoryDeclaration] [ArchitectureBody]
    deriving (Eq, Show)

instance Semigroup AST where
    (AST f a) <> (AST g b) = AST (f<>g) (a<>b)
instance Monoid AST where
    mempty = AST [] []
