{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}

module TypeSketch where

-- import           Data.Attoparsec.Text       (Parser)
-- import qualified Data.Attoparsec.Text       as A

import           Data.Text
import           Data.Text.ICU.Char         (Bool_ (..), property)

import           Control.Applicative
import           Data.Functor
import           Data.Monoid

import           Data.Typeable              (Typeable)

import           Control.Unification

import           Control.Applicative
import           Control.Monad.Error        (ErrorT (..), runErrorT)
import           Control.Monad.Identity
import           Control.Monad.Logic        (Logic (), runLogic)
import           Control.Monad.Trans
import           Control.Monad.Trans.Error
import           Control.Unification
import           Control.Unification
import           Control.Unification.IntVar
import           Control.Unification.IntVar
import           Data.Foldable
import           Data.Foldable
import           Data.Traversable
import           Data.Traversable

pairWith :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
pairWith f xs' ys' = pairWB xs' ys' id
  where
    pairWB (x:xs) (y:ys) cc = pairWB xs ys (cc . (:) (f x y))
    pairWB []     []     cc = Just (cc [])
    pairWB _      _      _  = Nothing



----------------------------------------------------------------
data T a = T String [a]
    deriving (Show, Functor, Foldable, Traversable)

foo x y = UTerm$T "foo" [x,y]
bar     = UTerm$T "bar" []
baz x   = UTerm$T "baz" [x]

atom n  = UTerm$T n []

instance Unifiable T where
    zipMatch (T m ls) (T n rs)
        | m /= n    = Nothing
        | otherwise =
            T n <$> pairWith (\l r -> Right(l,r)) ls rs

----------------------------------------------------------------
-- Some aliases for simplifying type signatures:
type PrologTerm           = UTerm T IntVar
type PrologFailure        = UnificationFailure T IntVar
type PrologBindingState   = IntBindingState T
type FallibleBindingMonad = ErrorT PrologFailure (IntBindingT T Identity)
type PrologMonad          = ErrorT PrologFailure (IntBindingT T Logic)

----------------------------------------------------------------

-- | @example1(X,Y,Z) :- X = Y, Y = Z.@
-- example1 :: PrologTerm -> PrologTerm -> PrologTerm -> Example
example1 x y z = do
  x =:= y
  y =:= z

-- | A more efficient implementation of 'example1'.
-- example1' :: PrologTerm -> PrologTerm -> PrologTerm -> Example
example1' x y z = do
    y' <- x =:= y
    y' =:= z


-- N.B., This type signature is (unfortunately) necessary in order
-- to avoid ambiguity when we discard the variable it returns. But,
-- if you never discard the result, then you should be able to get
-- away with commenting out the signature.
getFreeVar
    :: (Applicative m, Monad m)
    => ErrorT PrologFailure (IntBindingT T m) PrologTerm
getFreeVar = lift (UVar <$> freeVar)


-- | @example2(X,Z) :- X = Y, Y = Z.@
-- example2 :: PrologTerm -> PrologTerm -> Example
example2 x z = do
    y <- getFreeVar
    x =:= y
    y =:= z


-- | @example3(X,Z) :- example1(X,Y,Z).@
-- example3 :: PrologTerm -> PrologTerm -> Example
example3 x z = do
    y <- getFreeVar
    example1 x y z


-- BUG: transformers-0.4.1.0 deprecated Control.Monad.Trans.Error
-- (transformers-0.3.0.0 says it's fine). In order to use
-- Control.Monad.Trans.Except, we need a monoid instance... so we'll
-- need to redefine UnificationFailure to deal with all this
--
-- | @example4(X) :- X = bar; X = backtrack.@
-- example4 :: PrologTerm -> Example
example4 x = (x =:= bar) <|> (x =:= atom "backtrack")


-- However, note that the semantics of 'example4' may not be what
-- is expected. In particular, this example will fail with a
-- @TermMismatch@ because the invocation of 'example4' commits to
-- the success of its first branch, so that by the time we execute
-- the last line of this example, we can't get the 'example4'
-- invocation to backtrack and try the other branch.
commitsTooEarly = do
    x <- getFreeVar
    example4 x
    x =:= atom "backtrack"

{- However, both of these examples work just fine (since the first
-- branch of 'example4' fails immediately). Thus, choice does indeed
-- work, even if backtracking doesn't:

choiceWorks1 = do
    x <- getFreeVar
    x =:= atom "backtrack"
    example4 x

choiceWorks2 = do
    example4 (atom "backtrack")

-}


-- | Note that the semantics of this test may not be what is expected,
-- depending on the exact monad stack used. In particular, for
-- @FallibleBindingMonad@ it does not give Prolog's semantics!
backtrackingTest = do
    x <- getFreeVar
    y <- getFreeVar
    (x =:= y >> failure) <|> return (foo x y)
    where
    failure = atom "a" =:= atom "b"

----------------------------------------------------------------
runFBM :: FallibleBindingMonad a
       -> (Either PrologFailure a, PrologBindingState)
runFBM = runIdentity . runIntBindingT . runErrorT

evalFBM :: FallibleBindingMonad a -> Either PrologFailure a
evalFBM = runIdentity . evalIntBindingT . runErrorT

execFBM :: FallibleBindingMonad a -> PrologBindingState
execFBM = runIdentity . execIntBindingT . runErrorT


runProlog
    :: PrologMonad a
    -> Maybe (Either PrologFailure a, PrologBindingState)
runProlog = observeMaybe . runIntBindingT . runErrorT

evalProlog :: PrologMonad a -> Maybe (Either PrologFailure a)
evalProlog = observeMaybe . evalIntBindingT . runErrorT

execProlog :: PrologMonad a -> Maybe PrologBindingState
execProlog = observeMaybe . execIntBindingT . runErrorT

observeMaybe :: Logic a -> Maybe a
observeMaybe mx = runLogic mx (\a _ -> Just a) Nothing

----------------------------------------------------------------



-- Questions:
-- - How to compile GOOPS classes?
-- - How to compile typeclasses?

-- HM + the following extensions:
-- - Uses the compositional type checking algorithm
-- - GADTs


{-
newtype Name = Name Int

type KPt = KindPoint
type KFun k = KPt -> k
type KCon = KindConstraint

data KindVar
data KindPoint
data KindConstraint

class Kind k where {}
instance Kind KindVar where {}
instance Kind KindPoint where {}
instance Kind KindConstraint where {}
instance (Kind k, Kind k') => Kind (k -> k') where {}

data Type k where
  TypeRef    :: Kind k => TypeName                   -> Type k
  TypeApp    :: Kind k => Type (KFun k) -> Type KPt  -> Type k
  TypeConstr :: Kind k => Type KCon     -> Type k    -> Type k
  TypeVar    ::           TVarName                   -> Type KindVar
  TypeForall ::           TVarName      -> Type KPt  -> Type KPt
  TypeArrow  ::           Type KPt      -> Type KPt  -> Type KPt
  TypeCUnion ::           [Type KCon]   -> Type KCon -> Type KCon

newtype ValName  = ValName  Name
newtype ConName  = ConName  Name
newtype TypeName = TypeName Name
newtype TVarName = TVarName Name

data Value where
  VLitBool   :: Bool                      -> Value
  VLitInt    :: Integer                   -> Value
  VLitFloat  :: Double                    -> Value
  VLitList   :: [Value]                   -> Value
  VLitVector :: [Value]                   -> Value
  VLambda    :: ValName -> Value          -> Value
  VLet       :: ValName -> Value -> Value -> Value
  VReference :: ValName                   -> Value

data Judgment where
  Equality   :: Kind k => Type k        -> Type k    -> Judgment
  TypeDef    :: Kind k => TypeName      -> k         -> Judgment
  ConDef     :: Kind k => ConName       -> Type k    -> Judgment
  ValDef     ::           ValName       -> Value     -> Judgment
  Signature  ::           ValName       -> Type KPt  -> Judgment
  Subtype    ::           Type KPt      -> Type KPt  -> Judgment

newtype Symbol = Symbol String
data SApp t

data SExpr t where
  SLitBool   :: Bool                         -> SExpr Bool
  SLitString :: String                       -> SExpr String
  SLitInt    :: Integer                      -> SExpr Integer
  SLitNil    ::                                 SExpr ()
  SLitCons   :: SExpr t       -> SExpr t'    -> SExpr (t, t')
  SExpr      :: Symbol        -> SExpr t     -> SExpr (SApp t)

sexprParser :: Parser (SExpr ())
sexprParser = return SLitNil

data PVar
data PAtom
data PTerm
data PPred
data PStruct
data PQuery
data PClause
data PProgram

data Prolog a where
  PNum     :: Integer                            -> Prolog Integer
  PVar     :: Text                               -> Prolog PVar
  PAtom    :: Text                               -> Prolog PAtom
  PStruct  :: Prolog PAtom     -> [Prolog PTerm] -> Prolog PStruct
  PTNum    :: Prolog Integer                     -> Prolog PTerm
  PTVar    :: Prolog PVar                        -> Prolog PTerm
  PTAtom   :: Prolog PAtom                       -> Prolog PTerm
  PTStruct :: Prolog PStruct                     -> Prolog PTerm
  PPAtom   :: Prolog PAtom                       -> Prolog PPred
  PPStruct :: Prolog PStruct                     -> Prolog PPred
  PQuery   :: [Prolog PPred]                     -> Prolog PQuery
  PFact    :: Prolog PPred                       -> Prolog PClause
  PHorn    :: Prolog PPred     -> [Prolog PPred] -> Prolog PClause
  PProgram :: [Prolog PClause] -> Prolog PQuery  -> Prolog PProgram

deriving instance Eq (Prolog a)
deriving instance Show (Prolog a)
deriving instance Typeable (Prolog a)

(<?>) :: Parser a -> Text -> Parser a
parser <?> msg = (A.<?>) parser (unpack msg)
infix 0 <?>

annotate :: Text -> Parser a -> Parser a
annotate msg parser = parser <?> msg

whitespace :: Parser ()
whitespace = void $ A.takeWhile (property WhiteSpace)

token :: Parser a -> Parser a
token p = whitespace *> p <* whitespace

tokenT :: Text -> Parser Text
tokenT t = token $ annotate ("token: " <> t) $ A.string t

comma :: Parser ()
comma = void $ tokenT ","

commaSep :: Parser a -> Parser [a]
commaSep = (`A.sepBy1'` comma)

clauseParser :: Parser (Prolog PClause)
clauseParser = (factParser <|> hornParser) <* tokenT "."
  where
    factParser = PFact <$> predParser
    hornParser = PHorn <$> predParser
                       <*  tokenT ":-"
                       <*> predListParser

notReserved, isUpper, isLower :: Char -> Bool
notReserved ':'  = False
notReserved '-'  = False
notReserved '('  = False
notReserved ')'  = False
notReserved ','  = False
notReserved '.'  = False
notReserved ' '  = False
notReserved '\n' = False
notReserved _    = True
isUpper          = property Uppercase
isLower          = property Lowercase

identParser :: Parser Text
identParser = A.takeWhile notReserved

numParser :: Parser (Prolog Integer)
numParser = PNum <$> A.signed A.decimal

varParser :: Parser (Prolog PVar)
varParser = PVar <$> ((<>) <$> firstChar <*> identParser)
  where
    firstChar :: Parser Text
    firstChar = pack . (:[]) <$> A.satisfy (\c -> isUpper c && notReserved c)

atomParser :: Parser (Prolog PAtom)
atomParser = PAtom <$> ((<>) <$> firstChar <*> identParser)
  where
    firstChar :: Parser Text
    firstChar = pack . (:[]) <$> A.satisfy (\c -> isLower c && notReserved c)

structParser :: Parser (Prolog PStruct)
structParser = PStruct <$> atomParser
                       <*  tokenT "("
                       <*> commaSep termParser
                       <*  tokenT ")"

termParser :: Parser (Prolog PTerm)
termParser = (PTNum    <$> numParser)    <|>
             (PTStruct <$> structParser) <|>
             (PTAtom   <$> atomParser)   <|>
             (PTVar    <$> varParser)

predParser :: Parser (Prolog PPred)
predParser = (PPAtom <$> atomParser)     <|>
             (PPStruct <$> structParser)

predListParser :: Parser [Prolog PPred]
predListParser = commaSep predParser

queryParser :: Parser (Prolog PQuery)
queryParser = PQuery <$> (tokenT "?-" *> predListParser <* tokenT ".")

prologParser :: Parser (Prolog PProgram)
prologParser = PProgram <$> A.many' clauseParser <*> queryParser



-- newtype PVar = PVar Text
--              deriving (Eq, Show, Read, Typeable, Data)

-- newtype PAtom = PAtom Text
--               deriving (Eq, Show, Read, Typeable, Data)

-- data PPrim = PPAtom   PAtom
--            | PPStruct PAtom [PTerm]
--            deriving (Eq, Show, Read, Typeable, Data)

-- data PTerm = PTNat  Natural
--            | PTVar
--            | PTPrim PPrim
--            deriving (Eq, Show, Read, Typeable, Data)

-- newtype PPred = PPred PPrim
--               deriving (Eq, Show, Read, Typeable, Data)

-- data PClause = PFact PPred
--              | PHorn PPred [PPred]
--              deriving (Eq, Show, Read, Typeable, Data)

-- newtype PQuery = PQuery [PPred]
--                deriving (Eq, Show, Read, Typeable, Data)

-- data Prolog = Prolog [PClause] PQuery
--             deriving ( Eq, Show, Read, Typeable, Data )


-}


-- Prerequisite information:
-- Λ(τ) indicates the type of a list of τ
--
-- The unifier U is defined as follows:
-- U(∅) = ∅
-- U({α ∼ α} ∪ Π) = U(Π)
-- U({α ∼ τ} ∪ Π) | α ∈ vars(τ) = error "Infinite type"
--                 , otherwise   = let Ψ = {α ↝ τ} in U(Ψ(Π)) ∘ Ψ
-- U({τ ∼ α} ∪ Π) = U({α ∼ τ} ∪ Π)
-- U({(τ → μ) ∼ (τ' → μ')} ∪ Π) = U(Π ∪ {τ ∼ τ', μ ∼ μ'})
-- U({T(τ₁ … τₙ) ∼ T(τ₁' … τₙ')} ∪ Π = U(Π ∪ {τ₁ ∼ τ₁', …, τₙ ∼ τₙ'})
-- U({τ ∼ τ'} ∪ Π) = error "Conflicting constraints"
--

--------------------------------------------------------------------------------
-- Constructors:
--
-- [ (c ∷ τ) ∈ Υ ] ∧ [ (∅ ⊢ τ') = inst(∅) ⊢ τ ]
-- ───────────────────────────────────────────── CON
--               (Γ ∪ ∅) ⊢ c ∷ τ'
--
--------------------------------------------------------------------------------
-- Polymorphic variables:
--
-- [ Γ(x) = Δ ⊢ τ ] ∧ [ Δ' ⊢ τ' = inst(Δ) ⊢ τ ]
-- ────────────────────────────────────────────── POLYVAR
--               (Γ ∪ Δ') ⊢ x ∷ τ'
--
--------------------------------------------------------------------------------
-- Monomorphic variables:
--
-- [ x ∉ dom(Γ) ] ∧ [ new(α) ]
-- ──────────────────────────── MONOVAR
--   (Γ ∪ {x ∷ α}) ⊢ x ∷ α
--
--------------------------------------------------------------------------------
-- Lambda abstraction:
--
-- [ Γ ∪ Δ ⊢ E ∷ τ ] ∧ [ (x ∷ τ') ∈ Δ ]
-- ────────────────────────────────────── ABS
--    (Γ ∪ Δ \ x ⊢ (λx ↦ E) ∷ τ' → τ
--
--
-- [ Γ ∪ Δ ⊢ E ∷ τ ] ∧ [ x ∉ dom(Δ) ] ∧ [ new(α) ]
-- ───────────────────────────────────────────────── ABS'
--    (Γ ∪ Δ) ⊢ (λx ↦ E) ∷ α → τ
--
--------------------------------------------------------------------------------
-- Application:
--
-- [ Γ ∪ Δ₁ ⊢ E ∷ τ' ] ∧ [ Γ ∪ Δ₂ ⊢ F ∷ τ'' ]
-- ─────────────────────────────────────────── APP
--           (Γ ∪ Δ) ⊢ E F ∷ τ
-- where
--   new(α)
--   Ψ = U({Δ₁, Δ₂}, {τ' ∼ τ'' → α})
--   Δ = Ψ(Δ₁) ∪ Ψ(Δ₂)
--   τ = Ψ(α)
--
--------------------------------------------------------------------------------
-- Case expressions:
--
--     [ Θ₀ ⊢ E ∷ τ₀ ]
--   ∧ [ Δ₁' ⊢ P ∷ τ₁' ] ∧ [ Θ₁ ⊢ E ∷ τ₁ ]
--   ∧ …
--   ∧ [ Δₙ' ⊢ P ∷ τₙ' ] ∧ [ Θₙ ⊢ E ∷ τₙ ]
-- ───────────────────────────────────────── CASE
-- Γ ∪ Δ ⊢ (case E of P₁ ↦ E₁ … Pₙ ↦ Eₙ) ∷ τ
-- where
--   new(α)
--   Ψ = U({Δ₀, Δ₁, Δ₁', …, Δₙ, Δₙ'}, {τ₀ ∼ τₖ', τₖ ∼ α | k = 1 … n})
--   Δ = Ψ(Δ₀ ∪ Σ₁ ∪ … ∪ Σₙ)
--   Θₖ = Γ ∪ Δₖ
--   Σₖ = Ψ(Δₖ) \ dom(Δₖ')
--   τ = Ψ(α)
--
--     [ new(α) ]
-- ────────────────── VARPAT
-- { x ∷ α } ⊢ x ∷ α
--
-- ∧ [ (c ∷ τ₁ → … → τₙ → T(Λ(τ))) ∈ Υ ]
-- ∧ [ Δ₁ ⊢ P₁ ∷ τ₁' ] ∧ … ∧ [ Δₙ ⊢ Pₙ ∷ τₙ' ]
-- ─────────────────────────────────────────── CONPAT
--        Δ ⊢ c(P₁, …, Pₙ) ∷ Ψ(T(Λ(τ)))
-- where
--   Ψ = U({Δ₁, …, Δₙ}, {τ₁ ∼ τ₁', …, τₙ ∼ τₙ'})
--   Δ = Ψ(Δ₁) ∪ … ∪ Ψ(Δₙ)
--
--------------------------------------------------------------------------------
-- Let expressions:
--
-- [ Θ ⊢ f Λ(P₁) = E₁ ] ∧ … ∧ [ Θₙ ⊢ f Λ(Pₙ) = Eₙ ] ∧ [ Θ' ⊢ E ∷ τ ]
-- ────────────────────────────────────────────────────────────────── LET
--         Θ ⊢ let f Λ(P₁) = E₁ … f Λ(Pₙ) = Eₙ in E ∷ Ψ(τ)
-- where
--   Θ = Γ ∪ Δ
--   Θ' = Γ' ∪ Δ'
--   Θₖ = Γ ∪ Δₖ
--   Ψ₀ = U({Δ₁, …, Δₙ}, {Δₖ(∫) ∼ α | k = 1 … n})
--   Ψ = U({Δ₀, Δ'})
--   Σₖ = Ψ(Δₖ) \ {f}
--   Δ₀ = Σ₁ ∪ … ∪ Σₙ
--   Δ = Ψ(Δ') ∪ Ψ(Δ₀)
--
--------------------------------------------------------------------------------
-- Definitions:
--
-- [ Δ₁ ⊢ P₁ ∷ τ₁ ] ∧ … ∧ [ Δₙ ⊢ Pₙ ∷ τₙ ] ∧ [ Γ ∪ Δ' ⊢ E ∷ τ₀ ]
-- ───────────────────────────────────────────────────────────── DEF
--                  Γ ∪ Δ ⊢ f(P₁, …, Pₙ) = E
-- where
--   Δ₀ = {f ∷ τ₁ → … → τₙ → τ₀}
--   Δ = (Ψ(Δ₀) ∪ Ψ(Δ')) \ (Δ₁ ∪ … ∪ Δₙ)
--   Ψ = U({Δ₀, Δ₁, …, Δₙ, Δ'})
--
--
-- SOURCES:
-- http://gergo.erdi.hu/projects/tandoori/Tandoori-Compositional-Typeclass.pdf


-- Symbols: ─│├┤┐└┘┌┼┬╮╯∷→⇒
--
-- (sig vname : type) ──╮
-- (sig vname ∷ type) ──┤
-- (sig vname :: type) ─┤
-- (sig vname type) ────┼──→ (sig vname type) ⇒ (== vname type)
-- (:: vname type) ─────┤
-- (vname :: type) ─────┤
-- (∷ vname type) ──────┤
-- (vname ∷ type) ──────┤
-- (: vname type) ──────┤
-- (vname : type) ──────╯
--
-- (tsyn tname type) ───┬──→ (tsyn tname type) ⇒ (== tname type)
-- (type tname type) ───╯
--
-- (data tname (cname type*)*) ⇒ (ddef tname cname type*)*
--
-- (ddef tname cname type) ⇒ (== cname (→ type tname))
--                         + (define tname id)
