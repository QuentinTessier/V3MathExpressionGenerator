{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module TypeCheck where

import Control.Applicative ((<|>))
import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.Map as Map
import Data.Function ((&))

import Utils.Annotation
import Utils.Miscs
import Syntax
import Type

-- Typed AST

type Typed a = Annotation Type a

type TypedVariable = Typed Name

data TypedExpr = TypedLit (Typed Lit)
               | TypedVar (Typed (Name, [Name]))
               | TypedBin (Typed (Name, TypedExpr, TypedExpr))
               | TypedUn (Typed (Name, TypedExpr))
               | TypedCall (Typed (Name, [TypedExpr]))
               | TypedLet (Typed (TypedVariable, TypedExpr))
               deriving (Show)

data TypedStatement = TypedFormula (Typed (Name, [TypedVariable], TypedExpr))
                    | TypedTDef (Typed (Name, [TypedVariable]))
                    | TypedGlobal (Typed (TypedVariable, TypedExpr))
                    deriving (Show)

type TypedProgram = [TypedStatement]

-- Context

type TypeTable = SymbolTable Name Type

data TCError = TypeError Type Type
           | Generic String
           deriving (Show)

type TCLog = [TCError]

data TCEnv = TCEnv {
    formulas :: TypeTable,
    globals :: TypeTable,
    scopes :: [TypeTable],
    typeinfos :: [Type],
    err :: TCLog
}

defaultTypeCheckerEnv :: TCEnv
defaultTypeCheckerEnv = TCEnv {
    formulas = SymTable $ Map.empty,
    globals = SymTable $ Map.empty,
    scopes = [],
    typeinfos = types,
    err = []
}

type StateExpect e s a = ExceptT e (State s) a
type TypeChecker a = StateExpect TCLog TCEnv a

runStateExcept :: s -> StateExpect e s a -> (Either e a, s)
runStateExcept c se = runState (runExceptT se) c

runTypeChecker :: TCEnv -> TypeChecker a -> (Either TCLog a, TCEnv)
runTypeChecker = runStateExcept

-- Environment Management

extractMap :: TypeTable -> Map.Map Name Type
extractMap (SymTable m) = m

pushGlobal :: MonadState TCEnv m => TypedVariable -> m ()
pushGlobal (Ann t n) = modify $ \env -> env { globals = SymTable $ Map.insert n t (extractMap $ globals env) }

pushFormula :: MonadState TCEnv m => TypedVariable -> m ()
pushFormula (Ann t n) = modify $ \env -> env { formulas = SymTable $ Map.insert n t (extractMap $ formulas env) }

pushLocal :: MonadState TCEnv m => TypedVariable -> m ()
pushLocal (Ann t n) = modify $ \env -> case scopes env of
    [] -> env { scopes = [SymTable $ Map.singleton n t] }
    (x:xs) -> env { scopes = SymTable (Map.insert n t (extractMap x)) : xs }

freshScope, dropScope :: MonadState TCEnv m => m ()
freshScope = modify  $ \env -> env { scopes = SymTable Map.empty : scopes env }
dropScope = modify $ \env -> env { scopes = tail $ scopes env }

scoped :: MonadState TCEnv m => m a -> m a
scoped act = do
            freshScope
            res <- act
            dropScope
            return res

findGlobal :: MonadState TCEnv m => Name -> m (Maybe Type)
findGlobal n = gets $ \env -> env
                                & globals
                                & extractMap
                                & Map.lookup n

findFormula :: MonadState TCEnv m => Name -> m (Maybe Type)
findFormula n = gets $ \env -> env
                                & formulas
                                & extractMap
                                & Map.lookup n

findLocal :: MonadState TCEnv m => Name -> m (Maybe Type)
findLocal n = gets $ \env -> env
                            & scopes
                            & map (Map.lookup n . extractMap)
                            & foldl (<|>) Nothing

findSymbol :: MonadState TCEnv m => Name -> m (Maybe Type)
findSymbol n = do
    val <- findLocal n
    case  val of
        Just t -> return $ Just t
        Nothing -> findGlobal n

-- TypeChecking

checkVar :: Expr -> TypeChecker TypedExpr
checkVar (Var n fields) = do
    sym <- findSymbol n
    case sym of
        Just tSym -> case findVarType tSym fields of
            Just t -> return $ TypedVar (Ann t (n, fields))
            Nothing -> throwError [Generic "WIP"]
        Nothing -> throwError [Generic "Variable out of bound"]

checkBin :: Expr -> TypeChecker TypedExpr
checkBin (Bin name lhs rhs) = do
    lhs' <- checkExpr lhs
    rhs' <- checkExpr rhs
    return $ TypedBin (Ann aFloat (name, lhs', rhs'))

checkExpr :: Expr -> TypeChecker TypedExpr
checkExpr = \case
    (Lit l) -> case l of
                f@(LFloat _) -> return $ TypedLit (Ann aFloat f)
                i@(LInt _) -> return $ TypedLit (Ann aInt i)
    v@(Var n [fields]) -> checkVar v
    b@(Bin name lhs rhs) -> checkBin b