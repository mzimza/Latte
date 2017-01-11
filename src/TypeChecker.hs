{-# LANGUAGE TypeSynonymInstances #-}                                           
{-# LANGUAGE FlexibleInstances #-}                                              
{-# LANGUAGE FlexibleContexts #-}

module TypeChecker where

import System.IO ( stdin, hGetContents, hPutStr, stderr )

import AbsLatte
import PrintLatte 
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State  
import Data.Maybe
import CFGraph

type FEnv = Map.Map Ident (Type, [Arg])
type VEnv = Map.Map Ident Type
type TypeState = ({-CFGraph,-} FEnv, VEnv) 

initVEnv :: [Arg] -> VEnv 
initVEnv args = Map.fromList $ map (\(Arg t id) -> (id, t)) args

initFEnv :: FEnv
initFEnv = Map.fromList [(Ident "printInt", (Void, [Arg Int (Ident "a")])),
   (Ident "printString", (Void, [Arg Str (Ident "s")])),
   (Ident "error", (Void, [])),
   (Ident "readInt", (Int, [])),
   (Ident "readString", (Str, []))]

justTypesFEnv :: FEnv -> Map.Map Ident (Type, [Type])
justTypesFEnv fenv = foldr justTypes Map.empty $ Map.assocs fenv
   where
      justTypes (id, (ret, args)) m = Map.insert id (ret, types args) m 
      types s = map (\(Arg t id) -> t) s

getFunctions (Program defs) = do
   funs <- foldM (flip addFun) initFEnv defs
   checked <- check funs
   if checked then return funs
   else do
      hPutStr stderr "ERROR\n"
      error $ "Function 'main' with wrong return type or parameters!\n" where
         addFun (FnDef t id@(Ident s) args _) fenv = case Map.lookup id fenv of
            Nothing -> return $ Map.insert id (t, args) fenv
            Just x -> do
               hPutStr stderr "ERROR\n"
               error $ "Function '" ++ s ++ "' already declared!\n"
         check fenv = case Map.lookup (Ident "main") fenv of
            Nothing -> do
               hPutStr stderr "ERROR\n"
               error $ "No function 'main'!\n"
            Just (t, l) -> return $ t == Int && l == []

hasReturn (VRet:ss) = True
hasReturn (Ret e:ss) = True
hasReturn ((CondElse e s1 s2):ss) = case e1 of
   ELitTrue -> hasReturn [s1] || hasReturn ss
   ELitFalse -> hasReturn [s2] || hasReturn ss
   x -> (hasReturn [s1] && hasReturn [s2]) || (hasReturn [s1] && hasReturn ss) || (hasReturn [s2] && hasReturn ss) || hasReturn ss
   where
      (e1, t) = evalExpr e Map.empty
hasReturn ((Cond e s):ss) = case e1 of
   ELitTrue -> hasReturn [s] || hasReturn ss
   ELitFalse -> hasReturn ss
   x -> hasReturn ss
   where
   (e1, t) = evalExpr e Map.empty
hasReturn ((BStmt (Block s)):ss) = hasReturn s || hasReturn ss
hasReturn (s:ss) = hasReturn ss
hasReturn [] = False

hasReturnOrVoid stmts t = if hasReturn stmts == False && t == Void then (True, stmts ++ [VRet]) else (hasReturn stmts, stmts)
  
typeCheck p@(Program defs) = do
   fenv <- getFunctions p
   defs' <- forM defs (checkFunBlock fenv)
   return $ (justTypesFEnv fenv, (Program defs'))
   
checkFunBlock fenv (FnDef t (Ident id) args b@(Block stmts)) = if hasRet then do
      execStateT (checkBlock t b) (fenv, initVEnv args)
      return (FnDef t (Ident id) args (Block stmts'))
   else do
      hPutStr stderr "ERROR\n"
      error $ "No return in function: " ++ id ++ "!\n" where
      (hasRet, stmts') = hasReturnOrVoid stmts t

checkBlock :: (MonadTrans m, MonadState TypeState (m IO)) => Type -> Block -> m IO ()
checkBlock t (Block stmts) = do
   mapM (checkStmt t) stmts
   return ()

checkStmt :: (MonadTrans m, MonadState TypeState (m IO)) => Type -> Stmt -> m IO ()
checkStmt _ Empty = return ()
checkStmt _ stmt@(Incr id@(Ident s)) = do
   (fenv, venv) <- get
   case Map.lookup id venv of
      Nothing -> do
         lift $ hPutStr stderr "ERROR\n"
         error $ (printCodeLine stmt) ++ " No variable '" ++ s ++ "' declared in this scope!\n"
      Just t -> assertSameType Int t stmt
checkStmt _ stmt@(Decr id@(Ident s)) = do
   (fenv, venv) <- get
   case Map.lookup id venv of
      Nothing -> do
         lift $ hPutStr stderr "ERROR\n"
         error $ (printCodeLine stmt) ++ " No variable '" ++ s ++ "' declared in this scope!\n"
      Just t -> assertSameType Int t stmt
checkStmt _ stmt@(Ass id@(Ident s) e) = do
   --check if declared
   --check if of same type
   (fenv, venv) <- get
   vt <- case Map.lookup id venv of
      Nothing -> do
         lift $ hPutStr stderr "ERROR\n"
         error $ "ERROR\n" ++ (printCodeLine stmt) ++ "Variable '" ++ s ++ "' not declared in this scope!\n"
      Just x -> return x
   et <- checkExpr e stmt
   assertSameType vt et stmt
checkStmt _ stmt@(Decl t items) = do
   forM items (checkItem stmt t)
   return ()
checkStmt t stmt@(Ret e) = do
   et <- checkExpr e stmt
   assertSameType t et stmt
checkStmt t stmt@(VRet) = do
   assertSameType t Void stmt
checkStmt t stmt@(SExp e) = do
   _ <- checkExpr e stmt
   return ()
checkStmt t stmt@(BStmt b) = do
   (fenv, venv) <- get
   checkBlock t b
   put (fenv, venv)
checkStmt t stmt@(Cond e s) = do
   et <- checkExpr e stmt
   assertSameType Bool et stmt
   (fenv, venv) <- get
   checkStmt t s
   put (fenv, venv)
checkStmt t stmt@(CondElse e s1 s2) = do
   et <- checkExpr e stmt
   assertSameType Bool et stmt
   (fenv, venv) <- get
   checkStmt t s1
   put (fenv, venv)
   checkStmt t s2
   put (fenv, venv)
checkStmt t stmt@(While e s) = do
   et <- checkExpr e stmt
   assertSameType Bool et stmt
   (fenv, venv) <- get
   checkStmt t s
   put (fenv, venv)

checkItem :: (MonadTrans m, MonadState TypeState (m IO)) => Stmt -> Type -> Item -> m IO ()
checkItem stmt t (NoInit id@(Ident s)) = checkDoesNotExist stmt t id
checkItem stmt t (Init id@(Ident s) e) = do
   et <- checkExpr e stmt
   assertSameType t et stmt
   checkDoesNotExist stmt t id

checkDoesNotExist :: (MonadTrans m, MonadState TypeState (m IO)) => Stmt -> Type -> Ident -> m IO ()
checkDoesNotExist stmt t id@(Ident s) = do 
   (fenv, venv) <- get
   case Map.lookup id venv of
      Nothing -> return ()
      Just t -> do
         lift $ hPutStr stderr "ERROR\n"
         error $ (printCodeLine stmt) ++ " Variable '" ++ s ++ "' already declared!\n" 
   put (fenv, Map.insert id t venv)    

checkExists :: (MonadTrans m, MonadState TypeState (m IO)) => Stmt -> Ident -> m IO Type
checkExists stmt id@(Ident s) = do
   (_, venv) <- get 
   case Map.lookup id venv of
      Nothing -> do
         lift $ hPutStr stderr "ERROR\n"
         error $ (printCodeLine stmt) ++ " No variable '" ++ s ++ "' declared in this scope!\n"
      Just t -> return t

checkExpr :: (MonadTrans m, MonadState TypeState (m IO)) => Expr -> Stmt -> m IO Type
checkExpr exp@(EVar id@(Ident s)) stmt = checkExists stmt id
checkExpr (ELitInt i) _ = return Int
checkExpr ELitTrue _ = return Bool
checkExpr ELitFalse _ = return Bool
checkExpr (Neg e) stmt = do
   et <- checkExpr e stmt
   assertSameType Int et stmt
   return Int
checkExpr (Not e) stmt = do
   et <- checkExpr e stmt
   assertSameType Bool et stmt
   return Bool
checkExpr (EAdd e1 _ e2) stmt = checkIntOrString e1 e2 stmt
checkExpr (EMul e1 _ e2) stmt = checkBothSameType Int e1 e2 stmt
checkExpr (ERel e1 op e2) stmt = do
   et1 <- checkExpr e1 stmt
   et2 <- checkExpr e2 stmt
   assertSameType et1 et2 stmt
   assertComparable et1 op stmt
   return Bool
checkExpr (EAnd e1 e2) stmt = checkBothSameType Bool e1 e2 stmt
checkExpr (EOr e1 e2) stmt = checkBothSameType Bool e1 e2 stmt
checkExpr (EApp id@(Ident s) expr) stmt = do
   (fenv, venv) <- get
   case Map.lookup id fenv of
      Nothing -> do
         lift $ hPutStr stderr "ERROR\n"
         error $ (printCodeLine stmt) ++ " No function '" ++ s ++ "' declared!\n"
      Just (t, args) -> checkFunParams t args expr stmt   
checkExpr (EString s) _ = return Str

checkFunParams :: (MonadTrans m, MonadState TypeState (m IO)) => Type -> [Arg] -> [Expr] -> Stmt -> m IO Type
checkFunParams t args expr stmt = if (length args) == (length expr) then do
      forM (zip args expr) (checkParam stmt)
      return t
    else do
      lift $ hPutStr stderr "ERROR\n"
      error $ (printCodeLine stmt) ++ " Wrong number of parameters provided!\n"

checkParam :: (MonadTrans m, MonadState TypeState (m IO)) => Stmt -> (Arg, Expr) -> m IO Type
checkParam stmt ((Arg t id@(Ident s)), e) = do
   et <- checkExpr e stmt
   if t == et then return et
   else do
      lift $ hPutStr stderr "ERROR\n"
      error $ (printCodeLine stmt) ++ " Wrong type of parameter named '" ++ s ++ "'. Expected type: " ++ (show t) ++ ", got: " ++ (show et) ++ "!\n"

checkIntOrString e1 e2 stmt = do
   et1 <- checkExpr e1 stmt
   et2 <- checkExpr e2 stmt
   if (et1 == Int) then do
      assertSameType Int et2 stmt
      return Int
   else do
      assertSameType Str et2 stmt
      return Str 

checkBothSameType t e1 e2 stmt = do
   et1 <- checkExpr e1 stmt
   assertSameType t et1 stmt
   et2 <- checkExpr e2 stmt
   assertSameType t et2 stmt
   return t

printCodeLine stmt = filter (/= '\n') $ printTree stmt

--expected type, given type, in statement
assertSameType :: (MonadTrans m, MonadState TypeState (m IO)) => Type -> Type -> Stmt -> m IO ()
assertSameType t1 t2 stmt = if t1 == t2 then return ()
   else do
      lift $ hPutStr stderr "ERROR\n"
      error $ "Expected variable of type: " ++ (show t1) ++ ", got type: " ++ (show t2) ++ " in '" ++ (printCodeLine stmt) ++ "'!\n"         

--check if comparable with such operator
assertComparable _ EQU _ = return ()
assertComparable _ NE _ = return ()
assertComparable t _ stmt = if Set.member t (Set.fromList [Int]) then return ()
   else do
      lift $ hPutStr stderr "ERROR\n"
      error $ "In statement: " ++ printCodeLine stmt ++ "comparison of uncomparable type: " ++ show t ++ "!\n"

