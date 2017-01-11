{-# LANGUAGE TypeSynonymInstances #-}                                           
{-# LANGUAGE FlexibleInstances #-}                                              
{-# LANGUAGE FlexibleContexts #-}

module CFGraph where

import AbsLatte
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State  
import Data.Maybe

type Label = Int

data CFGraph = CFGraph {
   blocks :: Map.Map Label CFBlock,
   nextL :: Label
} deriving (Eq, Show)

data CFBlock = CFBlock {
   stmts :: [Stmt],
   jumpTo :: Jump,
   inEdges :: [Label],
   env :: EEnv
} deriving (Eq, Show)

type Loc = Int
type Env = Map.Map Ident (Loc, Type)
type EEnv = Map.Map Ident (CExpr, Type)

data Jump = JCond {true :: Label, false :: Label, cond :: Expr} | JJump Label deriving (Eq, Show)

--typ danych odpowiedzialny za przechowywanie w miare mozliwosci wyliczonych prostych wyrazen
data CExpr = Exp Expr | Phi Type [(CExpr, Label)] | CReg Int | PhiPlaceholder Int | CStr Int String deriving (Eq)

instance Show CExpr where
   show (Exp (ELitInt i)) = show i
   show (Exp ELitTrue) = "true"
   show (Exp ELitFalse) = "false"
   show (CReg i) = "%r" ++ show i
   show (PhiPlaceholder i) = "Placeholder " ++ show i
   show (Phi t a) = toLLVMType t ++ " "  ++ printPhiPaths a
   show (Exp e) = show e
   show (CStr i s) = "@s" ++ show i

printPhiPaths x = printPhiPaths' x 
   where
      printPhiPaths' ((cexp, lab):[]) = "[" ++ show cexp ++ ", %label" ++ show lab ++ "]" 
      printPhiPaths' ((cexp, lab):xs) = "[" ++ show cexp ++ ", %label" ++ show lab ++ "], " ++ printPhiPaths' xs 

toLLVMType Int = "i32"
toLLVMType Bool = "i1"
toLLVMType Str = "i8*"
toLLVMType Void = "void"

type JoinState = (Map.Map Label Label, Set.Set Label, CFGraph)

instance Num Expr where                                                    
   (ELitInt i1) + (ELitInt i2) = ELitInt (i1 + i2)                              
   a + b = EAdd a Plus b
   (ELitInt i1) - (ELitInt i2) = ELitInt (i1 - i2)                              
   a - b = EAdd a Minus b
   (ELitInt i1) * (ELitInt i2) = ELitInt (i1 * i2)                              
   a * b = EMul a Times b
   abs (ELitInt i1) = ELitInt (abs i1)                                          
   fromInteger i = ELitInt i                                                    
   signum (ELitInt i1) = ELitInt (signum i1)   

exprDiv :: Expr -> Expr -> Expr                                  
(exprDiv) (ELitInt i1) (ELitInt i2) = ELitInt $ i1 `div` i2
(exprDiv) a b = EMul a Div b

exprMod :: Expr -> Expr -> Expr   
(exprMod) (ELitInt i1) (ELitInt i2) = ELitInt $ i1 `mod` i2
(exprMod) a b = EMul a Mod b

data AllOp = A AddOp | M MulOp | R RelOp

initEnv :: Env
initEnv = Map.empty

createCFGraph (Program defs) = forM defs toCFG 

toCFG (FnDef _ _ args (Block stmts)) = do
   cfg <- execStateT (createBlock stmts (-1)) (CFGraph { blocks = Map.fromList [(0, addFunParams args)], nextL = 1})
   cfg' <- execStateT (setInEdges 1 [-1]) cfg
   return cfg'

addFunParams l =  CFBlock {
   stmts = [],
   jumpTo = JJump (1),
   inEdges = [-1],
   env = Map.fromList $ addParams l [] 0
}

addParams [] l _ = l
addParams ((Arg t id):xs) l n = addParams xs ((id, (CReg n, t)):l) $ n+1

createBlock :: (MonadTrans m, MonadState CFGraph (m IO)) => [Stmt] -> Label ->  m IO Label

createBlock (st@(BStmt (Block s)):ss) n = do
   next' <- createBlock ss n
   next <- createBlock s next'
   return next    
   
createBlock (st@(CondElse e sb1 sb2):ss) l = do
   lcond <- nextLabel
   --call recursively
   next <- createBlock ss l
   bt <- createBlock [sb1] next
   bf <- createBlock [sb2] next
   let (e1, _) = evalExpr e Map.empty
   let bcond = case e1 of
         ELitTrue -> CFBlock {stmts=[], jumpTo=JJump bt, inEdges=[], env=Map.empty}
         ELitFalse -> CFBlock {stmts=[], jumpTo=JJump bf, inEdges=[], env = Map.empty}
         x -> CFBlock {stmts=[], jumpTo=(JCond {true=bt, false=bf, cond=e}), inEdges=[], env = Map.empty}
   g <- get
   let m = blocks g
   let ifmap = Map.fromList [(lcond, bcond)]
   put $ CFGraph{blocks = (Map.union m ifmap), nextL = (nextL g)}
   return lcond    
createBlock (st@(Cond e s):ss) n = do
   lcond <- nextLabel
   --call recursively
   next <- createBlock ss n
   bt <- createBlock [s] next
   let (e1, _) = evalExpr e Map.empty
   let bcond = case e1 of
         ELitTrue -> CFBlock {stmts=[], jumpTo=JJump bt, inEdges=[], env=Map.empty}
         ELitFalse -> CFBlock {stmts=[], jumpTo=JJump next, inEdges=[], env = Map.empty}
         x -> CFBlock {stmts=[], jumpTo=(JCond {true=bt, false=next, cond=e}), inEdges=[], env = Map.empty}
   g <- get
   let m = blocks g
   let ifmap = Map.fromList [(lcond, bcond)]
   put $ CFGraph{blocks = (Map.union m ifmap), nextL = (nextL g)}
   return lcond    
createBlock (st@(While e s):ss) l = do
   lcond <- nextLabel
   next <- createBlock ss l
   body <- createBlock [s] lcond
   let bcond = CFBlock {stmts=[], jumpTo=(JCond {true=body, false=next, cond=e}), inEdges=[], env = Map.empty}
   g <- get
   let m = blocks g
   let ifmap = Map.fromList [(lcond, bcond)]
   put $ CFGraph{blocks = (Map.union m ifmap), nextL = (nextL g)}
   return lcond    
createBlock (s:ss) n = do
   l <- nextLabel
   let (stm, rest, ev') = joinBlockStmts (s:ss) Map.empty
   next <- createBlock rest n
   g <- get
   let m = blocks g
   let b = CFBlock {stmts=stm, jumpTo=(JJump next), inEdges=[], env = ev'}  
   put $ CFGraph{blocks = (Map.insert l b m),  nextL = (nextL g)}
   return l    
createBlock [] l = do
   return l

addNewVarsE t ((NoInit id):is) env stmts = addNewVarsE t is (Map.insert id ((newVar t), t) env) stmts
addNewVarsE t ((Init id (EApp id2 args)):is) env stmts = addNewVarsE t is (Map.insert id ((newVar t), t) env) $ (Ass id (EApp id2 args)):stmts
addNewVarsE t ((Init id e):is) env stmts = addNewVarsE t is (Map.insert id ((Exp exp), t) env) stmts
   where
      (exp, t1) = evalExpr e env
addNewVarsE t [] env stmts = (env, (reverse stmts))

newVar :: Type -> CExpr
newVar Int = Exp $ ELitInt 0
newVar Bool = Exp ELitFalse
newVar Str = Exp $ EString ""

--tu wykonaj to, co mozesz z expressions
evalExpr e@(EVar id) env = case Map.lookup id env of
   Nothing -> (e, Void) --placeholder type, do uzupelnienia w kolejnym obiegu
   Just (Exp x, t) -> (x, t)
   Just phi -> error $ "Should not be phi yet!\n"
evalExpr e@(Neg e1) env = case evalExpr e1 env of
   (ELitInt i, _) -> (ELitInt $ (-1)*i, Int)
   _ -> (e, Int) --czy Void?
evalExpr e@(Not e1) env = case evalExpr e1 env of
   (ELitTrue, _) -> (ELitFalse, Bool)
   (ELitFalse, _) -> (ELitTrue, Bool)
   _ -> (e, Bool)
evalExpr e@(EMul e1 op e2) env = (binaryOperation (M op) x1 x2, t1)
   where
      (x1, t1) = evalExpr e1 env
      (x2, t2) = evalExpr e2 env
evalExpr e@(EAdd e1 op e2) env = (binaryOperation (A op) x1 x2, t1)
   where
      (x1, t1) = evalExpr e1 env
      (x2, t2) = evalExpr e2 env
evalExpr e@(ERel e1 op e2) env = (binaryOperation (R op) x1 x2, t1)
   where
      (x1, t1) = evalExpr e1 env
      (x2, t2) = evalExpr e2 env
evalExpr e@(EAnd e1 e2) env = evalAnd x1 e2 env
   where
      (x1, t1) = evalExpr e1 env
      evalAnd ELitFalse y _ = (ELitFalse, Bool)
      evalAnd ELitTrue y env' = evalExpr e2 env'
      evalAnd x y _ = (EAnd x y, Bool)
evalExpr e@(EOr e1 e2) env = evalOr x1 e2 env
   where
      (x1, t1) = evalExpr e1 env
      evalOr ELitTrue x _ = (ELitTrue, Bool)
      evalOr ELitFalse y env' = evalExpr y env'
      evalOr x y _ = (EOr x y, Bool)
      
evalExpr e@(EString s) _ = (e, Str)
evalExpr e@(EApp id exps) _ = (e, Void)
evalExpr e@(ELitInt i) _ = (e, Int)
evalExpr e@ELitTrue _ = (e, Bool)
evalExpr e@ELitFalse _ =  (e, Bool)


binaryOperation (M Times) x1 x2 = x1*x2
binaryOperation (M Div) x1 x2 = x1 `exprDiv` x2
binaryOperation (M Mod) x1 x2 = x1 `exprMod` x2
binaryOperation (A Plus) x1 x2 =  x1+x2
binaryOperation (A Minus) x1 x2 = x1-x2
binaryOperation (R LTH) (ELitInt x1) (ELitInt x2) = if x1 < x2 then ELitTrue else ELitFalse
binaryOperation (R LE) (ELitInt x1) (ELitInt x2) = if x1 <= x2 then ELitTrue else ELitFalse
binaryOperation (R GTH) (ELitInt x1) (ELitInt x2) = if x1 > x2 then ELitTrue else ELitFalse
binaryOperation (R GE) (ELitInt x1) (ELitInt x2) = if x1 >= x2 then ELitTrue else ELitFalse
binaryOperation (R EQU) (ELitInt x1) (ELitInt x2) = if x1 == x2 then ELitTrue else ELitFalse
binaryOperation (R NE) (ELitInt x1) (ELitInt x2) = if x1 /= x2 then ELitTrue else ELitFalse
binaryOperation (R EQU) (ELitTrue) (ELitTrue) = ELitTrue
binaryOperation (R EQU) (ELitTrue) (ELitFalse) = ELitFalse
binaryOperation (R EQU) (ELitFalse) (ELitTrue) = ELitFalse
binaryOperation (R EQU) (ELitFalse) (ELitFalse) = ELitTrue
binaryOperation (R NE) (ELitTrue) (ELitTrue) = ELitFalse
binaryOperation (R NE) (ELitTrue) (ELitFalse) = ELitTrue
binaryOperation (R NE) (ELitFalse) (ELitTrue) = ELitTrue
binaryOperation (R NE) (ELitFalse) (ELitFalse) = ELitFalse
binaryOperation (R op) x1 x2 = ERel x1 op x2

--function which joins all non-conditional statements into one block
joinBlockStmts s ev = joinBS [] s ev
   where
      joinBS acc (s@(Decl t items):ss) ev = joinBS (s' ++ acc) ss ev' where
         (ev', s') = addNewVarsE t items ev []
      joinBS acc rest@((Cond e s):ss) ev = (reverse acc, rest, ev)
      joinBS acc rest@((CondElse e s1 s2):ss) ev = (reverse acc, rest, ev)
      joinBS acc rest@((While e s):ss) ev = (reverse acc, rest, ev)
      joinBS acc rest@((BStmt b):ss) ev = (reverse acc, rest, ev)
      joinBS acc (VRet:ss) ev = (reverse $VRet:acc, [], ev)
      joinBS acc (st@(Ret e):ss) ev = (reverse $ st:acc, [], ev)
      joinBS acc (s@(Ass id (EApp id2 args)):ss) ev = joinBS (s:acc) ss ev
      joinBS acc (s@(Ass id e):ss) ev = joinBS (s:acc) ss ev
      joinBS acc ((Incr id):ss) ev = joinBS ((Ass id (EAdd (EVar id) Plus (ELitInt 1))):acc) ss ev
      joinBS acc ((Decr id):ss) ev = joinBS ((Ass id (EAdd (EVar id) Minus (ELitInt 1))):acc) ss ev
      joinBS acc (s:ss) ev = joinBS (s:acc) ss ev
      joinBS acc [] ev = (reverse acc, [], ev)

{-
transformLazyExpr (EAnd e1 e2) vid  = do
   (c1, v1, vid1) <- transformLazyExpr' e1 (vid+1)
   (c2, v2, vid2) <- transformLazyExpr' e2 (vid+1)
   [(Ass (Ident "tmp" ++ show vid) (t))
   where
      transformLazyExpr' e@(EAnd e1' e2') v = transformLazyExpr e v
      transformLazyExpr' e@(EOr e1' e2') v = transformLazyExpr e v
      transformLazyExpr' exp v = [(Ass (Ident "tmp" ++ show v) exp)]

applyLazyRules nr vid =  
-}

updateInEdges :: CFBlock -> [Label] -> CFBlock
updateInEdges b l = CFBlock {
   stmts = stmts b,
   jumpTo = jumpTo b,
   inEdges = l ++ (inEdges b),
   env = env b
}

setInEdges nr ins = do
   g <- get
   let m = blocks g
   case Map.lookup nr m of
      Nothing -> return ()
      Just bl -> do
         if visited ins (inEdges bl) then return ()
         else do
            put $ CFGraph { blocks = Map.insert nr (updateInEdges bl ins) m, nextL = nextL g}
            case jumpTo bl of
               JJump l -> setInEdges l [nr]
               cond -> do
                  setInEdges (true cond) [nr]
                  setInEdges (false cond) [nr]
   where 
      visited [] l2 = False
      visited (x:xs) l2 = if any (==x) l2 then True else visited xs l2  

nextLabel :: (MonadTrans m, MonadState CFGraph (m IO)) => m IO Label
nextLabel = do
   g <- get
   let l = nextL g
   put $ CFGraph{blocks = (blocks g),  nextL = (l + 1)}
   return l    

newLoc :: Env -> Loc
newLoc e = Map.size e
