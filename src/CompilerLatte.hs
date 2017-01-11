{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module CompilerLatte where
                        
import AbsLatte
import PrintLatte
import qualified Data.Map as Map
import qualified Data.Set as Set 
import Control.Monad.State      
import Data.Maybe  
import CFGraph

type Register = Int
data LLVMCode = 
   PredefinedFuncs |
   Add CExpr CExpr CExpr | 
   Sub CExpr CExpr CExpr | 
   Mul CExpr CExpr CExpr |
   Xor CExpr CExpr CExpr | 
   SDiv CExpr CExpr CExpr | 
   SRem CExpr CExpr CExpr |
   FunCall CExpr Type Ident [(Type, CExpr)] | 
   Icmp AllOp Type CExpr CExpr CExpr |
   And Type CExpr CExpr CExpr | 
   Or Type CExpr CExpr CExpr |
   Br Int |
   BrCond CExpr Int Int |
   FunDef String | 
   FunEnd | 
   Label Int | 
   CPhi CExpr CExpr | 
   RetVoid | 
   RetT Type CExpr |
   Comment String |
   StrConst CExpr Int String |
   Alloca CExpr Type |
   StoreStr Type Int String CExpr |
   Bitcast CExpr Int CExpr |
   Store Type CExpr CExpr |
   Load Type CExpr CExpr |
   StayInPlace LLVMCode

type VEnv = Map.Map Ident CExpr
type FEnv = Map.Map Ident (Type, [Type])
type Phis = Map.Map Int Ident
type CState = (EEnv, CFGraph, (Map.Map Label [LLVMCode]), Register, FEnv, Phis, SEnv, OEnv)
type SEnv = Map.Map String Int
type OEnv = Map.Map Int Object

data Object = ArCh [Char]

instance Show AllOp where
   show (R LTH) = "slt"
   show (R LE) = "sle"
   show (R GTH) = "sgt"
   show (R GE) = "sge"
   show (R EQU) = "eq"
   show (R NE) = "ne"

instance Show LLVMCode where
   show (Add r1 r2 r3) = "\t" ++ show r1 ++ " = add i32 " ++ show r2 ++ ", " ++ show r3 ++ "\n"
   show (Sub r1 r2 r3) = "\t" ++ show r1 ++ " = sub i32 " ++ show r2 ++ ", " ++ show r3 ++ "\n"
   show (Mul r1 r2 r3) = "\t" ++ show r1 ++ " = mul i32 " ++ show r2 ++ ", " ++ show r3 ++ "\n"
   show (Xor r1 r2 r3) = "\t" ++ show r1 ++ " = xor i1 " ++ show r2 ++ ", " ++ show r3 ++ "\n"
   show (SDiv r1 r2 r3) = "\t" ++ show r1 ++ " = sdiv i32 " ++ show r2 ++ ", " ++ show r3 ++ "\n"
   show (SRem r1 r2 r3) = "\t" ++ show r1 ++ " = srem i32 " ++ show r2 ++ ", " ++ show r3 ++ "\n"
   show (Icmp op t r1 r2 r3) = "\t" ++ show r1 ++ " = icmp " ++ show op ++ " " ++ toLLVMType t ++ " " ++ show r2 ++ ", " ++ show r3 ++ "\n" 
   show (And t r1 r2 r3) = "\t" ++ show r1 ++ " = and " ++ toLLVMType t ++ " " ++ show r2 ++ ", " ++ show r3 ++ "\n"
   show (Or t r1 r2 r3) = "\t" ++ show r1 ++ " = or " ++ toLLVMType t ++ " " ++ show r2 ++ ", " ++ show r3 ++ "\n"
   show (FunCall r1 Void (Ident s) args) = "\tcall void @" ++ s ++ "(" ++ printArgs args ++ ")\t;unused register: " ++ show r1 ++ "\n"    
   show (FunCall r1 rt (Ident s) args) = "\t" ++ show r1 ++ " = call " ++ toLLVMType rt ++ " @" ++ s ++ "(" ++ printArgs args ++ ")\n"    
   show (Br i) = "\tbr label %label" ++ show i ++ "\n"
   show (BrCond r i1 i2) = "\tbr i1 " ++ show r ++ ", label %label" ++ show i1 ++ ", label %label" ++ show i2 ++ "\n"
   show (FunDef s) = s
   show (FunEnd) = "}\n"
   show (CPhi r e) = "\t" ++ show r ++ " = phi " ++ show e ++ "\n"
   show (Label nr) = "label" ++ show nr ++ ":\n"
   show RetVoid = "\tret void\n"
   show (RetT t p) = "\tret " ++ toLLVMType t ++ " " ++ show p ++ "\n"
   show (Comment s) = "\t;" ++ s ++ "\n"
   show (StrConst r l s) = show r ++ " = private constant [" ++ show l ++ " x i8] c\"" ++ (replaceNL s) ++ "\\00\"\n"
   show (Alloca r t) = "\t" ++ show r ++ " = alloca " ++ toLLVMType t ++ "\n"
   show (StoreStr t l from to) = "\tstore " ++ toLLVMType t ++ " getelementptr inbounds ([" ++ show l ++ " x i8], [" ++ show l ++ " x i8]* \"" ++ from ++ "\", i32 0, i32 0), " ++ toLLVMType t ++ "* " ++ show to ++ "\n" 
   show (Bitcast r l str) = "\t" ++ show r ++ " = bitcast [" ++ show l ++ " x i8]* " ++ show str ++ " to i8*\n"
   show PredefinedFuncs = "declare void @printInt(i32)\ndeclare void @printString(i8*)\ndeclare void @error()\ndeclare i32 @readInt()\ndeclare i8* @readString()\ndeclare i8* @concat(i8*, i8*)\n"
   show (Store t r1 r2) = "\t store " ++ toLLVMType t ++ " " ++ show r1 ++ ", " ++ toLLVMType t ++ "* " ++ show r2 ++ "\n"
   show (Load t r1 r2) = "\t" ++ show r1 ++ " = load " ++ toLLVMType t ++ ", " ++ toLLVMType t ++ "* " ++ show r2 ++ "\n"
   show (StayInPlace l) = show l
   
replaceNL s = replace s [] where
   replace ('\n':xs) l = replace xs (l ++ "\\0A")
   replace (x:xs) l = replace xs (l ++ [x])
   replace [] l = l

--Types env: just function parameters
initCState :: [Arg] -> CFGraph -> Register -> Map.Map Ident (Type, [Type]) -> SEnv -> CState
initCState args cfg r fenv senv = (m args, cfg, Map.empty, r, fenv, Map.empty, senv, Map.empty)
   where
      m args = m' args [] 0
      m' [] l n = Map.fromList l
      m' ((Arg t id):xs) l n = m' xs ((id, ((CReg n), t)):l) (n+1)

myMapValues :: Map.Map a b -> [b]
myMapValues m = map (\(k,v) -> v) $ Map.assocs m

printArgs [] = ""
printArgs ((t, p):[]) = toLLVMType t ++ " " ++ show p
printArgs ((t, p):xs) = toLLVMType t ++ " " ++ show p ++ ", " ++ printArgs xs

addFunDef l (FnDef t id@(Ident s) args _) = (l ++ [FunDef $ "define " ++ toLLVMType t ++ " @" ++ s ++ "(" ++ (fst getArgs) ++ ") {\n"], snd $getArgs)
   where
      getArgs = getA args "" 0
      getA [] l n = (l, n)
      getA ((Arg t (Ident s)):[]) l n = (l ++ (toLLVMType t ++ " %r" ++ show n), n+1)
      getA ((Arg t (Ident s)):xs) l n = getA xs (l ++ toLLVMType t ++ " %r" ++ show n ++ ", ") (n+1)


getGlobalStrings :: SEnv -> [LLVMCode]
getGlobalStrings senv = map (\(s', i) -> (StrConst (CStr i s') (length s' + 1) s')) (Map.assocs senv)

compileProg p@(Program defs) fenv = compileProg' defs Map.empty 0 fenv Map.empty
   where
      compileProg' [] l _ _ senv = return $ [PredefinedFuncs] ++ (getGlobalStrings senv) ++ (foldr (\m li -> li ++ ((foldr (++) [] $ map moveCode $ Map.elems m))) [] (Map.elems l))
      compileProg' (def@(FnDef t id@(Ident s) args b):cs) l r fenv senvInit = do
         c <- createCFGraph $ Program [def]
         let (l', r') = addFunDef [] def
         (_, _, code, reg, _, p, senv, o) <- execStateT (compileCFG 0) (initCState args (head c) (r') fenv senvInit)
         let prolog = Map.adjust (l' ++) 0 code
         let epilog = Map.insert (nextL $ head c) [FunEnd] prolog
         compileProg' cs (Map.insert id epilog l) 0 fenv senv
      
--replaces phi placeholders
replacePhi :: (MonadTrans m, MonadState CState (m IO)) => m IO ()
replacePhi = do
   (v, cfg, code, r, f, phis, senv, oenv) <- get
   c <-  mapM replace (Map.assocs code) 
   let code' = Map.fromList c
   put (v, cfg, code', r, f, phis, senv, oenv)
   where
      replace :: (MonadTrans m, MonadState CState (m IO)) => (Label, [LLVMCode]) -> m IO (Label, [LLVMCode])
      replace (l, code) = do
         code' <- mapM replaceP code
         return (l, code')
      replaceP :: (MonadTrans m, MonadState CState (m IO)) => LLVMCode -> m IO LLVMCode
      replaceP p@(CPhi reg phi@(Phi t args)) = do
         args' <- replace' args []
         return $ CPhi reg $ Phi t args'
         where 
            replace' :: (MonadTrans m, MonadState CState (m IO)) => [(CExpr, Label)] -> [(CExpr, Label)] -> m IO [(CExpr, Label)] 
            replace' (((PhiPlaceholder i), nr):xs) l = do
               (v, cfg, c, r, f, p, s, o) <- get
               let m = blocks cfg
               let bl = fromMaybe (error $ "No such block: " ++ show nr) $ Map.lookup nr m
               let id = fromMaybe (error $ "No such placeholder " ++ show i) $ Map.lookup i p
               let (reg, _) = case Map.lookup id (env bl) of
                     Just x -> x
                     Nothing -> fromMaybe (error $ "No such var in global env: " ++ show nr ++ "id: " ++ show id) $ Map.lookup id v
               replace' xs $ l ++ [(reg, nr)]
            replace' (x:xs) l = replace' xs $ l ++ [x]
            replace' [] l = return l
      replaceP e = do
         return e


moveCode :: [LLVMCode] -> [LLVMCode]
moveCode l = moveCode' l [] [] [] [] [] where
   moveCode' (c@(Label i):all) def label phis jump rest = moveCode' all def (c:label) phis jump rest
   moveCode' (f@(FunDef s):all) def label phis jump rest = moveCode' all (f:def) label phis jump rest
   moveCode' (p@(CPhi t args):all) def label phis jump rest = moveCode' all def label (p:phis) jump rest
   moveCode' ((Br i):all) def label phis jump rest = moveCode' all def label phis ((Br i):jump) rest
   moveCode' (j@(BrCond r i1 i2):all) def label phis jump rest = moveCode' all def label phis (j:jump) rest
   moveCode' (a:all) def label phis jump rest = moveCode' all def label phis jump (a:rest)
   moveCode' [] def label phis jump rest = def ++ label ++ (reverse phis) ++ (reverse rest) ++ jump

--goes over a signle CFGraph using bfs
compileCFG :: (MonadTrans m, MonadState CState (m IO)) => Label -> m IO ()
compileCFG nr = bfs [nr] Set.empty >> replacePhi
   where
      bfs (x:xs) visited = do
         (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
         let m = blocks cfg
         case Map.lookup x m of
            Nothing -> bfs xs (Set.insert x visited)
            Just bl -> if Set.notMember x visited then do
               compileCFGBlock x
               let visited' = Set.insert x visited
               bfs (addToQueue xs visited' $ jumpTo bl) visited'
               else bfs xs visited
      bfs [] _ = return ()
      addToQueue l visited (JJump nr) = if Set.notMember nr visited then l ++ [nr] else l
      addToQueue l visited jmp = addToQueue (addToQueue l visited (JJump (true jmp))) visited (JJump (false jmp))

compileCFGBlock (-1) = return ()
compileCFGBlock nr = do
   printLabel nr
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   let m = blocks cfg
   let bl = m Map.! nr
   let s = stmts bl
   let ve = env bl
   let (venv', senv') = addVarTypes venv senv $ Map.assocs ve
   put (venv', cfg, code, reg, fenv, phis, senv', oenv)
   forM s (compileStmt' nr)
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   let m = blocks cfg
   let bl = m Map.! nr
   _ <- checkJump $ jumpTo bl
   case jumpTo bl of
      JJump (-1) -> return ()
      JJump x -> printCode nr $ Br x
      jmp -> do
         printComment (SExp (cond jmp)) nr
         r <- compileExpr nr $ cond jmp
         printComment (SExp (cond jmp)) nr
         printCode nr $ BrCond r (true jmp) (false jmp)  
   return ()
   where
      compileStmt' n s = printComment s n >> compileStmt n s
      --insert variables with known type to global type environment
      addVarTypes :: EEnv -> SEnv -> [(Ident, (CExpr, Type))] -> (EEnv, SEnv)
      addVarTypes ma ms [] = (ma, ms)
      addVarTypes ma ms ((id, (e, Void)):xs) = addVarTypes ma ms xs
      addVarTypes ma ms ((id, (e, t)):xs) = if (Map.member id ma && (t == snd (ma Map.! id))) then addVarTypes ma ms xs
         else addVarTypes (Map.insert id (e,t) ma) ms xs

checkJump j@(JJump n) = return j
checkJump j = if (true j == -1 || false j == -1) then error $ "Unallowed jump!" else return j

--TODO: pierwszy blok- do niego nie moze byc jumpow
compileStmt :: (MonadTrans m, MonadState CState (m IO)) => Label -> Stmt -> m IO ()
compileStmt nr s@(SExp e1) = compileExpr nr e1 >> return ()
compileStmt nr VRet = printRet nr Nothing >> return ()
compileStmt nr s@(Ret e1) = do
   r <- compileExpr nr e1
   (_, t) <- getType nr e1
   printRet nr $ Just (t, r)
   return ()
compileStmt nr s@(Ass id e) = do
   r <- compileExpr nr e
   (_, t) <- getType nr e
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   let m = blocks cfg
   let bl = m Map.! nr
   let bl' = CFBlock {
      stmts=stmts bl,
      jumpTo=jumpTo bl,
      inEdges=inEdges bl,
      env=Map.insert id (r, t) $ env bl
   }
   let cfg' = CFGraph {
      blocks=Map.insert nr bl' m,
      nextL=nextL cfg
   }
   put (Map.insert id (r, t) venv, cfg', code, reg, fenv, phis, senv, oenv)

compileStmt nr s = return ()


findValue :: (MonadTrans m, MonadState CState (m IO)) => Ident -> Label -> m IO [CExpr]
findValue id (-1) = do 
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   let m = blocks cfg
   let bl = m Map.! (0)
   let ve = env bl
   case Map.lookup id ve of
      Just (Exp e, _) -> do
         x1 <- compileExpr 0 e
         return [x1]
      Just (e, _) -> return [e]
      Nothing -> error $ "Variable '" ++ show id ++ "' not found!\n"
findValue id nr = do
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   let m = blocks cfg
   let bl = m Map.! nr
   let ve = env bl
   (_, t) <- getType nr $ EVar id
   case Map.lookup id ve of
      Just (Exp e, _) -> do
         x1 <- compileExpr nr e
         return [x1]
      Just (e, _) -> return [e]
      Nothing -> do
         exps <- mapM (findValue id) $ inEdges bl
         let exps' = foldr (++) [] exps
         if length exps' == 1 then return exps'
         else do
            let args = zip exps' $ inEdges bl
            (v, c, cd, r, f, ph, sv, ov) <- get
            put (v, c, Map.adjust (++ [(CPhi (CReg r) (Phi t args))]) nr cd, (r+1),f, ph, sv, ov)
            return $ [CReg r] 

compileExpr :: (MonadTrans m, MonadState CState (m IO)) => Label -> Expr -> m IO CExpr
compileExpr nr e@(EVar id) = do
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   let m = blocks cfg
   let bl = m Map.! nr
   let ve = env bl
   case Map.lookup id ve of
      Just (e, t) -> case e of
         Exp e1 -> compileExpr nr e1
         CStr n s -> do
            return e
         _ -> do
            return e
      Nothing ->
         do
            (_, t) <- getType nr e
            let pnr = Map.size phis
            let b = CFBlock{stmts=stmts bl, jumpTo=jumpTo bl, inEdges=inEdges bl, env=Map.insert id ((PhiPlaceholder pnr), t) ve}
            let cfg' = CFGraph{blocks=Map.insert nr b m, nextL=nextL cfg}
            put (Map.insert id (PhiPlaceholder pnr, t) venv, cfg', code, reg, fenv, Map.insert pnr id phis, senv, oenv)
            exps <- mapM (findValue id) (inEdges b)
            let exps'' = map (\[x] -> x) exps
            let rawZipped = zip (inEdges b) exps''
            let rawMap = Map.fromList rawZipped
            let exps' = if (all (== head exps'') exps'') == True then [head exps''] else exps''
            if length exps' == 1 then do
               h <- case (head exps') of
                  Exp es@(EString s) -> compileExpr nr es 
                  x -> return x
               (v, c, cd, r, f, ph, sv, ov) <- get
               let m' = blocks c
               let bl' = m' Map.! nr
               let b' = CFBlock{
                  stmts=stmts bl',
                  jumpTo=jumpTo bl', 
                  inEdges=inEdges bl', 
                  env=Map.insert id (h, t) (env bl')
               }
               let c' = CFGraph{
                  blocks=Map.insert nr b' m',
                  nextL=nextL cfg
               }
               put (Map.insert id (h, t) v, c', cd, r, f, ph, sv, ov)
               return $ h
            else do
            --jak dostaje >1 rzecz to robie phi
               let args = zip exps' $ inEdges b
               (v, c, cd, r, f, phis, sv, ov)<- get
               let m' = blocks c
               let bl' = m' Map.! nr
               let b' = CFBlock{
                  stmts=stmts bl',
                  jumpTo=jumpTo bl', 
                  inEdges=inEdges bl', 
                  env=Map.insert id ((CReg r), t) (env bl')
               }
               let c' = CFGraph{
                  blocks=Map.insert nr b' m',
                  nextL=nextL cfg
               }

               put (Map.insert id ((CReg r), t) v, c', Map.adjust (++ [(CPhi (CReg r) (Phi t args))]) nr cd, (r+1), f, phis, sv, ov)
               return $ CReg r
compileExpr _ e@(ELitInt i) = return $ Exp e
compileExpr _ e@ELitTrue = return $ Exp e
compileExpr _ e@ELitFalse = return $ Exp e
compileExpr nr (EApp id exprs) = do
   m <- mapM (compileExpr nr) exprs
   printFunCall id m nr
compileExpr nr (EString s) = do
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   case Map.lookup s senv of
      Nothing -> do
         let next = Map.size senv
         r@(CReg i) <- newReg
         put (venv, cfg, code, (i+1), fenv, phis, Map.insert s next senv, Map.insert i (ArCh s) oenv)
         let l = length s + 1
         printCode nr (Bitcast r l (CStr next s))
         return r
      Just n -> do
         r@(CReg i) <- newReg
         put (venv, cfg, code, (i+1), fenv, phis, senv, Map.insert i (ArCh s) oenv)
         let l = length s + 1
         printCode nr (Bitcast r l (CStr n s))
         return r
compileExpr nr (Neg e) = do
   x <- compileExpr nr e
   case x of
      CReg r -> do
         nreg <- newReg
         printCode nr (Mul nreg (CReg r) (Exp (ELitInt (-1))))
         return nreg
      (Exp (ELitInt i)) -> return $ Exp $ ELitInt $ (-1)*i 
compileExpr nr (Not e) = do
   x <- compileExpr nr e
   case x of
      CReg r -> do
         nreg <- newReg
         printCode nr (Xor nreg (CReg r) (Exp (ELitTrue)))
         return nreg
      (Exp ELitTrue) -> return $ Exp ELitFalse
      (Exp ELitFalse) -> return $ Exp ELitTrue
compileExpr nr (EMul e1 op e2) = do
   x1 <- compileExpr nr e1
   x2 <- compileExpr nr e2
   r <- newReg
   printBinaryOperation (M op) Int r x1 x2 nr
   return r
compileExpr nr (EAdd e1 op e2) = do
   x1 <- compileExpr nr e1
   x2 <- compileExpr nr e2
   (_, t) <- getType nr e1 
   r <- newReg
   printBinaryOperation (A op) t r x1 x2 nr
   return r
compileExpr nr (ERel e1 op e2) = do
   x1 <- compileExpr nr e1
   x2 <- compileExpr nr e2
   r <- newReg
   (_, t) <- getType nr e1
   printBinaryOperation (R op) t r x1 x2 nr
   return r
compileExpr nr (EAnd e1 e2) = do
   x1 <- compileExpr nr e1
   --dodaj porownanie czy prawda, jezeli prawda skocz to bloku, w ktorym jest samo drugie
   --z tego drugiego bloku na r przypisz wynik
   ra <- newReg
   r1 <- newReg
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   let ifTrue = nextL cfg
   let ifFalse = ifTrue + 1
   put (venv, CFGraph { blocks= blocks cfg, nextL = ifFalse + 1}, code, reg, fenv, phis, senv, oenv)
   printCode nr (Alloca ra Bool)
   printCode nr (Store Bool x1 ra)
   printCode nr (Icmp (R EQU) Bool r1 x1 (Exp ELitTrue))
   printCode nr (StayInPlace $ BrCond r1 ifTrue ifFalse) --do ifTrue skocz, gdy e1 == true do ifFalse skocz, gdy e1 == False 
   printCode nr (StayInPlace $ Label ifTrue)
   x2 <- compileExpr nr e2
   printCode nr (Store Bool x2 ra)
   printCode nr (StayInPlace $ Br ifFalse)
   printCode nr (StayInPlace $ Label ifFalse)
   r <- newReg
  -- (venv, cfg', code, reg, fenv, phis, senv, oenv) <- get
 --  let latest = if (nextL cfg' == (ifFalse + 1)) then (ifTrue - 1)  else (ifTrue ) 
 --  printCode nr (StayInPlace $ CPhi r (Phi Bool [(x2, ifTrue), (Exp ELitFalse, (latest))]))
   printCode nr (Load Bool r ra)
   return r
compileExpr nr (EOr e1 e2) = do
   x1 <- compileExpr nr e1
   ra <- newReg
   r1 <- newReg
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   let ifFalse = nextL cfg
   let ifTrue = ifFalse + 1
   put (venv, CFGraph { blocks= blocks cfg, nextL = ifTrue + 1}, code, reg, fenv, phis, senv, oenv)
   printCode nr (Icmp (R EQU) Bool r1 x1 (Exp ELitTrue))
   printCode nr (Alloca ra Bool)
   printCode nr (Store Bool x1 ra)
   printCode nr (StayInPlace $ BrCond r1 ifTrue ifFalse) --do ifTrue skocz, gdy e1 == true do ifFalse skocz, gdy e1 == False 
   printCode nr (StayInPlace $ Label ifFalse)
   x2 <- compileExpr nr e2
   printCode nr (Store Bool x2 ra)
   printCode nr (StayInPlace $ Br ifTrue)
   printCode nr (StayInPlace $ Label ifTrue)
   r <- newReg
--   (venv, cfg', code, reg, fenv, phis, senv, oenv) <- get
   --let latest = if (nextL cfg' == (ifFalse + 1)) then (ifTrue - 1)  else (ifTrue ) 
--   printCode nr (StayInPlace $ CPhi r (Phi Bool [(x2, ifFalse), (Exp ELitTrue, (nr))]))
   printCode nr (Load Bool r ra)
   return r

--produceAndBlock nr exp true false = do
   
   

getType :: (MonadTrans m, MonadState CState (m IO)) => Label -> Expr -> m IO (CExpr, Type)
getType nr (EVar id) = do
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   let m = blocks cfg
   let bl = m Map.! nr
   let ve = env bl
   case Map.lookup id ve of
      Nothing -> do
         --rekurencyjnie do gory
         case Map.lookup id venv of
            Just (e, t) -> do
               return (e, t) 
            Nothing -> do
               let ins = inEdges bl
               typs <- mapM (flip getType (EVar id)) ins
               let (e, t) = head typs
               (venv', cfg', code', reg', fenv', phis', senv', oenv') <- get
               put (Map.insert id (e, t) venv', cfg', code', reg', fenv', phis', senv', oenv') 
               return (e, t)
      Just (e, Void) -> do
         let ins = inEdges bl
         typs <- mapM (flip getType (EVar id)) ins
         let (e, t) = head  typs
         (venv', cfg', code', reg', fenv', phis', senv', oenv') <- get
         let m' = blocks cfg'
         let bl' = m' Map.! nr
         let ve' = env bl'
         let b = CFBlock {
            stmts=stmts bl',
            jumpTo=jumpTo bl',
            inEdges=inEdges bl',
            env=Map.insert id (e, t) (env bl')
         }
         let cfg'' = CFGraph {
            blocks=Map.insert nr b m',
            nextL=nextL cfg'
         }
         put (Map.insert id (e, t) venv', cfg'', code', reg', fenv', phis', senv', oenv')
         return (e, t)
      Just (e, typ) -> do
         return (e, typ) 
getType _ e@(ELitInt _) = return (Exp e, Int)
getType _ e@(ELitTrue) = return (Exp e, Bool)
getType _ e@(ELitFalse) = return (Exp e, Bool)
getType _ e@(EApp id _) = do
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   let (t, _) = fenv Map.! id
   return (Exp e, t)
getType _ e@(EString _) = return (Exp e, Str)
getType _ e@(Neg _) = return (Exp e, Int)
getType _ e@(Not _) = return (Exp e, Bool)
getType _ e@(EMul _ _ _) = return (Exp e, Int)
getType nr (EAdd e1 Plus e2) = getType nr e1
getType _ e@(EAdd _ Minus _) = return (Exp e, Int)
getType nr (ERel e1 EQU e2) = getType nr e1
getType nr (ERel e1 NE e2) = getType nr e1
getType _ e@(ERel _ _ _) = return (Exp e, Bool)
getType _ e@(EAnd _ _) = return (Exp e, Bool)
getType _ e@(EOr _ _) = return (Exp e, Bool) 

getStrLen str@(CStr i s) _ = do
   r <- newReg
   let l = length s + 1
   return (length s, s, [(Bitcast r l str)], r)
getStrLen r@(CReg i) oenv = return (length s + 1, s, [], r) where
   ArCh s = case Map.lookup i oenv of
      Nothing -> error $ "No such string " ++ show i
      Just x -> x 
getStrLen e@(Phi t args) oenv = do   
   r <- newReg
   return (0, "", [], r) 

{--PRINTING FUNCTIONS--}

printBinaryOperation (A Plus) Str r@(CReg i) x1 x2 nr = do
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   (n1, s1, c1, r1) <- getStrLen x1 oenv
   (n2, s2, c2, r2) <- getStrLen x2 oenv
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   put (venv, cfg, Map.adjust (++ c1 ++ c2 ++ [(FunCall r Str (Ident "concat") [(Str, r1), (Str, r2)])]) nr code, reg, fenv, phis, senv, Map.insert i (ArCh $ s1 ++ s2) oenv)
printBinaryOperation (A Plus) t r x1 x2 nr = modify $ \(venv, cfg, code, reg, fenv, phis, senv, oenv) -> (venv, cfg, Map.adjust (++ [(Add r x1 x2)]) nr code, reg, fenv, phis, senv, oenv)
printBinaryOperation (A Minus) t r x1 x2 nr = modify $ \(venv, cfg, code, reg, fenv, phis, senv, oenv) -> (venv, cfg, Map.adjust (++ [(Sub r x1 x2)]) nr code, reg, fenv, phis, senv, oenv)
printBinaryOperation (M Times) t r x1 x2 nr = modify $ \(venv, cfg, code, reg, fenv, phis, senv, oenv) -> (venv, cfg, Map.adjust (++ [(Mul r x1 x2)]) nr code, reg, fenv, phis, senv, oenv)
printBinaryOperation (M Div) t r x1 x2 nr = modify $ \(venv, cfg, code, reg, fenv, phis, senv, oenv) -> (venv, cfg, Map.adjust (++ [(SDiv r x1 x2)]) nr code, reg, fenv, phis, senv, oenv)
printBinaryOperation (M Mod) t r x1 x2 nr = modify $ \(venv, cfg, code, reg, fenv, phis, senv, oenv) -> (venv, cfg, Map.adjust (++ [(SRem r x1 x2)]) nr code, reg, fenv, phis, senv, oenv)
printBinaryOperation relOp t r x1 x2 nr = modify $ \(venv, cfg, code, reg, fenv, phis, senv, oenv) -> (venv, cfg, Map.adjust (++ [(Icmp relOp t r x1 x2)]) nr code, reg, fenv, phis, senv, oenv)

printCode :: (MonadTrans m, MonadState CState (m IO)) => Label -> LLVMCode -> m IO ()
printCode nr c = modify $ \(venv, cfg, code, reg, fenv, phis, senv, oenv) -> (venv, cfg, Map.adjust (++ [c]) nr code, reg, fenv, phis, senv, oenv)

printLabel :: (MonadTrans m, MonadState CState (m IO)) => Label -> m IO ()
printLabel nr = modify $ \(venv, cfg, code, reg, fenv, phis, senv, oenv) -> (venv, cfg, Map.insert nr ([Label nr]) code, reg, fenv, phis, senv, oenv)

printRet :: (MonadTrans m, MonadState CState (m IO)) => Label -> Maybe (Type, CExpr) -> m IO ()
printRet nr x = do
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   let m = blocks cfg
   let bl = m Map.! nr
   let b = CFBlock {
      stmts=stmts bl,
      jumpTo=JJump (-1),
      inEdges=inEdges bl,
      env=env bl
   }
   let cfg' = CFGraph {
      blocks=Map.insert nr b m,
      nextL=nextL cfg
   }
   case x of
      Nothing -> put (venv, cfg', Map.adjust (++ [RetVoid]) nr code, reg, fenv, phis, senv, oenv)
      Just (t, p) -> put (venv, cfg', Map.adjust (++ [RetT t p]) nr code, reg, fenv, phis, senv, oenv)

--dodaj wszystkie funkcje do state
--znajdz typ zwracany przez funkcje
--ladnie wypisz parametry
printFunCall :: (MonadTrans m, MonadState CState (m IO)) => Ident -> [CExpr] -> Label -> m IO CExpr
printFunCall id m nr = do
   r <- newReg
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   let (t, types) = fenv Map.! id
   let code' = Map.adjust (++ [(FunCall r t id $ zip types m)]) nr code
   put (venv, cfg, code', reg, fenv, phis, senv, oenv)  
   return r

printComment :: (MonadTrans m, MonadState CState (m IO)) => Stmt -> Label -> m IO ()
printComment s nr =  printCode nr (Comment $ filter (/= ';') $ filter (/= '\n') $ printTree s) 

newReg :: (MonadTrans m, MonadState CState (m IO)) => m IO CExpr
newReg = do
   (venv, cfg, code, reg, fenv, phis, senv, oenv) <- get
   put (venv, cfg, code, (reg+1), fenv, phis, senv, oenv)
   return $ CReg reg
