import Control.Monad (zipWithM)
import Control.Monad.State
    ( evalState, MonadState(put, get), State )

type VarLabel = String
type PropLabel = String -- プロパティ名を表現
type PlaceholderLabel = String -- プレースホルダー名
type PlaceholderCounter = State Int

newtype Program
  = Program [Statement]       -- Program型は文のリスト
  deriving (Show, Eq)

data Statement
  = VarDecl VarLabel Exp       -- 変数宣言
  | Assign Lhs Exp             -- 代入
  deriving (Show, Eq)

data Lhs
  = Var VarLabel               -- 単純な変数
  | ObjAccess Lhs PropLabel    -- プロパティアクセス
  | PlaceholderLhs PlaceholderLabel -- プレースホルダー
  deriving (Show, Eq)

data Exp
  = Num Int                    -- 数値
  | VarExp VarLabel            -- 変数
  | New String                 -- コンストラクタ呼び出し
  | This                       -- thisキーワード
  | LhsExp Lhs                 -- Lhsを式として扱う
  | PlaceholderExp PlaceholderLabel -- プレースホルダー
  deriving (Show, Eq)

-- プレースホルダー生成
nextPlaceholder :: PlaceholderCounter String
nextPlaceholder = do
  n <- get
  put (n + 1)
  return ("Placeholder" ++ show n)

-- Lhsの共通部分
commonLhs :: Lhs -> Lhs -> PlaceholderCounter Lhs
commonLhs lhs1 lhs2
  | lhs1 == lhs2 = return lhs1
commonLhs (ObjAccess lhs1 prop1) (ObjAccess lhs2 prop2)
  | prop1 == prop2 = ObjAccess <$> commonLhs lhs1 lhs2 <*> pure prop1
commonLhs _ _ = PlaceholderLhs <$> nextPlaceholder

-- Expの共通部分
commonExp :: Exp -> Exp -> PlaceholderCounter Exp
commonExp exp1 exp2
  | exp1 == exp2 = return exp1
commonExp (LhsExp lhs1) (LhsExp lhs2) = LhsExp <$> commonLhs lhs1 lhs2
commonExp _ _ = PlaceholderExp <$> nextPlaceholder

commonProgram :: Program -> Program -> PlaceholderCounter Program
commonProgram (Program stmts1) (Program stmts2) =
  Program <$> Control.Monad.zipWithM commonStmt stmts1 stmts2

commonStmt :: Statement -> Statement -> PlaceholderCounter Statement
commonStmt (VarDecl x e1) (VarDecl y e2)
  | x == y    = VarDecl x <$> commonExp e1 e2
  | otherwise = VarDecl <$> nextPlaceholder <*> commonExp e1 e2
commonStmt (Assign lhs1 exp1) (Assign lhs2 exp2) =
  Assign <$> commonLhs lhs1 lhs2 <*> commonExp exp1 exp2
commonStmt _ _ = (Assign . PlaceholderLhs <$> nextPlaceholder) <*> (PlaceholderExp <$> nextPlaceholder)

-- Programの表示
showProgram :: Program -> String
showProgram (Program stmts) = unlines (map showStatement stmts)

-- Statementの表示
showStatement :: Statement -> String
showStatement (VarDecl x e) = "var " ++ x ++ " = " ++ showExp e
showStatement (Assign lhs e) = showLhs lhs ++ " = " ++ showExp e

-- Lhsの表示
showLhs :: Lhs -> String
showLhs (Var x) = x
showLhs (ObjAccess lhs prop) = showLhs lhs ++ "." ++ prop
showLhs (PlaceholderLhs ph) = ph

-- Expの表示
showExp :: Exp -> String
showExp (Num n) = show n
showExp (VarExp x) = x
showExp (New cls) = "new " ++ cls
showExp This = "this"
showExp (LhsExp lhs) = showLhs lhs
showExp (PlaceholderExp ph) = ph

prog1 :: Program
prog1 = Program
  [ VarDecl "temp1" (New "Node")
  , VarDecl "temp2" (Num 1)
  , Assign (ObjAccess (Var "temp1") "val") (VarExp "temp2")
  , Assign (ObjAccess (Var "this") "next") (VarExp "temp1")
  , Assign (ObjAccess (Var "temp1") "prev") (LhsExp (Var "this"))
  ]

prog2 :: Program
prog2 = Program
  [ VarDecl "temp1" (New "Node")
  , VarDecl "temp2" (Num 2)
  , Assign (ObjAccess (Var "temp1") "val") (VarExp "temp2")
  , Assign (ObjAccess (ObjAccess (Var "this") "next") "next")  (VarExp "temp1")
  , Assign (ObjAccess (Var "temp1") "prev") (LhsExp (ObjAccess (Var "this") "next"))
  ]

main :: IO ()
main = putStrLn . showProgram . evalState (commonProgram prog1 prog2) $ 1
