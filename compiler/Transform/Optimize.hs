module Transform.Optimize (optimize) where

import SourceSyntax.Declaration (Declaration(..))
import SourceSyntax.Expression
import SourceSyntax.Literal
import SourceSyntax.Location
import SourceSyntax.Module
import Control.Arrow (second, (***))
import Data.Char (isAlpha)

optimize (Module name ims exs stmts) =
    Module name ims exs (map optimizeStmt stmts)

optimizeStmt stmt = if stmt == stmt' then stmt' else optimizeStmt stmt'
    where stmt' = simp stmt

class Simplify a where
  simp :: a -> a

instance Simplify (Declaration t v) where
  simp (Definition def) = Definition (simp def)
  simp (ImportEvent js b elm t) = ImportEvent js (simp b) elm t
  simp stmt = stmt

instance Simplify (Def t v) where
  simp (Def name e) = Def name (simp e)
  simp x = x

instance Simplify e => Simplify (Located e) where
  simp (L s e) = L s (simp e)

instance Simplify (Expr t v) where
  simp expr =
    let f = simp in
    case expr of
      Range e1 e2 -> Range (f e1) (f e2)
      Binop op e1 e2 -> binop op (f e1) (f e2)
      Lambda x e -> Lambda x (f e)
      Record fs -> Record (map (second simp) fs)
      App e1 e2 -> App (f e1) (f e2)
      Let defs e -> Let (map simp defs) (f e)
      Data name es -> Data name (map f es)
      MultiIf es -> MultiIf . clipBranches $ map (f *** f) es
      Case e cases -> Case (f e) (map (second f) cases)
      _ -> expr


clipBranches [] = []
clipBranches (e:es) =
    case e of
      (L _ (Literal (Boolean True)), _) -> [e]
      _ -> e : clipBranches es


isValue e =
    case e of
      Literal _  -> True
      Var _      -> True
      Data _ _   -> True
      _          -> False


binop op ce1@(L s1 e1) ce2@(L s2 e2) =
  let c1 = L s1
      c2 = L s2
      int = Literal . IntNum
      str = Literal . Str
      bool = Literal . Boolean
  in  
  case (op, e1, e2) of
    (_, Literal (IntNum n), Literal (IntNum m)) ->
        case op of
          { "+" -> int $ (+) n m
          ; "-" -> int $ (-) n m
          ; "*" -> int $ (*) n m
          ; "^" -> int $ n ^ m
          ; "div" -> int $ div n m
          ; "mod" -> int $ mod n m
          ; "rem" -> int $ rem n m
          ; "<" -> bool $ n < m
          ; ">" -> bool $ n > m
          ; "<=" -> bool $ n <= m
          ; ">=" -> bool $ n >= m
          ; "==" -> bool $ n == m
          ; "/=" -> bool $ n /= m
          ;  _  -> Binop op ce1 ce2 }
  {--
    -- flip order to move lone integers to the left
    ("+", _, IntNum n) -> binop "+" ce2 ce1
    ("*", _, IntNum n) -> binop "*" ce2 ce1

    ("+", IntNum 0, _) -> e2
    ("+", IntNum n, Binop "+" (L _ (IntNum m)) ce) ->
        binop "+" (c1 $ IntNum (n+m)) ce
    ("+", Binop "+" (L _ (IntNum n)) ce1'
        , Binop "+" (L _ (IntNum m)) ce2') ->
        binop "+" (none $ IntNum (n+m)) (none $ Binop "+" ce1' ce2')

    ("*", IntNum 0, _) -> e1
    ("*", IntNum 1, _) -> e2
    ("*", IntNum n, Binop "*" (L _ (IntNum m)) ce) ->
        binop "*" (none $ IntNum (n*m)) ce
    ("*", Binop "*" (L _ (IntNum n)) ce1'
        , Binop "*" (L _ (IntNum m)) ce2') ->
        binop "*" (none $ IntNum (n*m)) (none $ Binop "*" ce1' ce2')

    ("-", _, IntNum 0) -> e1
    ("/", _, IntNum 1) -> e1
    ("div", _, IntNum 1) -> e1
--}
    (_, Literal (Boolean n), Literal (Boolean m)) ->
        case op of
          "&&" -> bool $ n && m
          "||" -> bool $ n || m
          _    -> Binop op ce1 ce2

    ("&&", Literal (Boolean  True), _) -> e2
    ("&&", Literal (Boolean False), _) -> bool False
    ("||", Literal (Boolean  True), _) -> bool True
    ("||", Literal (Boolean False), _) -> e2

    ("::", _, _) -> Data "::" [ce1, ce2]

    ("++", Literal (Str s1), Literal (Str s2)) -> str $ s1 ++ s2
    ("++", Literal (Str s1), Binop "++" (L _ (Literal (Str s2))) ce) ->
        Binop "++" (c1 . str $ s1 ++ s2) ce
    ("++", Binop "++" e (L _ (Literal (Str s1))), Literal (Str s2)) ->
        Binop "++" e (c1 . str $ s1 ++ s2)

    ("++", Data "[]" [], _) -> e2
    ("++", _, Data "[]" []) -> e1
    ("++", Data "::" [h,t], _) -> Data "::" [h, none $ binop "++" t ce2]

    _ | isAlpha (head op) || '_' == head op ->
          App (none $ App (none $ Var op) ce1) ce2
      | otherwise -> Binop op ce1 ce2