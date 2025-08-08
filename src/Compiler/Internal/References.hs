module Compiler.Internal.References where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Parser.AST qualified as AST
import Runtime.Value (Value (..))

type Definitions = Map Text Value
type Partition = (Definitions, [AST.Expr])

resolveReferences :: [AST.Presentation] -> Either Text [AST.Expr]
resolveReferences ps =
    let (definitions, exprs) = partition ps
     in traverse (go definitions) exprs
  where
    go :: Definitions -> AST.Expr -> Either Text AST.Expr
    go definitions (AST.Call name args body) = do
        args' <- resolve definitions args
        body' <- traverse (go definitions) body
        pure $ AST.Call name args' body'
    go _ e = pure e

resolve :: Definitions -> AST.Args -> Either Text AST.Args
resolve definitions = traverse resolve'
  where
    resolve' :: Value -> Either Text Value
    resolve' (Reference x) = case M.lookup x definitions of
        Nothing -> Left $ "unresolved reference $" <> x
        Just v -> pure v
    resolve' v = pure v

partition :: [AST.Presentation] -> (Definitions, [AST.Expr])
partition = foldl' go (M.empty, [])
  where
    go :: Partition -> AST.Presentation -> Partition
    go (definitions, exprs) (AST.Presentation{presentation}) =
        ( definitions <> extractDefinitions presentation
        , exprs <> extractExprs presentation
        )

extractDefinitions :: [AST.PresentationItem] -> Definitions
extractDefinitions = M.fromList . foldl' go []
  where
    go :: [(Text, Value)] -> AST.PresentationItem -> [(Text, Value)]
    go ds (AST.PDef n v) = (n, v) : ds
    go ds _ = ds

extractExprs :: [AST.PresentationItem] -> [AST.Expr]
extractExprs = reverse . foldl' go []
  where
    go :: [AST.Expr] -> AST.PresentationItem -> [AST.Expr]
    go es (AST.PExpr e) = e : es
    go es _ = es