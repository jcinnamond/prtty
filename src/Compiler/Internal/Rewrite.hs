module Compiler.Internal.Rewrite where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Parser.AST qualified as AST

type Rewrite = Either Text [AST.Expr]
type RewriteFn = AST.Args -> [AST.Expr] -> Rewrite

rewrite :: [AST.Expr] -> Rewrite
rewrite es = concat <$> traverse rewriteExpression es

rewriteExpression :: AST.Expr -> Rewrite
rewriteExpression (AST.LiteralLine t) = pure [AST.Literal t, AST.Newline]
rewriteExpression (AST.Call name args body) = do
    body' <- rewrite body
    case M.lookup name rewriteRules of
        Nothing -> pure [AST.Call name args body']
        (Just r) -> r args body'
rewriteExpression e = pure [e]

-- The rewrite rules

rewriteRules :: Map Text RewriteFn
rewriteRules =
    M.fromList
        [ ("middle", rewriteMiddle)
        ]

rewriteMiddle :: RewriteFn
rewriteMiddle = withNoArgs "middle" middle
  where
    middle :: [AST.Expr] -> Rewrite
    middle body =
        pure
            [ AST.Call
                "vcenter"
                M.empty
                [AST.Call "center" M.empty body]
            ]

withNoArgs :: Text -> ([AST.Expr] -> Rewrite) -> AST.Args -> ([AST.Expr] -> Rewrite)
withNoArgs name f args
    | M.null args = f
    | otherwise =
        const $
            Left $
                "unexpected args when calling '"
                    <> name
                    <> "': "
                    <> T.show args