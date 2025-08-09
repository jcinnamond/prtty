module Compiler.Internal.Rewrite where

import Compiler.Internal.Types (Compiler')
import Control.Monad.Error.Class (MonadError (..))
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Parser.AST qualified as AST
import Runtime.Value qualified as Value

type Rewrite = Compiler' [AST.Expr]
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
        , ("list", rewriteList)
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

rewriteList :: RewriteFn
rewriteList args =
    pure
        . L.intercalate [AST.Call "wait" M.empty []]
        . map showItem
  where
    showItem :: AST.Expr -> [AST.Expr]
    showItem e = bullet <> [e] <> [AST.Newline]

    bullet :: [AST.Expr]
    bullet = case M.lookup "bullet" args of
        (Just (Value.Literal t)) -> [AST.Literal $ t <> " "]
        _ -> []

withNoArgs :: Text -> ([AST.Expr] -> Rewrite) -> AST.Args -> ([AST.Expr] -> Rewrite)
withNoArgs name f args
    | M.null args = f
    | otherwise =
        const $
            throwError $
                "unexpected args when calling '"
                    <> name
                    <> "': "
                    <> T.show args