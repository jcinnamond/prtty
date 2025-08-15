module Compiler.Internal.Rewrite where

import Compiler.Internal.Sizes (width)
import Control.Monad.Error.Class (MonadError (..))
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Parser.AST qualified as AST
import Runtime.Value qualified as Value

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
        , ("list", rewriteList)
        , ("quote", rewriteQuote)
        , ("alternate", rewriteAlternate)
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

rewriteQuote :: RewriteFn
rewriteQuote _ [] = pure []
rewriteQuote args body@[q] = do
    cite <- qcite args body
    pure $ [AST.Call "center" M.empty [qopen args, q, qclose args], AST.Newline] <> cite
rewriteQuote args body@(x : xs) = do
    cite <- qcite args body
    pure $
        [ AST.Call "center" M.empty [qopen args, x]
        , AST.Newline
        ]
            <> showMiddle xs
            <> [ AST.Call "center" M.empty [last xs, qclose args]
               , AST.Newline
               ]
            <> cite
  where
    showMiddle :: [AST.Expr] -> [AST.Expr]
    showMiddle [] = []
    showMiddle [_] = []
    showMiddle (l : ls) = AST.Call "center" M.empty [l] : AST.Newline : showMiddle ls

qcite :: AST.Args -> [AST.Expr] -> Either Text [AST.Expr]
qcite args body = case M.lookup "citation" args of
    Nothing -> pure []
    (Just (Value.Literal c)) ->
        let padding = T.replicate (longest body - T.length c - 1) " "
         in pure [AST.Call "center" M.empty [quoteStyle args $ padding <> "- " <> c], AST.Newline]
    (Just v) -> throwError $ "invalid citation: " <> T.show v
  where
    longest :: [AST.Expr] -> Int
    longest b = maximum $ map (width . L.singleton) b

qopen, qclose :: AST.Args -> AST.Expr
qopen args = quoteStyle args "“"
qclose = flip quoteStyle "”"

quoteStyle :: AST.Args -> Text -> AST.Expr
quoteStyle args t = case M.lookup "altColor" args of
    Nothing -> AST.Literal t
    (Just v) -> AST.Call "style" (M.fromList [("fg", v)]) [AST.Literal t]

rewriteAlternate :: RewriteFn
rewriteAlternate args body = do
    literals <- getLiterals body
    pure $ interleave (toType literals) (toDelete $ take (length literals - 1) literals)
  where
    getLiterals :: [AST.Expr] -> Either Text [Text]
    getLiterals [] = pure []
    getLiterals (AST.Literal t : rest) = (t :) <$> getLiterals rest
    getLiterals e = throwError $ "cannot use " <> T.show e <> " as an alternate"

    toType :: [Text] -> [AST.Expr]
    toType = map (\t -> AST.Call "type" delay [AST.Literal t])

    toDelete :: [Text] -> [AST.Expr]
    toDelete = map (\t -> AST.Call "backspace" delay [AST.Literal t])

    interleave :: [AST.Expr] -> [AST.Expr] -> [AST.Expr]
    interleave (x : xs) (y : ys) = x : AST.Call "wait" M.empty [] : y : interleave xs ys
    interleave [] [] = []
    interleave xs [] = xs
    interleave [] ys = ys

    delay :: AST.Args
    delay = case M.lookup "delay" args of
        Nothing -> M.empty
        (Just v) -> M.fromList [("delay", v)]

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