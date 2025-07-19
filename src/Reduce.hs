module Reduce (
    reduce,
) where

import Data.Text (Text)
import Data.Text qualified as T
import PDL (Expr (..))
import Runtime (Instruction (..))
import VT qualified

reduce :: [Expr] -> Either Text [Instruction]
reduce es = case mapM reduceExpr es of
    Left e -> Left e
    Right is -> Right $ concat is

reduceExpr :: Expr -> Either Text [Instruction]
reduceExpr (Literal x) = Right [Output $ VT.Literal x]
reduceExpr (Instruction i es) = mapInstruction i es

mapInstruction :: Text -> [Expr] -> Either Text [Instruction]
mapInstruction "wait" [] = Right [WaitForInput]
mapInstruction "clear" [] = Right [Output VT.ClearScreen]
mapInstruction "color" [Literal c] = Right [Output $ VT.fgColor c]
mapInstruction "home" [] = Right [Output VT.Home]
mapInstruction i args = Left $ "unrecognised instruction " <> i <> " (" <> showArgs args <> ")"

showArgs :: [Expr] -> Text
showArgs args = T.intercalate ", " (tshow <$> args)

tshow :: (Show s) => s -> Text
tshow = T.pack . show