module Reduce (
    reduce,
) where

import Data.Text (Text)
import PDL ( Presentation(..), TopLevelExpr(..), Expr(..), Arg(..), Args(..) )
import PDL qualified
import Runtime (Instruction (..), Duration (..))
import VT qualified
import qualified Data.Text as T

reduce :: Presentation -> Either Text [Instruction]
reduce Presentation{presentation} = concat <$> mapM reduceTopLevelExpr presentation

reduceTopLevelExpr :: TopLevelExpr -> Either Text [Instruction]
reduceTopLevelExpr (PDefinition _) = Left "not implemented"
reduceTopLevelExpr (PSlide _ es) = reduceExprs es

reduceExprs :: [Expr] -> Either Text [Instruction]
reduceExprs es =
    case mapM reduceExpr es of
        Left err -> Left err
        Right is -> Right $ [Output VT.ClearScreen, Output VT.Home] <> concat is <> [WaitForInput]

reduceExpr :: Expr -> Either Text [Instruction]
reduceExpr (Literal x) = Right [Output $ VT.Literal $ x <> "\n"]
reduceExpr (Instruction i args es) = mapInstruction i args es

mapInstruction :: Text -> Args -> [Expr] -> Either Text [Instruction]
mapInstruction "cursorShow" _ es = Right [Output VT.ShowCursor] <> reduceExprs es
mapInstruction "cursorHide" _ es = Right [Output VT.HideCursor] <> reduceExprs es
mapInstruction "wait" _ [] = Right [WaitForInput]
mapInstruction "clear" _ [] = Right [Output VT.ClearScreen]
mapInstruction "color" (Args [PDL.ArgHex rgb]) [] = Right [Output $ VT.fgColor $ PDL.fromHex rgb]
mapInstruction "bgColor" (Args [PDL.ArgHex rgb]) [] = Right [Output $ VT.bgColor $ PDL.fromHex rgb]
mapInstruction "bold" _ [] = Right [Output VT.bold]
mapInstruction "home" _ [] = Right [Output VT.Home]
mapInstruction "type" _ es = typeChars es
mapInstruction i _ args = Left $ "unrecognised instruction " <> i <> " (" <> showArgs args <> ")"

typeChars :: [Expr] -> Either Text [Instruction]
typeChars [] = Right []
typeChars ((Literal l) : es) =
    Right
        ( T.foldl'
            (\is c -> is ++ [Output $ VT.Literal $ T.singleton c, Pause $ Millisecond 50])
            []
            l
        )
        <> typeChars es
typeChars (e : es) = reduceExpr e <> typeChars es

showArgs :: [Expr] -> Text
showArgs args = T.intercalate ", " (tshow <$> args)

tshow :: (Show s) => s -> Text
tshow = T.pack . show