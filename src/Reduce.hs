module Reduce (
    reduce,
) where

import Data.Text (Text)
import Data.Text qualified as T
import PDL (Args (..), Expr (..), Presentation (..), TopLevelExpr (..))
import PDL qualified
import Runtime (Duration (..), Instruction (..))
import VT qualified

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
mapInstruction "type" _ es = runTypeChars es
mapInstruction "center" _ es = runCenter es
mapInstruction "vcenter" _ es = runVCenter es
mapInstruction "moveTo" (Args [PDL.ArgInt y, PDL.ArgInt x]) [] = Right [Output $ VT.MoveTo y x]
mapInstruction i _ args = Left $ "unrecognised instruction " <> i <> " (" <> showArgs args <> ")"

runTypeChars :: [Expr] -> Either Text [Instruction]
runTypeChars [] = Right []
runTypeChars ((Literal l) : es) =
    Right
        ( T.foldl'
            (\is c -> is ++ [Output $ VT.Literal $ T.singleton c, Pause $ Millisecond 50])
            []
            l
        )
        <> runTypeChars es
runTypeChars (e : es) = reduceExpr e <> runTypeChars es

runCenter :: [Expr] -> Either Text [Instruction]
runCenter [] = Right [Center 0]
runCenter ((Literal l) : es) =
    Right (centerLines l) <> runCenter es
  where
    centerLines = concatMap centerLine . T.lines
    centerLine line = [Center $ T.length line, Output $ VT.Literal $ line <> "\n"]
runCenter (e : es) = reduceExpr e <> runCenter es

runVCenter :: [Expr] -> Either Text [Instruction]
runVCenter [] = Right [VCenter 0]
runVCenter es = do
    rest <- reduceExprs es
    Right ([VCenter $ sum $ map lineHeight es] <> rest)
  where
    lineHeight (Literal l) = length $ T.lines l
    lineHeight (Instruction _ _ []) = 0
    lineHeight (Instruction _ _ es') = sum $ map lineHeight es'

showArgs :: [Expr] -> Text
showArgs args = T.intercalate ", " (tshow <$> args)

tshow :: (Show s) => s -> Text
tshow = T.pack . show