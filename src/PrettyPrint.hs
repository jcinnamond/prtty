{-# LANGUAGE UndecidableInstances #-}

module PrettyPrint (
    PrettyPrint (..),
    Empty (..),
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V

class PrettyPrint a where
    prettyIndent :: Text -> a -> Text
    prettyIndent i = (<> i) . pretty

    pretty :: a -> Text
    pretty = prettyIndent ""

instance (PrettyPrint a) => PrettyPrint [a] where
    pretty = T.intercalate "\n" . map pretty

instance (PrettyPrint a) => PrettyPrint (Vector a) where
    pretty = T.intercalate "\n" . V.toList . V.map pretty

instance {-# OVERLAPPABLE #-} (Show a) => PrettyPrint a where
    pretty = T.show

class Empty a where
    empty :: a

instance Empty [a] where
    empty = []

instance Empty (Vector a) where
    empty = V.empty