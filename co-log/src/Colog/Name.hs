{-# LANGUAGE UndecidableInstances #-}

-- | Contains @newtype@ wrapper 'LoggerName' for list of components for logger name.

module Colog.Name
       ( LoggerName (..)
       ) where

-- import Data.Aeson.Types (ToJSON, ToJSONKey (..))
-- import Text.Show (Show (show))

import qualified Data.Text as T

{- | Logger name is a unique label for some 'LogAction'. Loggers in @co-log@
form tree. Every 'LoggerName' contains some list of components. In textual
representation those commponents separated by dot character. Below you can see
example of correspondence between list of components and string:

@
["foor", "bar", "baz"] <-> "foo.bar.baz"
@
-}
newtype LoggerName = LoggerName
    { unLoggerName :: [Text]
    } deriving (Eq, Ord, Generic, Semigroup, Monoid, IsList)

instance Hashable LoggerName where
    hashWithSalt salt = foldl' hashWithSalt salt . unLoggerName

-- -- | This instance is mostly for convenient way to create 'LoggerName' from list.
-- instance IsList LoggerName where
--     type Item LoggerName = Text
--     fromList = LoggerName
--     toList   = unLoggerName

{- | Split a dot-separated string. Empty string turns into a 'LoggerName' with
zero components.
-}
instance IsString LoggerName where
    fromString :: String -> LoggerName
    fromString s  = LoggerName $ case s of
        "" -> []
        s' -> T.splitOn "." (fromString s')

-- instance Buildable LoggerName where
--     build = mconcat . intersperse "." . toList . map smartBuild . unLoggerName
--       where
--         smartBuild "" = "<empty>"
--         smartBuild s  = build s
--
-- instance Show LoggerName where
--     show = fmt . build

-- instance ToJSON    LoggerName
-- instance ToJSONKey LoggerName
