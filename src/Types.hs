module Types where

import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Binary

newtype Username = Username ByteString
  deriving (Eq, Show, Ord, IsString, Binary)

newtype Password = Password ByteString
  deriving (Eq, Show, IsString)

newtype PasswordHash = PasswordHash ByteString
  deriving (Eq, Show, IsString, Binary)
