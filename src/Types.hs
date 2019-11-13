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

class ToByteString a where
  toByteString :: a -> ByteString

instance ToByteString Username where
  toByteString (Username bs) = bs

instance ToByteString Password where
  toByteString (Password bs) = bs

instance ToByteString PasswordHash where
  toByteString (PasswordHash bs) = bs

