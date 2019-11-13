module CryptoHash where

import Types
import qualified Polysemy as P

data CryptoHash m a where
  -- | Generates a hash from a password
  MakeHash :: Password -> CryptoHash m PasswordHash

  -- | Check if a password matches a hash
  ValidateHash :: Password -> PasswordHash -> CryptoHash m Bool

P.makeSem ''CryptoHash

-- Generates this code
{-
makeHash :: Member CryptoHash r => Password -> Sem r PasswordHash
makeHash password = P.send (MakeHash password :: CryptoHash (Sem r) PasswordHash)

validateHash :: Member CryptoHash r => Password -> PasswordHash -> Sem r Bool
validateHash password hash = P.send (ValidateHash password hash :: CryptoHash (Sem r) Bool)
-}

