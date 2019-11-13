module Lib where

import Types
import CryptoHash
import Polysemy (Sem, Members)
import Polysemy.KVStore (KVStore)
import qualified Polysemy.KVStore as Store

addUser
  :: Members [CryptoHash, KVStore Username PasswordHash] r
  => Username
  -> Password
  -> Sem r ()
addUser username password = makeHash password >>= Store.writeKV username

validatePassword
  :: Members [CryptoHash, KVStore Username PasswordHash] r
  => Username
  -> Password
  -> Sem r Bool
validatePassword username password = Store.lookupKV username >>= \case
  Nothing   -> return False
  Just hash -> validateHash password hash

