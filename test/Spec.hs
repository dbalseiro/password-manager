module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Function ((&))
import qualified Data.ByteString as BS
import qualified Data.Map as Map

import Polysemy (Sem, Members)
import qualified Polysemy as P

import Polysemy.KVStore (KVStore)
import qualified Polysemy.KVStore as KVStore

import CryptoHash
import Types
import Lib

main :: IO ()
main = hspec $
  describe "Basic Validation" $ do
    it "can validate an added password" $
      assert $ addAndValidate True "user" "password"
    it "works on random input" $
      property $ \user pass -> addAndValidate False (Username user) (Password pass) == True

hashit :: Password -> PasswordHash
hashit = const "HASHED"

reverseit :: Password -> PasswordHash
reverseit = PasswordHash . BS.reverse . toByteString

runCryptoHash :: Bool -> Sem (CryptoHash : r) a -> Sem r a
runCryptoHash constant =
  let f = if constant then hashit else reverseit
   in P.interpret $ \case
        ValidateHash password passwordHash -> return $ f password == passwordHash
        MakeHash password -> return $ f password

runAllEffects
  :: Bool
  -> (forall r. Members [CryptoHash, KVStore Username PasswordHash] r => Sem r a)
  -> a
runAllEffects constant pgm = pgm
  & runCryptoHash constant
  & KVStore.runKVStorePurely Map.empty
  & P.run
  & snd

addAndValidate :: Bool -> Username -> Password -> Bool
addAndValidate constant user password = runAllEffects constant $ do
  addUser user password
  validatePassword user password

assert :: Bool -> Expectation
assert = shouldBe True

