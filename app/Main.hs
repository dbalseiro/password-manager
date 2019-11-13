module Main where

import Lib
import CryptoHash
import Types

import Data.Function ((&))
import qualified Crypto.KDF.BCrypt as Crypto
import qualified Crypto.Random as Random
import Crypto.Random (DRG)

import qualified Polysemy as P
import Polysemy (Members, Member, Sem)

import qualified Polysemy.State as State
import Polysemy.State (State)

import qualified Polysemy.Embed as Embed
import qualified Polysemy.Error as Error

import Polysemy.KVStore (KVStore)
import qualified Polysemy.KVStore as Store

import Database.Redis (Connection)
import qualified Database.Redis as Hedis

runCryptoHashAsState
  :: (DRG gen, Member (State gen) r)
  => Sem (CryptoHash : r) a
  -> Sem r a
runCryptoHashAsState = P.interpret $ \case
  ValidateHash (Password password) (PasswordHash hash) ->
    return $ Crypto.validatePassword password hash

  MakeHash (Password password) -> do
    drg <- State.get
    let (hash, drg') = Random.withDRG drg (Crypto.hashPassword 5 password)
    State.put drg'
    return (PasswordHash hash)

runAllEffects
  :: DRG gen
  => gen
  -> Connection
  -> (forall r. Members [CryptoHash, KVStore Username PasswordHash] r => Sem r a)
  -> IO (Either Hedis.Reply a)
runAllEffects drg conn pgm = pgm
  & runCryptoHashAsState
  & Store.runKVStoreInRedis toByteString
  & Embed.runEmbedded (Hedis.runRedis conn)
  & State.evalState drg
  & Error.runError
  & P.runM

runAddUser :: Connection -> Username -> Password -> IO (Either Hedis.Reply ())
runAddUser conn username password = do
  drg <- Random.getSystemDRG
  runAllEffects drg conn (addUser username password)

runValidatePassword :: Connection -> Username -> Password -> IO (Either Hedis.Reply Bool)
runValidatePassword conn username password = do
  drg <- Random.getSystemDRG
  runAllEffects drg conn (validatePassword username password)

withDBConnection :: (Connection -> IO a) -> IO a
withDBConnection f =
  Hedis.checkedConnect Hedis.defaultConnectInfo >>= f

main :: IO ()
main = withDBConnection $ \conn -> do
  putStrLn "Adding a username and password to the store"
  runAddUser conn "avengers" "assemble" >>= \case
    Left err -> printError err
    Right () -> do
      putStr "Validating a good password: "
      runValidatePassword conn "avengers" "assemble" >>= printResult

      putStr "Validating a bad password: "
      runValidatePassword conn "avengers" "runaway" >>= printResult

  where
    printResult (Right True)  = putStrLn "Accepted"
    printResult (Right False) = putStrLn "Rejected"
    printResult (Left err) = printError err

    printError err = putStrLn $ "An error occured: " <> show err

