{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Effect where

import Data.Aeson
import Data.Time.Calendar
import Data.IntMap (IntMap)
import GHC.Generics
import Servant.API
import Servant
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Concurrent.STM
import Data.ByteString (ByteString)

fresh :: Effect Id
fresh = do
    i <- get dataId
    put dataId $ i + 1
    return i

modify :: (Data -> TVar a) -> (a -> a) -> Effect ()
modify f g = g <$> get f >>= put f

put :: (Data -> TVar a) -> a -> Effect ()
put f a = asks f >>= liftIO . atomically . flip writeTVar a

get :: (Data -> TVar a) -> Effect a
get f = asks f >>= liftIO . readTVarIO

runServantEffect :: Data -> Effect :~> EitherT ServantErr IO
runServantEffect dat = Nat $ effectToServant dat

effectToServant :: forall a. Data -> Effect a -> EitherT ServantErr IO a
effectToServant dat s = do
    result <- liftIO $ runReaderT (runEitherT s) dat
    case result of
        Left err -> left err
        Right a  -> right a

type Effect = (EitherT ServantErr (ReaderT Data IO))

data Data = Data { dataId     :: TVar Id
                 , dataUsers  :: TVar Users
                 , dataLogins :: TVar Logins
                 }

type Logins = IntMap ByteString

type Users = IntMap User

instance ToJSON User

instance ToJSON Day where
    toJSON d = toJSON (showGregorian d)

data User = User { userName  :: String
                 , userEmail :: String
                 , userSince :: Day
                 } deriving (Eq, Show, Generic)

deriving instance FromText Id
deriving instance ToJSON Id
deriving instance Num Id
deriving instance Eq Id
deriving instance Read Id
deriving instance Show Id

newtype Id = Id { unId :: Int }
