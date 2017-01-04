{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Effect (
    module U,
    Data(..),
    Effect,
    runUserUpdate,
    getLogins,
    getUsers,
    getUserByEmail,
    runServantEffect
) where

import Caltrops.Client
import Web.User as U
import Data.Text (Text)
import Data.Acid
import Data.Acid.Core (MethodState, MethodResult)
import Servant
import Control.Monad.Reader
import Control.Monad.Trans.Either

data Data = Data { userData :: AcidState UserData }

type Effect = (EitherT ServantErr (ReaderT Data IO))

runUserUpdate :: (UpdateEvent event, MethodState event ~ UserData)
              => event -> Effect (MethodResult event)
runUserUpdate f = do
    acid <- asks userData
    liftIO $ update acid f

getUserByEmail :: Text -> Effect (Maybe User)
getUserByEmail e = do
    users <- getUsers
    return $ foldl look4Email Nothing users
        where look4Email (Just u) _ = Just u
              look4Email Nothing u  = if userEmail u == e
                                      then Just u
                                      else Nothing

getLogins :: Effect Logins
getLogins = getsUserData udataLogins

getUsers :: Effect Users
getUsers = getsUserData udataUsers

getsUserData :: (UserData -> a) -> Effect a
getsUserData f = do
    acid <- asks userData
    f <$> (liftIO $ query acid GetUserData)

runServantEffect :: Data -> Effect :~> EitherT ServantErr IO
runServantEffect dat = Nat $ effectToServant dat

effectToServant :: forall a. Data -> Effect a -> EitherT ServantErr IO a
effectToServant dat s = do
    result <- liftIO $ runReaderT (runEitherT s) dat
    case result of
        Left err -> left err
        Right a  -> right a
