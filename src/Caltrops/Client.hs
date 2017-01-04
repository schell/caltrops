{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Caltrops.Client (
    module C,
    LoginJSON,
    loginJSON,
    login,
    LoginCookie(..),
    HeaderCookie(..),
    Login(..),
    AuthdRequest(..),
    UserJSON,
) where

import Caltrops.Common as C
import Servant
import Servant.Client
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Control.Monad.Trans.Either
--------------------------------------------------------------------------------
-- Login Types
--------------------------------------------------------------------------------
instance FromJSON LoginCookie
instance ToJSON LoginCookie

newtype LoginCookie = LoginCookie { unLoginCookie :: Text }
                    deriving (Show, Eq, Generic)

instance FromJSON HeaderCookie
instance ToJSON HeaderCookie

newtype HeaderCookie = HeaderCookie { unHeaderCookie :: Text }
                     deriving (Show, Eq, Generic)

instance FromJSON Login
instance ToJSON Login

data Login = Login { loginEmail :: Text
                   , loginPass :: Text
                   } deriving (Show, Eq, Generic)

loginJSON :: Proxy LoginJSON
loginJSON = Proxy

login :: BaseUrl -> Login -> EitherT ServantError IO (Maybe LoginCookie)
login = client loginJSON

type LoginJSON = "login" :> ReqBody '[JSON] Login
                         :> Post '[JSON] (Maybe LoginCookie)
--------------------------------------------------------------------------------
-- Auth'd Request Types
--------------------------------------------------------------------------------
data AuthdRequest a = AuthdRequest { authCookie :: LoginCookie
                                   , authRequest:: a
                                   } deriving (Show, Eq, Generic)
--------------------------------------------------------------------------------
-- User Types
--------------------------------------------------------------------------------
type UserRequest = AuthdRequest Id
type UserJSON = "user" :> ReqBody '[JSON] UserRequest :> Get '[JSON] User
--------------------------------------------------------------------------------
-- World Types
--------------------------------------------------------------------------------


