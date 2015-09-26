{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module API where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html
import Data.Text (Text)
import Data.ByteString.Conversion.To
import Data.Aeson (ToJSON(..))
import Data.Time.Calendar
import Data.Time.Clock
import Data.Typeable
import GHC.Generics

--------------------------------------------------------------------------------
-- Links
--------------------------------------------------------------------------------
instance ToValue URI where
    toValue = toValue . ('/':) . show

userUpdateLink :: URI
userUpdateLink = safeLink api (Proxy :: Proxy UserUpdateGET)

loginLink :: URI
loginLink = safeLink api (Proxy :: Proxy LoginGET)
--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
api :: Proxy API
api = Proxy

type API = LoginAPI :<|> UserAPI
--------------------------------------------------------------------------------
-- Login API
--------------------------------------------------------------------------------
loginAPI :: Proxy LoginAPI
loginAPI = Proxy

type LoginAPI = LoginGET
           :<|> LoginPOST
           :<|> LogoutGET

type LoginGET  = "login" :> Get '[HTML] Html

type LoginPOST = "login" :> ReqBody '[FormUrlEncoded] [(Text,Text)]
                         :> Post '[HTML] (Headers '[SetCookieAuth] Html)

type LogoutGET = "logout" :> Get '[HTML] (Headers '[SetCookieAuth] Html)
--------------------------------------------------------------------------------
-- User API
--------------------------------------------------------------------------------
userAPI :: Proxy UserAPI
userAPI = Proxy

type UserAPI = "users" :> CookieAuth :> Get '[JSON] [User]
          :<|> "user" :> CookieAuth :> Capture "userId" Id :> Get '[JSON] User
          :<|> UserUpdateGET
          :<|> UserUpdatePOST

type UserUpdateGET =
    "user" :> "update" :> CookieAuth :> Get '[HTML] Html

type UserUpdatePOST =
    "user" :> "update" :> CookieAuth :> ReqBody '[FormUrlEncoded] [(Text,Text)]
           :> Post '[HTML] Html
--------------------------------------------------------------------------------
-- Auth Header Types
--------------------------------------------------------------------------------
type SetCookieAuth = Header "Set-Cookie" LoginCookie
type CookieAuth = Header "Cookie" LoginCookie

instance ToByteString LoginCookie where
    builder = builder . unLoginCookie

instance FromText LoginCookie where
    fromText = Just . LoginCookie

newtype LoginCookie = LoginCookie { unLoginCookie :: Text }
--------------------------------------------------------------------------------
-- User Types
--------------------------------------------------------------------------------
newtype Id = Id { unId :: Int }

deriving instance FromText Id
deriving instance ToJSON Id
deriving instance Typeable Id
deriving instance Num Id
deriving instance Eq Id
deriving instance Read Id
deriving instance Show Id

data User = User { userId       :: Id
                 , userName     :: Text
                 , userEmail    :: Text
                 , userSince    :: Day
                 , userLastSeen :: UTCTime
                 } deriving (Eq, Show, Generic, Typeable)

instance ToJSON Day where
    toJSON d = toJSON (showGregorian d)

instance ToJSON User

--------------------------------------------------------------------------------
-- Login Types
--------------------------------------------------------------------------------
data Login = Login { loginEmail :: Text
                   , loginPass :: Text
                   } deriving (Show, Eq)
