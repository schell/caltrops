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
module Main where

import Effect
import Auth
import HTML.Login
import Web.ClientSession
import Data.Hashable
import Data.Time.Calendar
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.IntMap as IM
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html
import Control.Monad.Trans.Either
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Crypto.BCrypt

main :: IO ()
main = do
    putStrLn "Caltrops!"
    (users,logins) <- superUsers
    dat <- Data <$> (atomically $ newTVar 0)
                <*> (atomically $ newTVar users)
                <*> (atomically $ newTVar logins)
    run 8081 $ serve api $ dataServer dat

dataServer :: Data -> Server API
dataServer dat = enter (runServantEffect dat) handlers

api :: Proxy API
api = Proxy

handlers :: ServerT API Effect
handlers = loginHandlers :<|> userHandlers

loginHandlers :: ServerT LoginAPI Effect
loginHandlers = getLogin
           :<|> postLogin
           :<|> logout

userHandlers :: ServerT UserAPI Effect
userHandlers = getUsers :<|> getUser

getLogin :: Effect Html
getLogin = return loginHtml

postLogin :: LoginForm -> Effect (Headers '[Header "Set-Cookie" Text] Html)
postLogin (LoginForm email pass) = do
    logins <- get dataLogins
    let k = hash email
        mvalid = do hpass <- IM.lookup k logins
                    return $ validatePassword hpass $ B.pack pass
    case mvalid of
        Just True -> do ky <- liftIO getDefaultKey
                        u' <- liftIO $ encryptIO ky $ B.pack $ show k
                        let cred = cookieText (T.pack $ B.unpack u')
                                              (T.pack $ show cookieLife)
                        return $ addHeader cred loginSuccessHtml
        _ -> return $ addHeader "" loginFailureHtml

logout :: Effect (Headers '[Header "Set-Cookie" Text] Html)
logout = return $ addHeader nullCookie $ logoutHtml

getUser :: Maybe Text -> Id -> Effect User
getUser Nothing _ = left $ err403
getUser (Just blob) (Id i) = validate blob $ const $ do
    users <- get dataUsers
    case IM.lookup i users of
        Nothing -> left $ err404 { errBody = "User does not exist." }
        Just u  -> return u

getUsers :: Effect [User]
getUsers = do
   users <- get dataUsers
   return $ map snd $ IM.toList users

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
type API = LoginAPI :<|> UserAPI

type LoginAPI = "login" :> Get '[HTML] Html
           :<|> "login" :> ReqBody '[FormUrlEncoded] LoginForm
                        :> Post '[HTML] (Headers '[Header "Set-Cookie" Text] Html)
           :<|> "logout" :> Get '[HTML] (Headers '[Header "Set-Cookie" Text] Html)

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "user" :> Header "Cookie" Text :> Capture "userId" Id :> Get '[JSON] User
--------------------------------------------------------------------------------
-- USERS
--------------------------------------------------------------------------------
superUsers :: IO (Users, Logins)
superUsers = do
    Just hpass <- hashPasswordUsingPolicy slowerBcryptHashingPolicy "changeme"
    let schellEmail = "efsubenovex@gmail.com"
        aaronEmail = "aaronmaus@gmail.com"
        users = IM.fromList [ (hash schellEmail, User "Schell Scivally" schellEmail $ fromGregorian 2015 9 21)
                            , (hash aaronEmail, User "Aaron Maus" aaronEmail $ fromGregorian 2015 9 21)
                            ]
        logins = IM.fromList [ (hash schellEmail, hpass)
                             , (hash aaronEmail, hpass)
                             ]
    return (users,logins)
