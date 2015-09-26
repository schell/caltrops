{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Effect
import Auth
import API
import Web.Common
import Web.Login
import Web.Form
import Data.Hashable
import Data.Time.Calendar
import Data.Time.Clock
import Data.Text (Text)
import Data.Acid
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.IntMap as IM
import qualified Text.Digestive.Blaze.Html5 as F
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html
import Text.Blaze.Renderer.Pretty (renderMarkup)
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Crypto.BCrypt

main :: IO ()
main = do
    putStrLn "Caltrops!"
    (users,logins) <- superUsers
    acid <- openLocalState $ UserData 0 users logins
    run 8081 $ serve api $ dataServer $ Data acid

dataServer :: Data -> Server API
dataServer dat = enter (runServantEffect dat) handlers

handlers :: ServerT API Effect
handlers = loginHandlers :<|> userHandlers

loginHandlers :: ServerT LoginAPI Effect
loginHandlers = getLogin
           :<|> postLogin
           :<|> logout

userHandlers :: ServerT UserAPI Effect
userHandlers = users
          :<|> userById
          :<|> userUpdate
          :<|> userUpdatePost
    where userByEmail mck txt = requireAuth mck $ do muser <- getUserByEmail txt
                                                     maybe userDNE return muser
          userById mck i = requireAuth mck $ do muser <- getUserById i
                                                maybe userDNE return muser
          users mck = requireAuth mck $ (map snd . IM.toList) <$> getUsers
          userUpdate mck = requireAuthWith mck $ \i -> do
                               muser <- getUserById i
                               maybe userDNE userUpdatePage muser
          userUpdatePost mck fields = requireAuthWith mck $ \u ->
                                        postUserUpdate u fields

userDNE :: Effect a
userDNE = left $ err404{ errBody = "User does not exist." }

getLogin :: Effect Html
getLogin = loginPage

postLogin :: [(Text,Text)] -> Effect (Headers '[SetCookieAuth] Html)
postLogin fields = do
    result <- getFormResult "login" loginForm fields
    case result of
        (view, Nothing) -> do
            let v = toHtml <$> view
                html = guestContainer $ formWrapper (loginView v) loginLink
            return $ addHeader nullCookie html
        (_, Just (Login email pass)) -> do
            logins <- getLogins
            muser  <- getUserByEmail email
            let mvalid = do user  <- muser
                            hpass <- IM.lookup (unId $ userId user) logins
                            let pass' = B.pack $ T.unpack pass
                            return $ (user, validatePassword hpass pass')

            -- Update last seen
            now   <- liftIO $ getCurrentTime
            let mvalid' = do (user,valid) <- mvalid
                             return $ (user{ userLastSeen = now }, valid)

            case mvalid' of
                Just (user, True) -> do
                    runUserUpdate $ UpdateUser user
                    ck <- liftIO $ loginCookieForId $ userId user
                    return $ addHeader ck loginSuccessHtml
                _ -> return $ addHeader nullCookie loginFailureHtml

logout :: Effect (Headers '[SetCookieAuth] Html)
logout = return $ addHeader nullCookie logoutHtml

postUserUpdate :: Id -> [(Text,Text)] -> Effect Html
postUserUpdate u fields = do
    result <- getFormResult "user" (userUpdateForm Nothing) fields
    case result of
        (view, Nothing) -> do
            let v = toHtml <$> view
            liftIO $ print $ renderMarkup $ F.errorList "password" v
            return $ userContainer $ formWrapper (userUpdateView v) userUpdateLink

        (_, Just uu) -> do
            muser <- getUserById u
            let muser' = applyUpdateUser uu <$> muser
                mpass  = updatedPassword uu
            case muser' of
                Just user -> do
                    runUserUpdate $ UpdateUser user
                    case mpass of
                        Just pass -> do
                            let p' = B.pack $ T.unpack pass
                            Just hpass <- liftIO $ hashPasswordUsingPolicy
                                                     slowerBcryptHashingPolicy
                                                     p'
                            runUserUpdate $ UpdatePassword u hpass
                        Nothing   -> return ()
                    return userUpdateSuccessHtml
                Nothing -> userDNE


getUserById :: Id -> Effect (Maybe User)
getUserById (Id i) = do
    users <- getUsers
    return $ IM.lookup i users

--------------------------------------------------------------------------------
-- USERS
--------------------------------------------------------------------------------
superUsers :: IO (Users, Logins)
superUsers = do
    utc <- getCurrentTime
    Just hpass <- hashPasswordUsingPolicy slowerBcryptHashingPolicy "changeme"
    let date = fromGregorian 2015 9 21
        schellEmail = "efsubenovex@gmail.com"
        aaronEmail = "aaronmaus@gmail.com"
        users = IM.fromList [ (0, User 0 "Schell Scivally" schellEmail date utc)
                            , (1, User 0 "Aaron Maus" aaronEmail date utc)
                            ]
        logins = IM.fromList [ (0, hpass)
                             , (1, hpass)
                             ]
    return (users,logins)
