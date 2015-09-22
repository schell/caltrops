{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Auth where

import Effect
import Servant
import Web.ClientSession
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Map as M
import Data.Maybe
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either

authenticate_ :: Text -> Effect a -> Effect a
authenticate_ blob = authenticate blob . const

authenticate :: Text -> (Id -> Effect a) -> Effect a
authenticate blob f = do
    key <- liftIO getDefaultKey
    -- Decode our UserCookie
    let cookies = parseCookies blob
        maybeRead = fmap fst . listToMaybe . reads
        mId = do cookie  <- M.lookup cookieName cookies
                 cookieD <- decrypt key (B.pack $ T.unpack cookie)
                 Id <$> (maybeRead $ B.unpack cookieD)
    case mId of
        Nothing -> left $ err403{ errBody = "Unauthorized :(" }
        Just u  -> f u

nullCookie :: Text
nullCookie = cookieText "" "-1"

cookieText :: Text -> Text -> Text
cookieText val age =
    T.concat [ cookieName
              , "="
              , val
              , "; "
              , "Path=/; "
              , "Max-Age="
              , age
              , "; HttpOnly"
              ]

cookieName :: Text
cookieName = "cookie"

parseCookies :: Text -> CookieMap
parseCookies = Prelude.foldl mapify M.empty . Prelude.map tuple . splitCookies
    where splitCookies   = T.split (==';')
          tuple t        = (T.takeWhile (/= '=') $ T.dropWhile (== ' ') t, T.drop 1 $ T.dropWhile (/= '=') t)
          mapify m (k,v) = M.insert k v m

cookieLife :: Int
cookieLife = 2592000 -- Default sign-in cookie life is 30 days worth of seconds

type CookieMap = Map Text Text
