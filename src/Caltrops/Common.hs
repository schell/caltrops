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
module Caltrops.Common where

import Servant
import Data.Aeson (ToJSON(..))
import Data.Typeable
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Generics

newtype Id = Id { unId :: Int }

deriving instance FromText Id
deriving instance ToJSON Id
deriving instance Typeable Id
deriving instance Num Id
deriving instance Eq Id
deriving instance Read Id
deriving instance Show Id
--------------------------------------------------------------------------------
-- User Types
--------------------------------------------------------------------------------
data User = User { userId       :: Id
                 , userName     :: Text
                 , userEmail    :: Text
                 , userSince    :: Day
                 , userLastSeen :: UTCTime
                 } deriving (Eq, Show, Generic, Typeable)

instance ToJSON Day where
    toJSON d = toJSON (showGregorian d)

instance ToJSON User
