{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Web.User where

import API
import Caltrops.Common
import Web.Bootstrap
import Web.Common
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.Text (Text)
import Data.SafeCopy
import Data.Acid
import Data.Typeable
import Data.ByteString (ByteString)
import GHC.Generics
import Control.Monad.State
import Control.Monad.Reader
import qualified Text.Digestive.Blaze.Html5 as F
import qualified Data.Text as T
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
import Text.Digestive


type Logins = IntMap ByteString

type Users = IntMap User

data UserData = UserData { udataNextId :: Id
                         , udataUsers  :: Users
                         , udataLogins :: Logins
                         } deriving (Typeable)

$(deriveSafeCopy 0 'base ''Id)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''UserData)

updateUser :: User -> Update UserData ()
updateUser u = do
    let k = unId $ userId u
    users <- gets udataUsers
    modify $ \dat -> dat{ udataUsers = IM.insert k u users }

updatePassword :: Id -> ByteString -> Update UserData ()
updatePassword (Id i) p = do
    logins <- gets udataLogins
    modify $ \dat -> dat{ udataLogins = IM.insert i p logins }

putUserData :: UserData -> Update UserData ()
putUserData = put

getUserData :: Query UserData UserData
getUserData = ask

fresh :: Update UserData Id
fresh = do
    i <- gets udataNextId
    modify $ \dat -> dat{ udataNextId = i + 1 }
    return i

$(makeAcidic ''UserData [ 'putUserData
                        , 'getUserData
                        , 'updateUser
                        , 'fresh
                        , 'updatePassword
                        ])

--------------------------------------------------------------------------------
-- Forms
--------------------------------------------------------------------------------
data UserUpdate = UserUpdate { userUpdateName    :: Text
                             , userUpdateEmail   :: Text
                             , userUpdatePass    :: (Maybe Text, Maybe Text)
                             } deriving (Typeable, Generic, Show, Eq)

passwordUpdateForm :: Monad m => Form Text m (Maybe Text, Maybe Text)
passwordUpdateForm = (,)
    <$> "password" .: optionalText Nothing
    <*> "password-check" .: optionalText Nothing

userUpdateForm :: Monad m => Maybe User -> Form Text m UserUpdate
userUpdateForm mu = UserUpdate
    <$> "name" .: check "name cannot be empty" notEmpty (text $ userName <$> mu)
    <*> "email" .: check "email cannot be empty" notEmpty (text $ userEmail <$> mu)
    <*> "password" .: check "passwords must match" mustmatch passwordUpdateForm
        where notEmpty = not . T.null
              mustmatch (a,b) = a == b

userUpdateSuccessHtml :: Html
userUpdateSuccessHtml = userContainer $ H.div "Successfully updated user."

passwordUpdateView :: View Html -> Html
passwordUpdateView v = do
    H.legend "Password:"
    bootstrapFormFieldPassword "password" "Password:" v
    bootstrapFormFieldPassword "password-check" "Password Check:" v

userUpdateView :: View Html -> Html
userUpdateView v = do
    H.legend "User:"
    bootstrapFormFieldInput "name" "Name:" v
    bootstrapFormFieldInput "email" "Email:" v
    passwordUpdateView $ subView "password" v
    F.errorList "password" v

userUpdatePage :: Monad m => User -> m Html
userUpdatePage u = do
    v <- getForm "user" $ userUpdateForm $ Just u
    let v' = H.toHtml <$> v
    return $ userContainer $ formWrapper (userUpdateView v') userUpdateLink

updatedPassword :: UserUpdate -> Maybe Text
updatedPassword UserUpdate{..} = do
    one <- fst userUpdatePass
    two <- snd userUpdatePass
    if one == two then Just one else Nothing

applyUpdateUser :: UserUpdate -> User -> User
applyUpdateUser UserUpdate{..} u = u{ userName = userUpdateName
                                    , userEmail = userUpdateEmail
                                    }
