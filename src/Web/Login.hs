{-# LANGUAGE OverloadedStrings #-}
module Web.Login where

import API
import Caltrops.Client
import Web.Common
import Web.Bootstrap
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Html5 hiding (text)
import qualified Text.Blaze.Html5 as H
import Text.Digestive

loginPage :: Monad m => m Html
loginPage = do
    view <- getForm "login" loginForm
    return $ guestContainer $ formWrapper (loginView (toHtml <$> view))
                                          loginLink

loginSuccessHtml :: Html
loginSuccessHtml = userContainer $ H.div "Successfully logged in."

loginFailureHtml :: Html
loginFailureHtml = guestContainer $ H.div "Login unsuccessful."

logoutHtml :: Html
logoutHtml = guestContainer $ H.div "You have been logged out."

loginView :: View Html -> Html
loginView view = do
    legend "Login:"
    bootstrapFormFieldInput "email" "Email:" view
    bootstrapFormFieldPassword "password" "Password:" view

loginForm :: Monad m => Form Text m Login
loginForm = Login
    <$> "email" .: check "email cannot be empty" notEmpty (text Nothing)
    <*> "password" .: check "password cannot be empty" notEmpty (text Nothing)
        where notEmpty = not . T.null

