{-# LANGUAGE OverloadedStrings #-}
module Web.Login where

import Prelude
import Servant.API.ContentTypes
import Web.Common
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Html5 hiding (text)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Digestive
import Text.Digestive.Blaze.Html5 as F

getLoginPage :: Monad m => m Html
getLoginPage = do
    view <- getForm "login" loginForm
    return $ loginWrapper $ loginView (toHtml <$> view)

loginWrapper :: Html -> Html
loginWrapper x = do
    guestContainer $ H.form ! method "POST" ! action "/login" $ do
        x
        button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"

loginSuccessHtml :: Html
loginSuccessHtml = guestContainer $ H.div "Successfully logged in."

loginFailureHtml :: Html
loginFailureHtml = guestContainer $ H.div "Login unsuccessful."

logoutHtml :: Html
logoutHtml = guestContainer $ H.div "You have been logged out."

loginView :: View Html -> Html
loginView view = do
    legend "Login:"
    H.div ! class_ "form-group" $ do
        F.label "email" view "Email:"
        inputText "email" view ! class_ "form-control"
        errorList "email" view

    H.div ! class_ "form-group" $ do
        F.label "password" view "Password:"
        inputPassword "password" view ! class_ "form-control"
        errorList "password" view

loginForm :: Monad m => Form Text m Login
loginForm = Login
    <$> "email" .: check "email cannot be empty" notEmpty (text Nothing)
    <*> "password" .: check "password cannot be empty" notEmpty (text Nothing)
        where notEmpty = not . T.null

data Login = Login { loginEmail :: Text
                   , loginPass :: Text
                   } deriving (Show, Eq)
