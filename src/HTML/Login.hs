{-# LANGUAGE OverloadedStrings #-}
module HTML.Login where

import Prelude
import Servant.API.ContentTypes
import Html.Common
import qualified Data.Text as T
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

loginHtml :: Html
--loginHtml r = let login = toValue $ Uri UrlUserLogin [("r", r)] in
loginHtml = let login = "/login" in
    guestContainer $ H.form ! method "POST" ! action login $ do
        legend "Login:"
        H.div ! class_ "form-group" $ do
            H.label ! for "loginFormName" $ "Username"
            input ! type_ "text" ! class_ "form-control" ! A.id "authname"
                  ! name "loginFormName" ! placeholder "your username"
        H.div ! class_ "form-group" $ do
            H.label ! for "loginFormPass" $ "Password"
            input ! type_ "password" ! class_ "form-control" ! A.id "authpass"
                  ! name "loginFormPass" ! placeholder "your password"

        button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"

loginSuccessHtml :: Html
loginSuccessHtml = guestContainer $ H.div "Successfully logged in."

loginFailureHtml :: Html
loginFailureHtml = guestContainer $ H.div "Login unsuccessful."

logoutHtml :: Html
logoutHtml = guestContainer $ H.div "You have been logged out."

instance FromFormUrlEncoded LoginForm where
    fromFormUrlEncoded [("loginFormName", name), ("loginFormPass", pass)] =
        Right $ LoginForm (T.unpack name) (T.unpack pass)
    fromFormUrlEncoded t = Left $ "malformed login: " ++ show t

data LoginForm = LoginForm { loginFormName :: String
                           , loginFormPass :: String
                           } deriving (Show, Eq)
