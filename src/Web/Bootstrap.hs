{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Web.Bootstrap where

import Data.Text (Text)
import Text.Digestive
import qualified Text.Digestive.Blaze.Html5 as F
import qualified Data.Text as T
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

role :: AttributeValue -> Attribute
role = customAttribute "role"

dataToggle :: AttributeValue -> Attribute
dataToggle = dataAttribute "toggle"

ariaExpanded :: Bool -> Attribute
ariaExpanded = customAttribute "aria-expanded" . toTruth

toTruth :: Bool -> AttributeValue
toTruth t = if t then "true" else "false"

liDropdown :: Html -> Html -> Html
liDropdown toggle lst = li ! class_ "dropdown" $ do
    a ! href "#" ! class_ "dropdown-toggle" ! role "button"
      ! dataToggle "dropdown" ! ariaExpanded False $ do
        toggle
        ul ! class_ "dropdown-menu" ! role "menu" $
            lst

liIcon :: AttributeValue -> Text -> Text -> Html
liIcon hrf icn txt = li $ a ! href hrf $ faIcon icn $ toHtml txt

faIcon :: Text -> Html -> Html
faIcon icn htmel = do
    i ! class_ (toValue $ "fa fa-" `T.append` icn) $ mempty
    htmel

bootstrapFormField :: (Text -> View Html -> Html) -> Text -> Html -> View Html -> Html
bootstrapFormField inp fname tle view =
    H.div ! class_ "form-group" $ do
        F.label fname view tle
        inp fname view ! class_ "form-control"
        F.errorList fname view

bootstrapFormFieldInput :: Text -> Html -> View Html -> Html
bootstrapFormFieldInput = bootstrapFormField F.inputText

bootstrapFormFieldPassword :: Text -> Html -> View Html -> Html
bootstrapFormFieldPassword = bootstrapFormField F.inputPassword
