module Web.Form where

import Data.Text (Text)
import Text.Blaze.Html5 hiding (text)
import Text.Digestive as D

getFormResult :: Monad m
              => Text -> Form v m a -> [(Text, Text)] -> m (View v, Maybe a)
getFormResult name frm fields = postForm name frm $ fromKVPairs fields

emptyForm :: Monad m => Text -> Form Text m a -> (View Html -> Html) -> m Html
emptyForm name frm viewf = do
   view <- D.getForm name frm
   return $ viewf (toHtml <$> view)

fromKVPairs :: Monad m => [(Text, Text)] -> b -> m (Env m)
fromKVPairs = const . return . keyValueEnv

keyValueEnv :: Monad m => [(Text, Text)] -> Env m
keyValueEnv fields path = return $
    case lookup (fromPath path) fields of
        Nothing -> []
        Just t  -> [TextInput t]
