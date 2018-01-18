module Authentication.OAuth2 where


import Data.Text (Text)

data OAuthKeys = OAuthKeys
    { oauthKeysClientId :: Text
    , oauthKeysClientSecret :: Text
    }
