module Authenticators where

import Prelude ((<>))
import Process.Env (authenticatorGoogle)
import Common (Host(..))

data Provider = Google
data Authenticator = Authenticator Host Provider

authRequestUrl :: Authenticator -> String
authRequestUrl (Authenticator (Host host) Google) = "https://accounts.google.com/o/oauth2/v2/auth?\
 \scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email&\
 \access_type=offline&\
 \include_granted_scopes=true&\
 \state=state_parameter_passthrough_value&\
 \redirect_uri=" <> host <> "%2F?login=google&\
 \response_type=code&\
 \client_id=" <> authenticatorGoogle
