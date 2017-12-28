module Components.Login where

import Prelude (($))
import React (ReactClass, ReactElement, createClassStateless, createFactory)
import React.DOM as D
import React.DOM.Props as P
import Authenticators (Authenticator(..), Provider(..), authRequestUrl)
import Common (Host)

data LoginProps = LoginProps Host

loginComponent :: ReactClass LoginProps
loginComponent = createClassStateless loginText
  where
    loginText :: LoginProps -> ReactElement
    loginText (LoginProps host) = D.h1 [] [
        D.a [P.href provider ] [D.text "Login with google"]
      ]
      where provider = authRequestUrl $ Authenticator host Google


createLogin :: LoginProps -> ReactElement
createLogin props = D.div' [ createFactory loginComponent props ]
