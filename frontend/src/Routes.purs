module Routes where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, lookup)
import Data.Show (class Show, show)
import Routing (matches)
import Routing.Match (Match)
import Routing.Match.Class (lit, params)
import Control.Alt ((<|>))

data Locations
  = Home
  | Login (Map String String) -- Callback url for oauth2
  | User

derive instance genericLocations :: Generic Locations _
instance showLocations :: Show Locations where show = genericShow

oneSlash :: Match Unit
oneSlash = lit "/"

homeSlash :: Match Unit
homeSlash = lit ""

home :: Match Locations
home = Home <$ lit ""

login :: Match Locations
login = Login <$> (homeSlash *> lit "login" *> params)

user :: Match Locations
user = User <$ (homeSlash *> lit "user")

routing :: Match Locations
routing =
  login    <|>
  user     <|>
  home
