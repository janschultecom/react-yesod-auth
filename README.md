This is a minimalistic template for starting a SPA using Purescript/React in the frontend and Haskell/Yesod as the backend.
Authentication is done using OAuth+JWT (not yet implemented -- coming soon).

# Getting Started

## Backend

Start
```
cd backend
stack install yesod-bin --install-ghc
stack build
stack exec -- yesod devel
```

## Frontend

Setup

1. Create your Google clientId by following the steps here:  https://developers.google.com/identity/protocols/OAuth2
2. Copy frontend/.env.example to frontend/.env
3. Set your Google clientId in frontend/.env and change host/ports if necessary

Start
```
cd frontend
bower install
npm install
npm install xhr2
npm run webpack-dev-server
open http://localhost:4008
```

# Notes

* The frontend (index.html, javascripts, css, etc.) is hosted by webpack/node. Imho this is the way to go, because serving it from the backend just makes everything more complicated and doesn't integrate that seamlessly (e.g. source maps, hot reload, etc.).
* The backend template is based on yesod-minimal and therefore contains a bit of boilerplate. I chose this because it contains some development facilities like hot reloading. yesod-simple doesn't and yesod-sqlite has too much useless stuff.  
