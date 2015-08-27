{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import Control.Failure
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Configurator
import Data.Default
import Data.Foldable
import Data.Scientific
import Data.String
import Data.Text (Text, breakOn, isPrefixOf, stripPrefix, takeWhile)
import Debug.Trace
import Network.HTTP.Client hiding(method)
import Network.HTTP.Types hiding(GET)
import Network.OAuth.OAuth2
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HashMap
import qualified Network.HTTP.Client as Client

------------------------------------------------------------------------------
import Application

baseUri = "http://cloud.feedly.com" :: ByteString
streamIdsPath = "/v3/streams/ids"
entriesPath = "/v3/entries/.mget"

handleEntries :: Handler App App ()
handleEntries = method GET getter
  where
    getter :: Handler App App ()
    getter = do
          let aaa = traceShowId "One"
          f <- gets _feedly
          let token = AccessToken (fcAccessToken f) Nothing Nothing Nothing
          mgr <- liftIO $ newManager defaultManagerSettings
          eIds <- liftIO $ getIds mgr token (fcUserId f) 100 -- Magic number: num records retrieved from server.
          eEntries <- liftIO $ getEntriesWrap eIds mgr token
          writeLBS $ either id encode eEntries
          return ()


getEntriesWrap :: Either L.ByteString [Text] -> Manager -> AccessToken ->
                  IO (Either L.ByteString [EntryResponse])
getEntriesWrap (Left l) _ _ = return $ Left l
getEntriesWrap (Right ids) mgr token = getEntries mgr token ids

parseUrlBS :: (Failure HttpException m, MonadThrow m) => ByteString -> m Network.HTTP.Client.Request
parseUrlBS bs = parseUrl s
  where s = C8.unpack bs

getEntries :: Manager -> AccessToken -> [Text] -> IO (Either L.ByteString [EntryResponse])
getEntries mgr token ids = do
  let (uri, postData) = entryDataUriAndPostData ids
  req <- parseUrlBS uri
  let req' = (updateRequestHeaders (Just token) req) {
        Client.method = "POST",
        requestBody = RequestBodyLBS postData
        }
  let qqq = traceShowId req'
  rsp <- authRequest req' id mgr
  return $ parseResponseJSON rsp

entryDataUriAndPostData :: [Text] -> (ByteString, L.ByteString)
entryDataUriAndPostData xs = (uri, postData)
  where uri = baseUri `mappend` entriesPath
        postData = encode xs

data EntryResponse = EntryResponse {
  entryId :: Text,
  visualUrl :: Maybe Text,
  categories :: [Text],
  content :: Maybe Text,
  title :: Maybe Text,
  published :: Maybe Integer
} deriving (Show, Eq)

instance FromJSON EntryResponse where
  parseJSON (Object o) = EntryResponse
                         <$> (o .: "id")              -- entryId
                         <*> ((o .:? "visual")
                              >>= (\a -> case a of       -- visualUrl
                                          Just a -> a .:? "url"
                                          Nothing -> return Nothing))
                         
                         <*> return []
                         -- <*> ((o .:? "categories")
                         --      >>= (\a -> case a of       -- categories
                         --                  Just (Array arr) ->
                         --                    return $ grabField "label" arr
                         --                  Nothing -> mzero))
                         
                         <*> ((o .:? "summary")
                              >>= (\a -> case a of       -- content
                                          Just a -> a .:? "content"
                                          Nothing -> return Nothing))
                         <*> (o .: "title")
                         <*> (o .: "published")
  parseJSON _ = mzero

instance ToJSON EntryResponse where
  toJSON e = object [ "entryId" .= entryId e,
                      "categories" .= categories e,
                      "content" .= content e,
                      "imageUrl" .= entryBestUrl id e,
                      "title" .= title e
                    ]

grabField :: Text -> Array -> [Text]
grabField label arr = foldl' (\acc v -> case v of
                               (Object o) -> if HashMap.member label o
                                             then case (o HashMap.! label) of
                                                   (String txt) -> txt : acc
                                                   _ -> acc
                                             else acc
                               _ -> acc)
                      [] arr
                      
entryBestUrl :: (Text -> Text) -> EntryResponse -> Maybe Text
entryBestUrl f e = let raw = maybe contentUrl useVisualUrl vu
                         where vu = do
                                 u <- visualUrl e
                                 if isPrefixOf "http" u
                                   then return u
                                   else Nothing
                               useVisualUrl = Just
                               contentUrl = getContentUrl e
                   in f <$> raw

getContentUrl :: EntryResponse -> Maybe Text
getContentUrl e = do
  let prefix = "src=\""
  c <- content e
  let (_, rest) = breakOn prefix c
  u <- stripPrefix prefix rest
  return $ Data.Text.takeWhile (/= '"') u
  
getIds :: Manager -> AccessToken -> ByteString -> Int -> IO (Either L.ByteString [Text])
getIds mgr token uid num =
  return . either Left (Right . ids) =<< authGetJSON mgr token (savedIdsUri uid num)

savedIdsUri :: ByteString -> Int -> ByteString
savedIdsUri userid count = uri `mappend` params
  where uri = baseUri `mappend` streamIdsPath
        savedTag = savedTagPrefix `mappend` userid `mappend` savedTagSuffix
        savedTagPrefix = "user/" :: ByteString
        savedTagSuffix = "/tag/global.saved" :: ByteString
        params = renderSimpleQuery True [ ("count", fromString $ show count),
                                          ("streamId", savedTag) ]

data IdsResponse = IdsResponse {
  continuation :: Text,
  ids :: [Text]
} deriving (Show, Eq)

instance FromJSON IdsResponse where
  parseJSON (Object o) = IdsResponse
                         <$> o .: "continuation"
                         <*> o .: "ids"
  parseJSON _ = mzero



------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
  -- REST handlers
  ("entry",     handleEntries),

  -- HTML/CSS/JS
  ("static",    serveDirectory "static"),
  -- Bootstrap CSS
  ("bootstrap", serveDirectory "assets/lib/bootstrap-bower/css"),
  -- index.html
  ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
  
  s <- nestSnaplet "sess" sess $
       initCookieSessionManager "site_key.txt" "sess" (Just 3600)

  -- NOTE: We're using initJsonFileAuthManager here because it's easy and
  -- doesn't require any kind of database server to run.  In practice,
  -- you'll probably want to change this to a more robust auth backend.
  a <- nestSnaplet "auth" auth $
       initJsonFileAuthManager defAuthSettings sess "users.json"
  addRoutes routes

  conf <- getSnapletUserConfig
  accessToken <- liftIO $ require conf "favsrv.accessToken"
  userId <- liftIO $ require conf "favsrv.userId"
  return $ App s a (FeedlyConfig accessToken userId)

