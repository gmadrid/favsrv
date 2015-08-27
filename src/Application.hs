{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Data.ByteString
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Session

------------------------------------------------------------------------------
data FeedlyConfig = FeedlyConfig {
  fcAccessToken :: ByteString,
  fcUserId      :: ByteString
  }

data App = App {
  _sess   :: Snaplet SessionManager,
  _auth   :: Snaplet (AuthManager App),
  _feedly :: FeedlyConfig
  }

makeLenses ''App


------------------------------------------------------------------------------
type AppHandler = Handler App App


