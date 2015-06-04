{-# LANGUAGE OverloadedStrings,
             DeriveDataTypeable,
             DeriveGeneric #-}
module Main where

import Rest
import Rest.Api
import Data.Aeson
import qualified Data.JSON.Schema.Types as T
import Control.Monad.Reader
import Rest.Driver.Snap
import Generics.XmlPickler
import Data.Monoid
import GHC.Generics
import Data.Typeable
import Control.Monad
import Data.Functor
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Snap.Http.Server
import Text.XML.HXT.Arrow.Pickle
import System.Environment
import Snap.Util.FileServe
import qualified Snap.Core as Core
--import qualified Rest.Gen as Gen
--import qualified Rest.Gen.Config as Gen
import qualified Rest.Resource as R

postResource:: Resource IO (ReaderT String IO) String () Void
postResource = mkResourceReader 
     {
       R.name = "post"
     , R.schema = withListing () $ named [("title",singleBy id)]
     , R.list = const list
     , R.get = Just get
     }

data Post = Post {postTitle :: String, postContent :: String} deriving (Eq, Generic, Ord, Show, Typeable)


instance FromJSON Post where
  parseJSON (Object v) = Post <$>
                         v .: "post" <*>
                         v .: "content"
  parseJSON _ = mzero
instance ToJSON Post where
  toJSON (Post title content) = object ["title" .= title, "content" .= content]

instance T.JSONSchema Post where
  schema _ = T.Any

instance XmlPickler Post where xpickle = gxpickle
  
testPost::String -> IO Post
testPost _ = return $ Post "hi reggie" "strings are inneficient in haskell sorry guys lol"

get:: Handler (ReaderT String IO)
get = mkIdHandler (xmlJsonO) $ \_ gtitle -> go' gtitle
  where go':: (MonadIO m ) => String -> m Post 
        go' t =  liftIO $ readPostFromDB t

readPostFromDB:: String -> IO Post
readPostFromDB _ = return $ Post "test" "test"

list:: ListHandler IO
list = mkListing xmlJsonO $ \_ -> liftIO $ sequence [testPost "", testPost ""]

blog:: Router IO IO
blog = root -/ (route postResource)

api:: Api IO
api = [(mkVersion 1 0 0, Some1 blog)]

resthandle = apiToHandler' liftIO api

statichandle:: Core.Snap ()
statichandle = Core.route [("", serveDirectory "www")]

handle = statichandle <|> resthandle

main = do
  quickHttpServe handle
