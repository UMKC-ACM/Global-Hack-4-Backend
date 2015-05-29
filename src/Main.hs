{-# LANGUAGE OverloadedStrings,
             DeriveDataTypeable,
             DeriveGeneric #-}
module Main where

import Rest
import Rest.Api
import Data.Aeson
import qualified Data.JSON.Schema.Types as T
import Control.Monad.Reader
import Rest.Driver.Happstack
import Generics.XmlPickler
import Data.Monoid
import GHC.Generics
import Data.Typeable
import Control.Monad
import Data.Functor
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Happstack.Server.SimpleHTTP
import Text.XML.HXT.Arrow.Pickle
import System.Environment
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

handle:: ServerPartT IO Response
handle = apiToHandler' liftIO api

myParseConfig:: [String] -> Either String Conf
myParseConfig args = if (length args == 1) then Right (portConf ( read $ args !! 0)) else Left "Specify port"

portConf inPort = Conf { port = inPort, validator = Nothing, logAccess = Just logMAccess, timeout = 30}

main = do
  --args <- getArgs
  --if (length args > 0) then do -- if there are args we want fancy output
   --config <- Gen.configFromArgs "test-backend"
   --Gen.generate config "TestBackend" api [] [] []
   --else
   args <- getArgs
   let config = myParseConfig args
   spawnServer config	
	where   spawnServer (Left str) = print str
		spawnServer (Right conf) = simpleHTTP conf handle		

