
{-# LANGUAGE ConstraintKinds, TupleSections, BangPatterns #-}

{-|


@
  /.stash/
      81273888827172/   -- A Hash of meta
        data.json       -- JSON value
        type.json       -- JSON string which is the TypeRep
        meta.json       -- JSON string which is the TypeRep
@
-}
module Database.Stash (
    Stashable(..),
    Key,
    put,
    get,
    find,
    find',
    getType,
    findType,
    findType',    
) where

{-# LANGUAGE ConstraintKinds #-}
import           Control.Monad        (forM, join)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Maybe           (listToMaybe)
import           Data.Maybe           (catMaybes)
import           Data.Typeable        (Typeable)
import qualified Data.Aeson
import qualified Data.Aeson.Parser
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString as SByteString
import qualified Data.Hashable
import qualified Data.String
import qualified Data.Typeable
import qualified Data.Vector
import qualified System.Directory
import qualified Data.Attoparsec.ByteString
import qualified Data.Text 
type Stashable a = (ToJSON a, FromJSON a, Typeable a, Show a)
-- class (ToJSON a, FromJSON a, Typeable a            , Show a) => Stashable a where

-- instance Stashable () -- TODO
-- instance Stashable Int -- TODO
-- instance Stashable Double -- TODO
-- instance Stashable Char -- TODO
-- instance Stashable a => Stashable [a] -- TODO
-- instance (Stashable a, Stashable b) => Stashable (a, b) -- TODO

{-
TODO Only works if key and value are arrays and objects (in their JSON representation!)
-}


type Key = Data.Aeson.Value
emptyKey = Data.Aeson.Null

put     :: Stashable a => Key -> a -> IO ()
put k v = do
  let meta_ = Data.Aeson.encode $ k -- need to wrap to match JSON spec!
  let type_ = Data.Aeson.encode $ Data.Aeson.String $ Data.String.fromString $ show $ Data.Typeable.typeOf v -- Just for sanity at the moment
  let data_ = Data.Aeson.encode v
  let hash  = Data.Hashable.hash k
  let dirPath = stashPath ++ "/" ++ (tail $ show hash) -- get rid of - sign
  System.Directory.createDirectoryIfMissing True stashPath -- i.e: ~/.stash
  System.Directory.createDirectoryIfMissing True dirPath   -- i.e: ~/.stash/2271273
  ByteString.writeFile (dirPath++"/"++"meta.json") meta_
  ByteString.writeFile (dirPath++"/"++"type.json") type_
  ByteString.writeFile (dirPath++"/"++"data.json") data_
  return ()

get     :: Stashable a => Key -> IO (Maybe a)
get k = fmap (listToMaybe) $ find (== k)


find    :: Stashable a => (Key -> Bool) -> IO [a]
find = fmap (fmap (\(_,_,x)->x)) . find'

find'    :: Stashable a => (Key -> Bool) -> IO [(Key,String,a)]
find' p = do
  stashSubDirectories <- fmap (filter ((> 11) . length)) $ -- get rid of . .. .DS_Store etc
    System.Directory.getDirectoryContents stashPath

  -- TODO warn on type error
  fmap catMaybes $ forM stashSubDirectories $ \subDirectoryName -> do
    let dirPath = stashPath ++ "/" ++ subDirectoryName
    !_meta <- SByteString.readFile (dirPath++"/"++"meta.json")
    !_type <- SByteString.readFile (dirPath++"/"++"type.json")
    _data <- SByteString.readFile (dirPath++"/"++"data.json")

    let k = (dec _meta :: Data.Aeson.Value)
    let ok  = p k
    let da  = (Data.Aeson.decodeStrict $ _data)
    let ty  = getS (dec _type)
    let res = (join $ if ok then Just (fmap (k,ty,) da) else Nothing)
    return res

getType :: Key -> IO (Maybe String)
getType k = fmap (listToMaybe) $ findType (== k)

findType :: (Key -> Bool) -> IO [String]
findType = fmap (fmap snd) . findType'

findType' :: (Key -> Bool) -> IO [(Key,String)]
findType' p = do
  stashSubDirectories <- fmap (filter ((> 11) . length)) $ -- get rid of . .. .DS_Store etc
    System.Directory.getDirectoryContents stashPath

  -- TODO warn on type error (as opposed to decode error/key not present etc)
  fmap catMaybes $ forM stashSubDirectories $ \subDirectoryName -> do
    let dirPath = stashPath ++ "/" ++ subDirectoryName
    !_meta <- SByteString.readFile (dirPath++"/"++"meta.json")
    !_type <- SByteString.readFile (dirPath++"/"++"type.json")

    let k = (dec _meta :: Data.Aeson.Value)
    let ok  = p $ (dec _meta :: Data.Aeson.Value)
    let ty  = getS (dec _type)
    let res = (if ok then Just (k, ty) else Nothing)
    return res

getS :: Key -> String
getS (Data.Aeson.String x) = Data.Text.unpack x

dec :: SByteString.ByteString -> Data.Aeson.Value
dec = fromEither . Data.Attoparsec.ByteString.parseOnly Data.Aeson.Parser.value

fromEither (Right x) = x
fromJust (Just x) = x

-- TODO configuration file determines this (or find out HOME dir somehow)
stashPath = "/Users/hans/.stash"

