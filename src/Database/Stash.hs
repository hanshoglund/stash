
{-# LANGUAGE ConstraintKinds, TupleSections #-}

module Database.Stash (
    Stashable(..),
    Meta,
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


type Meta = Data.Aeson.Value
emptyMeta = Data.Aeson.Null

put     :: Stashable a => Meta -> a -> IO ()
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

get     :: Stashable a => Meta -> IO (Maybe a)
get k = fmap (listToMaybe) $ find (== k)


find    :: Stashable a => (Meta -> Bool) -> IO [a]
find = fmap (fmap (\(_,_,x)->x)) . find'

find'    :: Stashable a => (Meta -> Bool) -> IO [(Meta,String,a)]
find' p = do
  stashSubDirectories <- fmap (filter ((> 11) . length)) $ -- get rid of . .. .DS_Store etc
    System.Directory.getDirectoryContents stashPath

  -- TODO warn on type error
  fmap catMaybes $ forM stashSubDirectories $ \subDirectoryName -> do
    let dirPath = stashPath ++ "/" ++ subDirectoryName
    _meta <- SByteString.readFile (dirPath++"/"++"meta.json")
    _type <- SByteString.readFile (dirPath++"/"++"type.json")
    _data <- ByteString.readFile (dirPath++"/"++"data.json")

    let k = (dec _meta :: Data.Aeson.Value)
    let ok  = p k
    let da  = (Data.Aeson.decode $ _data)
    let ty  = getS (dec _type)
    let res = (join $ if ok then Just (fmap (k,ty,) da) else Nothing)
    return res

getType :: Meta -> IO (Maybe String)
getType k = fmap (listToMaybe) $ findType (== k)

findType :: (Meta -> Bool) -> IO [String]
findType = fmap (fmap snd) . findType'

findType' :: (Meta -> Bool) -> IO [(Meta,String)]
findType' p = do
  stashSubDirectories <- fmap (filter ((> 11) . length)) $ -- get rid of . .. .DS_Store etc
    System.Directory.getDirectoryContents stashPath

  -- TODO warn on type error
  fmap catMaybes $ forM stashSubDirectories $ \subDirectoryName -> do
    let dirPath = stashPath ++ "/" ++ subDirectoryName
    _meta <- SByteString.readFile (dirPath++"/"++"meta.json")
    _type <- SByteString.readFile (dirPath++"/"++"type.json")
    _data <- ByteString.readFile (dirPath++"/"++"data.json")

    let k = (dec _meta :: Data.Aeson.Value)
    let ok  = p $ (dec _meta :: Data.Aeson.Value)
    -- TODO
    let ty  = getS (dec _type)
    let res = (if ok then Just (k, ty) else Nothing)
    return res

-- TODO hide
getS (Data.Aeson.String x) = Data.Text.unpack x
    -- print $ _meta
    -- print $ _data
    -- print $ ok
    -- print $ da
    -- print $ res
    -- putStrLn ""
    -- putStrLn ""


dec :: SByteString.ByteString -> Data.Aeson.Value
-- dec = fromJust . Data.Aeson.decode
dec = fromEither . Data.Attoparsec.ByteString.parseOnly Data.Aeson.Parser.value

fromEither (Right x) = x

-- wrapKey :: Meta -> Meta
-- wrapKey = Data.Aeson.Array . return
--
-- unwrapKey = Data.Vector.head . getArray
--   where
--     getArray (Data.Aeson.Array x) = x

-- /.stash/
--   /items/
--     1a238a9aa289a898/ -- hash of meta
--       data.json -- JSON value
--       type.json -- JSON string which is the TypeRep
--       meta.json -- JSON string which is the TypeRep
--

-- js :: String -> Data.Aeson.Value
-- js = fromJust . Data.Aeson.decode . Data.String.fromString
-- 
-- put' k = put (js k)
-- get' k = get (js k)
fromJust (Just x) = x
-- fromRes (Data.Aeson.Success x) = x

stashPath = "/Users/hans/.stash"

-- ---------
-- -- in stash dir, add all, git commit, git pull, git push
-- sync :: IO ()
--
--
--
-- ///////
--
--
-- type Conf = () -- TODO
-- type Stash a = ReaderT Conf STA a
--
-- class (ToJSON a, FromJSON a, Typeable a) => Stashable a where
--   makePreview :: a -> FilePath -> IO Bool
--
-- -- A 128-bit sum or something
-- type Index -- Eq, Ord
--
-- -- Put something in stash, generating a unique index
-- stash :: Stashable a => a -> Stash Index
-- -- Get and remove something from stash
-- unstash :: Stashable a => Index -> Stash a
-- -- Get and put back something to stash
-- peekStash :: Stashable a => Index -> Stash a
--
-- -- View all currently stashed items
-- items :: Stash [(Index, TypeRep)]
-- -- Write a png file containing a thumbnail of the given stashed item to the given path.
-- -- Return whether successful
-- preview :: Index -> FilePath -> Stash Bool
--
--
-- type Name = String
-- collectIndices :: Name -> [Index] -> Stash ()
-- uncollectIndices :: Name -> Stash [Index]
-- peekCollectIndices :: Name -> Stash [Index]
-- -- Combination with stash
-- collect :: Stashable a => Name -> [a] -> Stash ()
-- uncollect :: Stashable a => Name -> Stash [a]
-- peekCollect :: Stashable a => Name -> Stash [a]
--
-- -- Same thing, using single-value collections
-- name :: Stashable a => Name -> a -> Stash a
-- unname :: Stashable a => Name -> Stash [Index]
-- peekName :: Stashable a => Name -> Stash a
--
-- -- TODO various configurations: zip files, gitetc
-- backupStash :: Stash ()
--
--
