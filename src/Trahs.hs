module Trahs (trahs) where

import Codec.Digest.SHA
import Control.Applicative
import Control.Monad
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.PosixCompat.Files
import System.Random
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.List as L
import qualified Data.Map.Strict as Map


-- | Replica ID of a local directory
type ReplicaID = Integer

-- | Version number of a file
type VersionNo = Integer

-- | Version vector
type Vector = Map.Map ReplicaID VersionNo

-- | Write stamp array
type StampVector = Map.Map FilePath ((ReplicaID, VersionNo), String)

-- | Command
data Cmd = DownloadRequest String
		 | DownloadReply String
		 | VectorRequest
		 | VectorReply String
		 | StampRequest
		 | StampReply String
		 | Switch
		 deriving Show

-- | Command for executing trahs on a remote system.  The '@' will be
-- replaced by the hostname, and the directory will be appended.
trassh :: String
trassh = "ssh -CTaxq @ ./trahs --server"


-- | Create trahs database directory if not exists.
ensureDir :: FilePath -> IO ()
ensureDir dir = createDirectoryIfMissing True dir


-- | Get the replica ID for a repository.
-- Replica ID is stored in .trahs.db/.id
getReplicaID :: FilePath -> IO ReplicaID
getReplicaID dir = do
	let filename = L.intercalate "/" [ dir, ".trahs.db", ".id" ]
	hasId <- doesFileExist filename
	if hasId
		then read <$> readFile filename
		else do
			rid <- abs <$> randomIO
			writeFile filename $ show rid
			return rid


-- | Get the version ID for a repository.
-- Version vector is stored in .trahs.db/.vec
getVector :: FilePath -> ReplicaID -> IO Vector
getVector dir rid = do
	let filename = L.intercalate "/" [ dir, ".trahs.db", ".vec" ]
	hasVec <- doesFileExist filename
	case hasVec of
		False -> return $ Map.singleton rid 1
		_ -> do
			m <- read <$> readFile filename
			let version = fromJust $ Map.lookup rid m
			return $ Map.insert rid (version + 1) m


-- | Scan the files in the repository
scanDirectory :: FilePath -> IO (Map.Map FilePath String)
scanDirectory dir = do
	let underDir = (++) (dir ++ "/")
	list <- getDirectoryContents dir
	filelist <- filterM (\x -> isRegularFile <$> getFileStatus (underDir x)) list
	hashlist <- mapM (hashFile . underDir) filelist
	return $ Map.fromList $ zip filelist hashlist
	where
		hashFile path = showBSasHex <$> (hash SHA256 <$> Lazy.readFile path)


-- | Get the writestamps for a repository.
-- Write stamp array is stored in .trahs.db/.ws
getWriteStamp :: FilePath -> ReplicaID -> VersionNo -> IO StampVector
getWriteStamp dir rid version = do
	hashmap <- scanDirectory dir
	let
	  filename = L.intercalate "/" [ dir, ".trahs.db", ".ws" ]
	  writestamp = (rid, version)
	hasVec <- doesFileExist filename
	case hasVec of
	  False ->
	  	return $ Map.fromList $ map (\(x, y)-> (x, (writestamp, y))) $ Map.toList hashmap
	  _ -> do
	  	oldmap <- read <$> readFile filename
	  	let
	  	  search key = fromJust $ Map.lookup key oldmap
	  	  (old, new) = (flip span) (Map.toList hashmap) (\(x, y) -> Map.member x oldmap && y == (snd $ search x))
	  	return $ Map.fromList $ (map (\(x, y) -> (x, (writestamp, y))) new) ++ (map (\(x, _) -> (x, search x)) old)	  


-- | Merge the state of local and remote repositories
mergeState :: Handle -> Handle -> FilePath -> Vector -> Vector -> StampVector -> StampVector -> IO StampVector
mergeState r w dir lvv rvv lws rws = do
	m <- foldM merge Map.empty $ L.nub $ Map.keys lws ++ Map.keys rws
	return m
	where
		getValue key mp = fromJust $ Map.lookup key mp
		version fn vec
			| Map.notMember fn vec = Nothing
			| otherwise = (Just . snd . fst) $ getValue fn vec
		replica fn vec
			| Map.notMember fn vec = Nothing
			| otherwise = (Just . fst . fst) $ getValue fn vec
		hashcode fn vec
			| Map.notMember fn vec = Nothing
			| otherwise = (Just . snd) $ getValue fn vec
		merge mp fn
			| onBoth && (hashcode fn lws) == (hashcode fn rws) = 
				return $ Map.insert fn val mp
			| onBoth && (version fn rws) <= (flip Map.lookup lvv $ fromJust $ replica fn rws) = 
				return $ Map.insert fn val mp
			| onBoth && (version fn lws) <= (flip Map.lookup rvv $ fromJust $ replica fn lws) = 
				do
					download r w dir fn
					return $ Map.insert fn (getValue fn rws) mp
			| onBoth = 
				do
					flagConflict r w dir fn
					return $ mp
			| onServer && (version fn rws > (flip Map.lookup lvv $ fromJust $ replica fn rws)) = 
				do
					download r w dir fn
					return $ Map.insert fn (getValue fn rws) mp
			| onClient && (version fn lws <= (flip Map.lookup rvv $ fromJust $ replica fn lws)) =
				do
					exist <- doesFileExist $ dir ++ "/" ++ fn
					case exist of
						True -> removeFile $ dir ++ "/" ++ fn
						_ -> return ()
					return $ mp
			| onClient = return $ Map.insert fn val mp 
			| otherwise = return $ mp
			where
				onClient = Map.member fn lws
				onServer = Map.member fn rws
				onBoth = onClient && onServer
				val = getValue fn lws


-- | Merge version vectors
mergeVector :: Vector -> Vector -> Vector
mergeVector lvv rvv = Map.fromListWith max $ Map.toList lvv ++ Map.toList rvv


-- | Strip command type off the contents 
stripCommand :: String -> String
stripCommand = takeWhile (/= '\"') . tail . dropWhile (/= '\"')


-- | @client@ downloads @fn@ from @server@ to @dir/fn@.
download :: Handle -> Handle -> FilePath -> FilePath -> IO ()
download r w dir fn = do
	hPutStrLn w $ show $ DownloadRequest fn
	msg <- hGetLine r
	case "DownloadReply" `L.isPrefixOf` msg of
		False -> hPutStrLn stderr "Unexpected command when downloading"
		_ -> do
			f <- openFile (dir ++ "/" ++ fn) WriteMode
			hPutStrLn f $ stripCommand msg
			hClose f
			return ()


-- | @client@ requests for vector from @server@
reqVector :: Handle -> Handle -> IO Vector
reqVector r w = do
	hPutStrLn w $ show VectorRequest
	msg <- hGetLine r
	case "VectorReply" `L.isPrefixOf` msg of
		False -> do
			hPutStrLn stderr "Unexpected command when requesting vector"
			return Map.empty
		_ -> return $ read $ stripCommand msg


-- | @client@ requests for write stamp from @server@
reqStamp :: Handle -> Handle -> IO StampVector
reqStamp r w = do
	hPutStrLn w $ show StampRequest
	msg <- hGetLine r
	case "StampReply" `L.isPrefixOf` msg of
		False -> do
			hPutStrLn stderr "Unexpected command when requesting stamp"
			return Map.empty
		_ -> return $ read $ stripCommand msg



-- | @client@ flags conflict 
flagConflict :: Handle -> Handle -> FilePath -> FilePath -> IO ()
flagConflict _ _ _ _ = hPutStrLn stderr "Conflict"


-- | Dump synchronization states into database on disk 
storeState :: FilePath -> ReplicaID -> Vector -> StampVector -> IO ()
storeState dir rid vec ws = do
	ensureDir $ newdb
	writeFile idfile $ show rid
	writeFile vecfile $ show vec
	writeFile wsfile $ show ws
	removeDB olddb
	renameDirectory olddb newdb
	where
		newdb = dir ++ "/.trahs.db~"
		olddb = dir ++ "/.trahs.db"
		idfile = newdb ++ "/.id"
		vecfile = newdb ++ "/.vec"
		wsfile = newdb ++ "/.ws"
		removeDB db = do
			forM_ (map ((++) (db ++ "/")) [ ".id", ".vec", ".ws "])
				$ (\x -> do
					exist <- doesFileExist x 
					case exist of
						True -> removeFile x
						_ -> return ())
			removeDirectory db


-- |
clientSync :: Handle -> Handle -> FilePath -> IO ()
clientSync r w dir = do
	ensureDir $ dir ++ "/.trahs.db"
	replica <- getReplicaID dir
	lvv <- getVector dir replica
	lws <- getWriteStamp dir replica $ fromJust $ Map.lookup replica lvv
	rvv <- reqVector r w
	rws <- reqStamp r w
	newlws <- mergeState r w dir lvv rvv lws rws
	let newlvv = mergeVector lvv rvv
	storeState dir replica newlvv newlws
	return ()


-- |
serverLoop :: Bool -> ReplicaID -> Vector -> StampVector -> Handle -> Handle -> FilePath -> IO ()
serverLoop turn rid vector stamp r w dir = do
	request <- hGetLine r
	case words request of
		("DownloadRequest":fn:_) -> do
			content <- readFile $ dir ++ "/" ++ stripCommand fn
			hPutStrLn w . show $ DownloadReply content
			serverLoop turn rid vector stamp r w dir
		("VectorRequest":_) -> do 
			hPutStrLn w . show $ VectorReply $ show vector
			serverLoop turn rid vector stamp r w dir
		("StampRequest":_) -> do
			hPutStrLn w . show $ StampReply $ show stamp
			serverLoop turn rid vector stamp r w dir
		("Switch":_) -> do
			when turn $ client False r w dir
		_ -> hPutStrLn stderr "Unrecognized command"


-- | @server real r w dir@ runs the code to serve the contents of @dir@,
-- reading input from @r@ and writing it to @w@.
server :: Bool -> Handle -> Handle -> FilePath -> IO ()
server turn r w dir = do
	ensureDir $ dir ++ "/.trahs.db"
	rid <- getReplicaID dir
	vector <- getVector dir rid
	stamp <- getWriteStamp dir rid . fromJust $ Map.lookup rid vector
	storeState dir rid vector stamp
	hPutStrLn stderr "haha"
	serverLoop turn rid vector stamp r w dir


-- | @client turn r w dir@ runs the client to update @dir@ based on
-- the remote contents.  Commands for the remote server are written to
-- @w@, while replies are read from @r@.  If @turn@, then when done
-- the client should attempt to swap roles and run the protocol in the
-- other direction (uploading any changes to the other side).
-- Otherwise, if @turn@ is false, @client@ should simply return when
-- done.
client :: Bool -> Handle -> Handle -> FilePath -> IO ()
client turn r w dir = do
	clientSync r w dir
	hPutStrLn w $ show Switch
	when turn $ server False w r dir


hostCmd :: String -> FilePath -> IO String
hostCmd host dir = do
  tmpl <- maybe trassh id <$> lookupEnv "TRASSH"
  case break (== '@') tmpl of
    (b, '@':e) -> return $ b ++ host ++ e ++ ' ':dir
    _          -> return $ tmpl ++ ' ':dir


spawnRemote :: String -> FilePath -> IO (Handle, Handle)
spawnRemote host dir = do
  cmd <- hostCmd host dir
  hPutStrLn stderr $ "running " ++ show cmd
  (Just w, Just r, _, _) <- createProcess (shell cmd) {
        std_in = CreatePipe
      , std_out = CreatePipe
    }
  hSetBuffering w LineBuffering
  return (r, w)


connect :: String -> FilePath -> FilePath -> IO ()
connect host rdir ldir = do
  (r, w) <- spawnRemote host rdir
  client True r w ldir


trahs :: IO ()
trahs = do
  args <- getArgs
  case args of
    ["--server", l] -> do hSetBuffering stdout LineBuffering
                          server True stdin stdout l
    [r, l] | (host, ':':rdir) <- break (== ':') r -> connect host rdir l
    _ -> do hPutStrLn stderr "usage: trahs HOST:DIR LOCALDIR"
            exitFailure

