Stanford CS240h Lab 3
=====================

In this lab, you will build a multi-way file synchronizer called
`trahs`. This is will be a simplified version of the [tra] file
synchronizer described in
[File Synchronization with Vector Time Pairs][traTR] by Rus Cox and
William Josephson.

We are providing a skeleton Cabal project to help get started,
download it from
[here](http://www.scs.stanford.edu/14sp-cs240h/labs/lab3.tar.gz).

## No Lost Updates

Your task will be to synchronize all the files in a single directory
across multiple machines respecting the *no lost updates* rule.  At a
high level, the no lost updates rule considers the update history of a
file.  It says that when synchronizing two versions of a file, $F_1$
and $F_2$, it is okay to discard version $F_1$ and replace it with
version $F_2$ only if $F_2$'s history is a superset of $F_1$'s.  In
other words, none of the updates made to $F_1$ are being discarded.
More specifically:

* If a file has changed on only one machine since the last
  synchronization, then the change should be propagated to the other
  machine.
* If a file has been deleted on only one machine (and remains
  unchanged on the other), then the deletion should be propagated to
  the other machine.
* If a file has changed on both machines, then you must detect this
  and report a conflict, as neither version can replace the other.
* If a file has changed on one machine (`A`) and been deleted on the
  other (`B`), this is also a kind of conflict.  For this lab, you
  will automatically resolve such conflicts by propagating the changed
  file from machine `A` to machine `B`.  (Put another way, the only
  "updates" it is permissible to lose are file deletions when there is
  a conflicting update.)

## Example Session

For example, here is how your program might behave when synchronizing
between two machines, garage (directory `market-test`) and market
(directory `cs240h/test`).

Initially, garage is empty while market has two files, `fileA` and
`fileB`:

```
market 1$ cd cs240h/test
market 2$ ls
fileA fileB
market 3$ cat fileA
content a
market 4$ cat fileB
content b
```

We run a first session, bringing garage and market into sync:

```
garage 753$ trahs market:cs240h/test market-test
fetching "fileA"
fetching "fileB"
=== switching from client to server ===
```

The output above the `=== switching from ...` line is for the commands
required to update garage (the client), while output below is for the
commands required to update market.  This denotes the fact that after
pulling all changes from market, the trahs process on garage has
switched rules to allow market to pull any changes.  (In this case
there are no changes to send to market.)

Now that they are in sync, we make some updates:

```
garage 754$ echo contents of file c > market-test/fileC
garage 755$ echo more contents for file b >> market-test/fileB 
```

We run `trahs` again to sync the changes from garage to market:

```
garage 756$ trahs market:cs240h/test market-test
=== switching from client to server ===
fetching "fileB"
fetching "fileC"
```

Note that now the changes are being reported by market, which has
fetched `fileB` and `fileC` from garage.  Now, we create a conflict
and delete some files:

```
garage 757$ echo create conflict > market-test/fileA 
garage 758$ rm market-test/fileB market-test/fileC 
garage 759$ ssh market
Last login: Fri Apr 25 12:28:06 2014 from ...
market 1$ echo more content >> cs240h/test/fileA
market 2$ echo more content >> cs240h/test/fileB
market 3$ logout
```

After command 3 on market in the above transcript, `fileA` has been
modified on both machines, `fileB` has been modified on market and
deleted on garage, and `fileC` has been deleted on garage.

Next, we re-run `trahs` to bring garage and market back into sync:

```
garage 760$ trahs market:cs240h/test market-test
conflicting "fileA"
fetching "fileB"
=== switching from client to server ===
deleting "fileA"
fetching "fileA#3702144909419514571.6"
fetching "fileA#9192274446083366835.5"
deleting "fileC"
```

This should leave us with a directory as so:

```
garage 761$ ls -al market-test/
total 16
drwxr-xr-x  2 dm   dm    120 Apr 25 12:29 .
drwxrwxrwt 54 root root 1680 Apr 25 12:30 ..
-rw-r--r--  1 dm   dm    721 Apr 25 12:29 .trahs.db
-rw-r--r--  1 dm   dm     16 Apr 25 12:29 fileA#3702144909419514571.6
-rw-r--r--  1 dm   dm     23 Apr 25 12:29 fileA#9192274446083366835.5
-rw-r--r--  1 dm   dm     48 Apr 25 12:29 fileB
garage 762$ cat market-test/fileB
content b
more contents for file b
more content
garage 763$ 
```

After synchronization, the following is the state of the file system:

* There are now two versions of `fileA`. Hence, `fileA` itself is
  gone, and the two conflicting versions are called
  `fileA#3702144909419514571.6` and `fileA#9192274446083366835.5`.
  Those two number signify that one version was found on replica
  `3702144909419514571` when that replica was at version 6, and the
  other was found on replica `9192274446083366835` when that replica
  was at version 5.

* Though `fileB` was deleted on garage, it was also modified on
  market.  Though this is a conflict, we simply resolve the conflict
  by overruling the deletion, and now market's version of `fileB`
  exists on garage.

* `fileC` was deleted on garage. Moreover, the particular version
  deleted on garage contained all the changes known by market.  Hence
  the deletion is propagated to market, and the file no longer exists.

Though this example shows only two hosts, your synchronizer should
work with an arbitrary number of hosts. Moreover, pairwise
synchronizations should be possible between all pairs of replicas in
any order. For instance, if A synchronizes with B and then B
synchronizes with C, the results should be the same as if A
synchronized with C directly (after A learns all changes on B, of
course).

## Details

The main challenge in file synchronization is deciding, when the state
of two files differs, whether there is an update conflict, or whether
one version of the file is based on the other and hence supersedes it.
In the latter case, of course you also need to figure out which
version is newer. To keep track of this information, you will need to
store some extra synchronization state in either a file or directory
with the special reserved name `.trahs.db`.

As described in this section, synchronization happens in one direction
from a server to a client. To achieve bidirectional synchronization,
your program should end by flipping the protocol around and performing
the same actions in the other direction.

### Overall architecture

The `trahs` command should take exactly two command-line arguments:

    trahs SERVER:SERVER-DIR CLIENT-DIR

Once it finishes running, `CLIENT-DIR` on the client and `SERVER-DIR`
on machine `SERVER` should have the same contents.

You should write `trahs` with the expectation that an identical copy
of `trahs` will be available on the server.  `trahs` should use `ssh`
to run the copy of itself on the server.  To make finding `trahs`
easier, we recommend putting a link to your build directory into your
home directory, e.g.:

    $ ln -s ~/cs240h/trahs/dist/build/trahs/trahs ~/trahs

On the server, you will likely want `trahs` to run in a special server
mode, which you might indicate with a special command-line option,
`--server`, as follows:

    ./trahs --server SERVER-DIR

### Synchronization state

Synchronization data needed on each server should be stored in the
directory that is being synchronized as either a file or subdirectory
using the special reserved name `.trahs.db`.

Each replica will need to store the following information for a
synchronized directory:

* A unique *replica ID*.  The first time `trahs` is run on a particular
  directory, it should randomly generate a replica ID for itself. The
  replica ID should then never change.

* A *local version number* that starts at 1 and increments every time
  `trahs` is run.

* For each file, a *write stamp* consisting of replica ID and version
  number of the file's last update.  The replica ID corresponds to the
  replica that created this version of the file; the version number is
  that replica's local version number at the time `trahs` saw that
  version of the file.

* For each file, a way to determine if the file has been locally
  modified since the last time `trahs` was run.  One way to do this is
  to store a collision-resistant (SHA-256) hash of the file's
  contents.  Another way is to store both the size and modification
  time of the file.  Yet another approach is to combine the previous
  two:  Track file contents by SHA-256 hash, but also store size and
  mtime and only bother re-hashing a file if one of the other two
  values has changed.

* A *version vector* reflecting how up to date the local replica is
  with respect to other replicas.  More specifically, a version vector
  is a map from replica ID to version number.  A replica's own replica
  ID always maps to its latest local version number.  (Hence, you do
  not actually need to store the local version number separately.)
  Conceptually, any replica IDs not in the version vector is mapped to
  version number 0.  After synchronizing from a remote replica, the
  local replica should set its version vector to the element-wise
  maximum of old local version vector and the remote version vector.

### Synchronization algorithm

One-way synchronization from a server to a client proceeds in four
phases.  First, both sides bump their local version numbers and scan
their local directories to discover any files that have changed.
Second, the server sends its current state to the client.  Third, the
client merges the server's state into its local directory, downloading
any missing files from the server.  Fourth, the client updates its
version vector.

The first phase consists of finding modified files in the local
directory.  `trahs` must read the directory and compare each file to
the hash and/or size+mtime information last recorded.  If the file has
changed, `trahs` sets the file's write stamp to the local replica ID
and version number (as the changes are not reflected on any other
replica).

In the second phase, the server simply sends the client its version
vector and a list of (file name, writestamp) pairs describing the
contents of the directory.  (Besides a writestamp, the per-file
information can be augmented with other information such as SHA-256
hashes.  You may find it simplest just to dump the server's entire
database to the client.)

In the third phase, the client merges the remote server state into its
own local state.  This is the heart of the algorithm, and it makes use
of four pieces of information:

* The local (client's) version vector.  Let's call it $LVV$.  We'll
  use the notation $LVV!R$ to refer to the version number of replica
  $R$ in $LVV$.  If $R$ does not appear in $LVV$, then $LVV!R = 0$.
* The remote (server's) version vector.  Let's call it $RVV$.
  We define $RVV!R$ analogously to $LVV!R$.
* For each file that exists locally, the local writestamp.  Let's call
  it *LWS*.  If the file does not exist locally, there will be no
  LWS.  we write $replica(LWS)$ and $version(LWS)$ for the two
  components of the writestamp.
* For each file that exists remotely, the remote writestamp.  Let's
  call it $RWS$, and similarly access the two fields as $replica(RWS)$
  and $version(RWS)$.

Now for each file we proceed by cases:

* If the file exists on both the client and server and $LWS = RWS$,
  there is nothing to do.

* If the files differ, but $version(RWS)\le LVV!replica(RWS)$, then
  the client already learned about the server's version in some
  previous synchronization and subsequently overwrote it.  Hence, the
  client ignores the server's version and keeps its own with no
  change.

* Conversely, if $version(LWS)\le RVV!replica(LWS)$, then the server
  knew about and overwrote the client's version.  Hence the client
  downloads the new version from the server, replaces the local file
  with the contents of the remote one, and also replaces the local
  writestamp with the remote one in the synchronization state
  ($LWS\gets RWS$).

* If the file exists on both replicas and none of the above cases
  holds, flag a conflict.

* If the file exists on neither the client nor server (deleted or
  never created on both), there is obviously nothing to do.

* If the file exists only on the server, then download it only if the
  client has not previously downloaded that version or a version that
  supersedes it (i.e., download only if $version(RWS) >
  LVV!replica(RWS)$).  Otherwise, ignore the file as it was previously
  downloaded and deleted.

* If the file exists only on the client, then delete it only if the
  server previously had the client's version of the file or a version
  derived from it.  In other words, delete only if $version(LWS)\le
  RVV!replica(LWS)$.

In the fourth phase, the client sets its version vector to contain the
replica-wise maximum of its previous contents and the values in the
remote server's version vector.  This ensures that $\forall
R. LVV!R\ge RVV!R$, reflecting the fact that the client now knows
everything the server knows.

### Comparison to `tra`

This assignment has several simplifications compared to the work
described in the [tra paper](traTR).  You only need to synchronize a
single directory and can ignore anything (including symbolic links)
that is not a regular file.

Another simplification is that `tra` keeps a second writestamp
corresponding to file creation.  This allows one to differentiate
between a deletion that conflicts with an update and one that doesn't.
But since we always resolve deletion conflicts by superseding the
deletion event, the extra information is unnecessary for `trahs`.

## Hints and suggestions

The instructor was able to complete the assignment in approximately
250 lines of Haskell (not counting comments), without using any
language extensions.  Based on that implementation, here are some
suggestions that may help you out.  These are not requirements.

### Libraries and idioms

In addition to base, you may find the following packages useful:
bytestring, containers, directory, filepath, process, random, SHA2,
unix-compat.

Module `System.PosixCompat` in the [unix-compat] package has useful
functions for file attributes, and has the advantage of working on
both Unix/Linux and other operating systems.  (It just reexports the
unix package on Unix-like systems.)

You will also likely find the use of `Data.Map` or `Data.Map.Strict`
in the [containers library][containers] quite handy, both to represent
version vectors and to store per-file information.

You will likely want to call

~~~~ {.haskell}
  hSetBuffering h LineBuffering
~~~~

on file handles `h` used for communication between the client and
server (including `stdout` on the server).  Otherwise, data you write
to the peer `trahs` process may get buffered, and your process may get
stuck.

Using lazy IO to operate on whole files will make certain operations
very concise.  For example, the following function computes the
SHA-256 hash of a file, but by virtue of lazy IO does not actually
need to store the whole file in memory:

~~~~ {.haskell}
import Codec.Digest.SHA
import Control.Applicative
import qualified Data.ByteString.Lazy as L

hashFile :: FilePath -> IO String
hashFile path = showBSasHex <$> (hash SHA256 <$> L.readFile path)
~~~~

Similar tricks are useful when copying an entire file to or from a
handle connected to a peer `trahs`.

### Protocol

A simple text-based protocol is easiest to debug.  For instance, you
might have a command to retrieve the server's state, and another
command to fetch a file.

Each command can end with a newline so you can read commands with
`hGetLine`.  To avoid duplicating code on the client and server, you
can have a special command `TURN` that causes the server to become the
client.  (But obviously be careful not to execute such a command more
than once, or you will go into an infinite loop and never finish
synchronizing.)

Make sure you send all your diagnostics to stderr (with `hPutStrLn
stderr ...`).  Otherwise, diagnostic output on the server may get
interleaved with your protocol and confuse `trahs` or corrupt files.

### Simplifying assumptions

In order to simplify the problem, feel free to make the following
assumptions:

* You can safely reserve some file (or directory) name to store
  synchronization information.  For example, you might store all your
  synchronization information in the same directory in a file called
  `.trahs.db`.  And when updating the file (for crash recoverability),
  you might write new versions of the state to a file called
  `.trahs.db~` and then [rename] `.trahs.db~` to `trahs.db`.
  (Obviously your program then needs to skip these files when
  synchronizing.)

* You only need to synchronize the regular files in a single
  directory, as determined by [`isRegularFile`].  You can ignore
  subdirectories, devices, pipes, sockets, and even symbolic links.
  Make sure you call [`getSymbolicLinkStatus`] and not
  [`getFileStatus`] to detect symbolic links.

* You can assume the `trahs` executable (or a symbolic link to it) is
  in your home directory on all machines.  Thus, executing `ssh -CTaxq
  HOST ./trahs --server ARGS...` is sufficient to connect you to a
  `trahs` server process on `HOST`.

* You can assume filename lengths are nowhere near the limit, so it is
  okay to append version numbers and such to file names when reporting
  conflicts.

* You do not need to worry about modifications being made to the
  directory at either end while your program is running.

* If a file's size and time returned by [`modificationTime`] have not
  changed between two invocations of `trahs`, you can assume the
  content of the file has not changed either.

* Assume there are few enough replicas that if you generate a random
  ~63-bit integer to identify each replica, the probability of two
  replicas ending up with the same ID is negligible.

* Since you need shell access to run `trahs` anyway, you do not need
  to worry about either end of a connection being malicious.  You can
  furthermore avoid any protocol version negotiation and just assume
  both ends are running the exact same version of the `trahs` program.

* Assume directories are small enough that it is fine to store all
  synchronization state in memory, to read and write it to disk all at
  once, and to exchange a complete list of all files on every
  synchronization event

* It is fine to use `show` and `read` to serialize and unserialize
  synchronization state for both the database and the network
  protocol.  Though `show` output is not very compact, it has the big
  advantage of being human readable, which will help you debug.

* Since you are ignoring directories, you don't need to worry about
  conflicts between files and directories (i.e., a file name that is a
  file on one replica and a directory on another).

* You do not need to worry about synchronizing file permissions, such
  as the execute bit.

### Getting two `trahs` processes communicating

To help you get to the interesting part of the assignment as soon as
possible, here is an example of how to get two `trahs` processes
communicating over ssh.  On the client, this code checks for an
environment variable `TRASSH`, which should contain an `@` character.
The name of the host is then substituted for the `@` character, and
the directory is appended.  By default, if there is no `TRASSH`
environment variable, it uses:

    ssh -CTaxq @ ./trahs --server

Using the default, if you execute `./trahs server:server-dir
client-dir`, the code will spawn:

    ssh -CTaxq server ./trahs --server server-dir

which is what you want if you put a symbolic link to `trahs` in your
home directory as recommended above.

~~~~ {.haskell}
module Main where

import Control.Applicative
import System.Environment
import System.Exit
import System.Process
import System.IO

-- | Command for executing trahs on a remote system.  The '@' will be
-- replaced by the hostname, and the directory will be appended.
trassh :: String
trassh = "ssh -CTaxq @ ./trahs --server"

-- | @server r w dir@ runs the code to serve the contents of @dir@,
-- reading input from @r@ and writing it to @w@.
server :: Handle -> Handle -> FilePath -> IO ()
server r w dir = do
  hPutStrLn w "I am the server"
  line <- hGetLine r
  -- If the command asked us to switch roles, then at this point we
  -- would run client False r w dir here.  Otherwise want to process
  -- command and keep looping.
  hPutStrLn w $ "You said " ++ line
  
-- | @client turn r w dir@ runs the client to update @dir@ based on
-- the remote contents.  Commands for the remote server are written to
-- @w@, while replies are read from @r@.  If @turn@, then when done
-- the client should attempt to swap roles and run the protocol in the
-- other direction (uploading any changes to the other side).
-- Otherwise, if @turn@ is false, @client@ should simply return when
-- done.
client :: Bool -> Handle -> Handle -> FilePath -> IO ()
client turn r w dir = do
  line <- hGetLine r
  hPutStrLn stderr $ "The server said " ++ show line
  hPutStrLn w "Hello, server"
  line' <- hGetLine r
  hPutStrLn stderr $ "The server said " ++ show line'
  -- At the end, if turn == True, then we issue some command to swap
  -- roles and run server r w dir.

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
                          server stdin stdout l
    [r, l] | (host, ':':rdir) <- break (== ':') r -> connect host rdir l
    _ -> do hPutStrLn stderr "usage: trahs HOST:DIR LOCALDIR"
            exitFailure
~~~~


## Due Date

Lab 3 should be submitted by the start of class (12:50pm) on
*Thursday, May 8th*.  However, you can have a free extension to
midnight if you show up to lecture on time.  We encourage you to
complete the lab sooner, however, so as to work on your projects.

You have 48 hours of late days for the three labs. They are consumed
in 24 hour blocks and are used automatically. After they are used,
you'll have the maximum grade you can receive for a late lab reduced
by 25% each day.

## cabal -- build & test tool

cabal is the standard build and packaging tool for haskell. a starting
framework is provided for you. you can find the user guide for cabal
[here][cabal].

## provided files

the files provided to get started are:

* trahs.cabal -- specifies the build system.
* Main.hs -- main module, functions as the executable for trahs
* src/Trahs.hs -- the trahs library, you should edit this file to
  implement lab3.
* test/Test.hs-- the test harness. you need to edit this and add your
  own tests!

if you add any new files, please make sure to add them into
`trahs.cabal` as otherwise they won't be packaged up when you run
`cabal sdist` and then they won't be sumitted.

The reason for this separation of `Main.hs` and `src/Trah.hs` is that
it allows us to compile all your code as a library so that we can
import it into our test framework with cabal and avoid recompiling it
for both building and testing.

## building lab 3

To get up and running (using cabal), issue the following commands:

        cabal sandbox init

This will initiate a self-contained build environment where any
dependencies you need are installed locally in the current directory.
This helps avoid the haskell equivalent of "dll hell!"  If your
version of cabal is older such that it doesn't have the `sandbox`
command, then just proceed without it and it should all be fine.

Next, you want to build the lab. for that, issue the following
commands:

        cabal install --only-dependencies --enable-tests
        cabal configure --enable-tests
        cabal build

After that, you should also be able to run the test harness simply by
typing:

        cabal test

and you'll get some pretty output!

## testing lab 3

Some skeleton code for a test framework is provided in `test/Test.hs`.
you'll need to edit it to add your own tests.  The test framework uses
a haskell package called [hspec](http://hspec.github.io/).  Please
refer to it for documentation on how to use it.

We also strongly encourage the use of
[quick check](http://hackage.haskell.org/package/quickcheck)!  It can
be integrated with the test driver that hspec provides, please see the
skeleton code that has an example of how.

## Grading

While we strongly encourage you to take testing seriously and write a
comprehensive test suite, we are only going to grade you on the
`trahs` executable itself.

Grading will be just done on functionality but we will try to give
feedback on your coding style.

## Submission instructions

First, simply type:

        cabal sdist

This will generate a tar file of your code in `dist/trahs.tar.gz`.

Then go to [upload.ghc.io](http://upload.ghc.io/) and submit your work
through the online form. You can resubmit as many times as you want up
until the deadline.

If you have any trouble submitting on-line, then please email
<img id="email" class="img-responsive"
style="display:inline-block;padding-bottom:4px;" width=280 height=10
src="../staff.png" alt="staff email">.

[rename]: http://hackage.haskell.org/package/unix-compat-0.4.1.1/docs/System-PosixCompat-Files.html#v:rename
[`isRegularFile`]: http://hackage.haskell.org/package/unix-compat-0.4.1.1/docs/System-PosixCompat-Files.html#v:isRegularFile
[`getSymbolicLinkStatus`]: http://hackage.haskell.org/package/unix-compat-0.4.1.1/docs/System-PosixCompat-Files.html#v:getSymbolicLinkStatus
[`getFileStatus`]: http://hackage.haskell.org/package/unix-compat-0.4.1.1/docs/System-PosixCompat-Files.html#v:getFileStatus
[`modificationTime`]: http://hackage.haskell.org/package/unix-compat-0.4.1.1/docs/System-PosixCompat-Files.html#v:modificationTime
[tra]: http://swtch.com/tra/
[traTR]: http://publications.csail.mit.edu/tmp/MIT-CSAIL-TR-2005-014.pdf
[containers]: http://hackage.haskell.org/package/contns
[unix-compat]: http://hackage.haskell.org/package/unix-compat
[cabal]: http://www.haskell.org/cabal/users-guide/developing-packages.html#test-suites
