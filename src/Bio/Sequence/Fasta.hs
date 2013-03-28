{- |
   Module: Bio.Sequence.Fasta

   This module incorporates functionality for reading and writing
   sequence data in the Fasta format.
   Each sequence consists of a header (with a '>' prefix)
   and a set of lines containing the sequence data..
-}

module Bio.Sequence.Fasta
    ( Sequence(..)
    -- * Reading and writing plain FASTA files
    , readFasta, writeFasta, hReadFasta, hWriteFasta
    -- * Counting sequences in a FASTA file
    , countSeqs
    -- * Helper function for reading your own sequences
    , mkSeqs
    -- * Other
    , toStr, seqid, seqheader, seqdata, seqlength
) where


import Data.Char (chr, isSpace)
import Data.List (groupBy, intersperse)
import System.IO

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BB
import Data.ByteString.Lazy.Char8 (ByteString)

import Bio.Core.Sequence

splitsAt :: Offset -> ByteString -> [ByteString]
splitsAt n s = let (s1,s2) = B.splitAt (unOff n) s
               in if B.null s2 then [s1] else s1 : splitsAt n s2

data Sequence = Seq SeqLabel SeqData (Maybe QualData)
                deriving (Show, Eq)

instance BioSeq Sequence where
  seqid     (Seq lab seq mqual) = SeqLabel {unSL = B.takeWhile (/= ' ') $ unSL lab}
  seqheader (Seq lab seq mqual) = lab
  seqdata   (Seq lab seq mqual) = seq
  seqlength (Seq lab seq mqual) = Offset {unOff = B.length $ unSD seq}

toStr :: SeqData -> String
toStr  = B.unpack . unSD

-- | Lazily read sequences from a FASTA-formatted file
readFasta :: FilePath -> IO [Sequence]
readFasta f = (mkSeqs . B.lines) `fmap` B.readFile f

-- | Write sequences to a FASTA-formatted file.
--   Line length is 60.
writeFasta :: FilePath -> [Sequence] -> IO ()
writeFasta f ss = do
  h <- openFile f WriteMode
  hWriteFasta h ss
  hClose h

-- | Lazily read sequence from handle
hReadFasta :: Handle -> IO [Sequence]
hReadFasta h = (mkSeqs . B.lines) `fmap` B.hGetContents h

-- | Write sequences in FASTA format to a handle.
hWriteFasta :: Handle -> [Sequence] -> IO ()
hWriteFasta h = mapM_ (wFasta h)

wHead :: Handle -> SeqLabel -> IO ()
wHead h l = do
   B.hPut h $ B.pack ">"
   B.hPut h (unSL l)
   B.hPut h $ B.pack "\n"

wFasta :: Handle -> Sequence -> IO ()
wFasta h (Seq l d _) = do
  wHead h l
  let ls = splitsAt 60 (unSD d)
  mapM_ (B.hPut h) $ intersperse (B.pack "\n") ls
  B.hPut h $ B.pack "\n"

-- | Convert a list of FASTA-formatted lines into a list of sequences.
--   Blank lines are ignored.
--   Comment lines start with "#" are allowed between sequences (and ignored).
--   Lines starting with ">" initiate a new sequence.
mkSeqs :: [ByteString] -> [Sequence]
mkSeqs = map mkSeq . blocks

mkSeq :: [ByteString] -> Sequence
mkSeq (l:ls) = Seq (SeqLabel (B.drop 1 l))
               (SeqData (B.filter (not . isSpace) $ B.concat $ takeWhile isSeq ls))
               Nothing
               where isSeq s = (not . B.null) s &&
                               (flip elem (['A'..'Z']++['a'..'z']) . B.head) s
mkSeq []     = error "empty input to mkSeq"

-- | Split lines into blocks starting with '>' characters
--   Filter out # comments (but not semicolons?)
blocks :: [ByteString] -> [[ByteString]]
blocks = groupBy (const (('>' /=) . B.head))
         . filter ((/='#') . B.head)
         . dropWhile (('>' /=) . B.head)
         . filter (not . B.null)

countSeqs :: FilePath -> IO Int
countSeqs f = do
  ss <- B.readFile f
  let hdrs = filter (('>'==).B.head) $ filter (not . B.null) $ B.lines ss
  return (length hdrs)
