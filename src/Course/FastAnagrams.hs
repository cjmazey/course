{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S
import qualified Data.Map as M


sort :: Ord a
     => List a -> List a
sort Nil = Nil
sort (h :. t) =
  sort (filter (< h) t) ++
  (h :. Nil) ++
  sort (filter (>= h) t)

-- Create an anagram map from a file.  The file should be a dictionary
-- containing one word per line, e.g., "/usr/share/dict/cracklib-small."
makeMap :: Filename -> IO (M.Map Chars (List Chars))
makeMap f =
  foldRight (\w m ->
               M.insertWith (++)
                            (sort w)
                            (w :. Nil)
                            m)
            M.empty .
  lines <$>
  readFile f

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams :: Chars -> Filename -> IO (List Chars)
fastAnagrams s f =
  M.findWithDefault (s :. Nil)
                    ((sort . (toLower <$>)) s) <$>
  makeMap f

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
