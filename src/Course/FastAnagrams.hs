{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams s f =
  let p :: S.Set NoCaseString
      p = (S.fromList . hlist) (NoCaseString <$> permutations s)
      d :: IO (S.Set NoCaseString)
      d = (S.fromList . hlist) <$> ((NoCaseString <$>) <$> lines <$> readFile f)
  in (ncString <$>) <$> ((listh . S.toList) <$> (S.intersection p <$> d))

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString

instance Ord NoCaseString where
  compare = compare `on` map toLower . ncString
