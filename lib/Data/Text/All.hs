{-# LANGUAGE
GADTs,
TypeSynonymInstances
  #-}


{- |
Here are the nice things from text that you get (thanks to a restrictive lower bound) but that aren't documented elsewhere in this module:

* The 'T.takeWhileEnd' function.
* An instance for @Semigroup@.
* An instance for @printf@ (i.e. you can use a 'Text' as one of @printf@'s arguments).
* An instance for @Binary@.
-}
module Data.Text.All
(
  -- * Standard modules from text
  module Data.Text,
  module Data.Text.IO,
  module Data.Text.Encoding,

  -- * Types
  LText,
  Builder,

  -- * Showing
  show, lshow, bshow,
  -- ** Via 'Show'
  show', lshow', bshow',

  -- * Conversion
  toStrict, toLazy, toBuilder, toString,

  -- * Formatting
  module Data.Text.Format,
  module Data.Text.Buildable,
  format, lformat, bformat,

  -- * Builder
  bsingleton,
  flush,
)
where


import Data.Text
import Data.Text.IO
import Data.Text.Encoding
import qualified Data.Text.Lazy.Builder as B
import Data.Text.Lazy.Builder (Builder)

import qualified Data.Text.Lazy as TL

import TextShow hiding (Builder, toString)

import Data.Text.Format hiding (format, print, hprint, build)
import Data.Text.Format.Params
import Data.Text.Buildable
import qualified Data.Text.Format as Format
import qualified Prelude as P
import Prelude hiding (show)


-- | Lazy 'TL.Text'.
type LText = TL.Text

-- | A fast variant of 'show' for 'Text' that only works for some types. If you want more instances, import <https://hackage.haskell.org/package/text-show-instances text-show-instances> or use 'show'' if the type is your own and you only have a 'Show' instance defined.
show :: TextShow a => a -> Text
show = showt
{-# INLINE show #-}

lshow :: TextShow a => a -> LText
lshow = showtl
{-# INLINE lshow #-}

bshow :: TextShow a => a -> Builder
bshow = showb
{-# INLINE bshow #-}

-- | Like 'show', but works for anything that has a 'Show' instance. Slower than 'show'.
show' :: Show a => a -> Text
show' = pack . P.show
{-# INLINE show' #-}

lshow' :: Show a => a -> LText
lshow' = TL.pack . P.show
{-# INLINE lshow' #-}

bshow' :: Show a => a -> Builder
bshow' = B.fromString . P.show
{-# INLINE bshow' #-}

-- | A variant of 'Data.Text.Format.format' that produces strict 'Text'.
format :: Params ps => Format -> ps -> Text
format f = TL.toStrict . Format.format f
{-# INLINE format #-}

-- | A variant of 'Data.Text.Format.format' that produces lazy 'Text'.
lformat :: Params ps => Format -> ps -> LText
lformat = Format.format
{-# INLINE lformat #-}

-- | A variant of 'Data.Text.Format.format' that produces a 'Builder'.
bformat :: Params ps => Format -> ps -> Builder
bformat = Format.build
{-# INLINE bformat #-}

class ToStrict t where
    -- | Convert a 'String', lazy 'Text', or 'Builder' into a strict 'Text'.
    toStrict :: t -> Text
instance (a ~ Char) => ToStrict [a] where
    toStrict = pack
    {-# INLINE toStrict #-}
instance ToStrict LText where
    toStrict = TL.toStrict
    {-# INLINE toStrict #-}
instance ToStrict Builder where
    toStrict = TL.toStrict . B.toLazyText
    {-# INLINE toStrict #-}

class ToLazy t where
    -- | Convert a 'String', strict 'Text', or 'Builder' into a lazy 'Text'.
    toLazy :: t -> LText
instance (a ~ Char) => ToLazy [a] where
    toLazy = TL.pack
    {-# INLINE toLazy #-}
instance ToLazy Text where
    toLazy = TL.fromStrict
    {-# INLINE toLazy #-}
instance ToLazy Builder where
    toLazy = B.toLazyText
    {-# INLINE toLazy #-}

class ToBuilder t where
    -- | Convert a 'String', strict 'Text', or lazy 'Text' into a 'Builder'.
    toBuilder :: t -> Builder
instance (a ~ Char) => ToBuilder [a] where
    toBuilder = B.fromString
    {-# INLINE toBuilder #-}
instance ToBuilder Text where
    toBuilder = B.fromText
    {-# INLINE toBuilder #-}
instance ToBuilder LText where
    toBuilder = B.fromLazyText
    {-# INLINE toBuilder #-}

class ToString t where
    -- | Convert a strict 'Text', lazy 'Text', or 'Builder' into a 'String'.
    toString :: t -> String
instance ToString Text where
    toString = unpack
    {-# INLINE toString #-}
instance ToString LText where
    toString = TL.unpack
    {-# INLINE toString #-}
instance ToString Builder where
    toString = TL.unpack . B.toLazyText
    {-# INLINE toString #-}

-- | A 'Builder' producing a single character.
bsingleton :: Char -> Builder
bsingleton = B.singleton
{-# INLINE bsingleton #-}
