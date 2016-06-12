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

  -- * Lazy 'Text'
  LText,

  -- * Conversion
  -- $conversion
  toStrict, toLazy, toBuilder, toString,

  -- * Showing
  -- $showing

  -- ** Strict 'Text'
  show, show',
  -- ** Lazy 'Text'
  lshow, lshow',
  -- ** 'Builder'
  bshow, bshow',

  -- * Formatting
  -- $formatting
  module Data.Text.Format,
  module Data.Text.Buildable,
  format, lformat, bformat,

  -- * 'Builder'
  Builder,
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


type LText = TL.Text

{- $showing

'show' is a fast variant of @show@ for 'Text' \/ 'Builder' that only works for some types – it's very fast for 'Int', etc, but doesn't work for types that you have defined yourself. (If you want more instances, import <https://hackage.haskell.org/package/text-show-instances text-show-instances>.)

'show'' is a shortcut for @pack.show@ that works for everything with a 'Show' instance but is slower.
-}

show :: TextShow a => a -> Text
show = showt
{-# INLINE show #-}

lshow :: TextShow a => a -> LText
lshow = showtl
{-# INLINE lshow #-}

bshow :: TextShow a => a -> Builder
bshow = showb
{-# INLINE bshow #-}

show' :: Show a => a -> Text
show' = pack . P.show
{-# INLINE show' #-}

lshow' :: Show a => a -> LText
lshow' = TL.pack . P.show
{-# INLINE lshow' #-}

bshow' :: Show a => a -> Builder
bshow' = B.fromString . P.show
{-# INLINE bshow' #-}

{- $formatting

'format' is a function similar to @printf@ in spirit. Don't forget to enable @OverloadedStrings@ if you want to use it!

>>> format "{}+{}={}" (2, 2, 4)
"2+2=4"

If you have only one argument, use a list:

>>> format "2+2={}" [4]
"2+2=4"

There are some formatting options available:

>>> format "123 = 0x{}, pi = {}" (hex 123, fixed 5 pi)
"123 = 0x7b, pi = 3.14159"

For more formatters, see "Data.Text.Format".
-}

format :: Params ps => Format -> ps -> Text
format f = TL.toStrict . Format.format f
{-# INLINE format #-}

lformat :: Params ps => Format -> ps -> LText
lformat = Format.format
{-# INLINE lformat #-}

bformat :: Params ps => Format -> ps -> Builder
bformat = Format.build
{-# INLINE bformat #-}

{- $conversion
These functions can convert from strict\/lazy 'Text', 'Builder', and 'String'.
-}

class ToStrict t where
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
