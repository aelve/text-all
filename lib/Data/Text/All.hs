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
  strictToLazy, lazyToStrict,

  -- * Formatting
  module Data.Text.Format,
  module Data.Text.Buildable,
  format, lformat, bformat,

  -- * Builder
  bsingleton,
  flush,
  -- ** Conversion
  builderToStrict, builderToLazy,
  strictToBuilder, lazyToBuilder,
)
where


import Data.Text
import Data.Text.IO
import Data.Text.Encoding
import qualified Data.Text.Lazy.Builder as B
import Data.Text.Lazy.Builder (Builder)

import qualified Data.Text.Lazy as TL

import TextShow hiding (Builder)

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

-- | Convert a 'Text' into a lazy 'TL.Text'.
strictToLazy :: Text -> LText
strictToLazy = TL.fromStrict
{-# INLINE strictToLazy #-}

-- | Convert a lazy 'TL.Text' into a 'Text'.
lazyToStrict :: LText -> Text
lazyToStrict = TL.toStrict
{-# INLINE lazyToStrict #-}

-- | Convert a 'Builder' into a 'Text'.
builderToStrict :: Builder -> Text
builderToStrict = TL.toStrict . B.toLazyText
{-# INLINE builderToStrict #-}

-- | Convert a 'Builder' into a lazy 'TL.Text'.
builderToLazy :: Builder -> LText
builderToLazy = B.toLazyText
{-# INLINE builderToLazy #-}

-- | Convert a 'Text' into a 'Builder'.
strictToBuilder :: Text -> Builder
strictToBuilder = B.fromText
{-# INLINE strictToBuilder #-}

-- | Convert a lazy 'TL.Text' into a 'Builder'.
lazyToBuilder :: LText -> Builder
lazyToBuilder = B.fromLazyText
{-# INLINE lazyToBuilder #-}

-- | A 'Builder' producing a single character.
bsingleton :: Char -> Builder
bsingleton = B.singleton
{-# INLINE bsingleton #-}
