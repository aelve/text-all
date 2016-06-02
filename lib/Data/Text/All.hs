module Data.Text.All
(
  -- * Standard modules from text
  module Data.Text,
  module Data.Text.IO,
  module Data.Text.Encoding,

  -- * Lazy text
  toStrict, fromStrict,

  -- * Showing
  show, show',

  -- * Formatting
  module Data.Text.Format,
  module Data.Text.Buildable,
  format,

  -- * 'Builder'
  Builder,
  fromBuilder,
)
where


import Data.Text
import Data.Text.IO
import Data.Text.Encoding
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Lazy.Builder (Builder)

import TextShow hiding (Builder)

import Data.Text.Format hiding (format, print, hprint)
import Data.Text.Format.Params
import Data.Text.Buildable (Buildable)
import qualified Data.Text.Format as Format
import qualified Prelude as P
import Prelude hiding (show)


-- | A fast variant of 'show' for 'Text' that only works for some types. If you want more instances, import <https://hackage.haskell.org/package/text-show-instances text-show-instances> or use 'show'' if the type is your own and you only have a 'Show' instance defined.
show :: TextShow a => a -> Text
show = showt

-- | Like 'show', but works for anything that has a 'Show' instance.
show' :: Show a => a -> Text
show' = pack . P.show

-- | A formatting function that mimics 'Data.Text.Format.format', but produces strict 'Text'.
format :: Params ps => Format -> ps -> Text
format f = toStrict . Format.format f

fromBuilder :: Builder -> Text
fromBuilder = toStrict . toLazyText
