module Data.Text.Lazy.All
(
  module Data.Text.Lazy,
  module Data.Text.Lazy.IO,
  module Data.Text.Lazy.Encoding,
  module Data.Text.Format,
  module Data.Text.Buildable,
  show, show',
  fromBuilder,
)
where


import Data.Text.Lazy
import Data.Text.Lazy.IO
import Data.Text.Lazy.Encoding
import Data.Text.Lazy.Builder

import TextShow

import Data.Text.Format hiding (print, hprint)
import Data.Text.Buildable (Buildable)
import qualified Prelude as P
import Prelude hiding (show)


-- | A fast variant of 'show' for 'Text' that only works for some types. If you want more instances, import <https://hackage.haskell.org/package/text-show-instances text-show-instances> or use 'show'' if the type is your own and you only have a 'Show' instance defined.
show :: TextShow a => a -> Text
show = showtl

-- | Like 'show', but works for anything that has a 'Show' instance.
show' :: Show a => a -> Text
show' = pack . P.show

fromBuilder :: Builder -> Text
fromBuilder = toLazyText
