module Data.Text.Builder.All
(
  module Data.Text.Lazy.Builder,
  module Data.Text.Format,
  module Data.Text.Buildable,
  show, show',
  format,
)
where


import Data.Text.Lazy.Builder

import TextShow

import Data.Text.Format hiding (format, print, hprint)
import Data.Text.Format.Params
import Data.Text.Buildable (Buildable)
import qualified Prelude as P
import Prelude hiding (show)


-- | A fast variant of 'show' for 'Builder' that only works for some types. If you want more instances, import <https://hackage.haskell.org/package/text-show-instances text-show-instances> or use 'show'' if the type is your own and you only have a 'Show' instance defined.
show :: TextShow a => a -> Builder
show = showb

-- | Like 'show', but works for anything that has a 'Show' instance.
show' :: Show a => a -> Builder
show' = fromString . P.show

-- | A formatting function that mimics 'Data.Text.Format.format', but produces a 'Builder'.
format :: Params ps => Format -> ps -> Builder
format f = build f
