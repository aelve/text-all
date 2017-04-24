module Data.Text.Lazy.All
(
  -- * Standard modules from text
  module Data.Text.Lazy,
  module Data.Text.Lazy.IO,
  module Data.Text.Lazy.Encoding,

  -- * Lazy text functions
  show, format,
)
where


import Data.Text.Lazy
import Data.Text.Lazy.IO
import Data.Text.Lazy.Encoding
import Data.Text.Format (format)
import qualified Prelude as P
import Prelude hiding (show)


-- | A synonym for 'Data.Text.All.lshow'.
show :: Show a => a -> Text
show = pack . P.show
{-# INLINE show #-}
