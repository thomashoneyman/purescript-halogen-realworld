-- | We only receive a portion of large resources like "all articles" for performance reasons. This
-- | module exports a type to represent partial data in an array format.
module Conduit.Data.PaginatedArray where

-- | This type is a simple model of paginated data. We don't receive an entire payload at once, so
-- | it's unnecessary to track anything beyond our current window of data. We'll hold on to the
-- | total so we can tell how many pages to render in a clickable list in the dashboard, among
-- | others.
-- |
-- | Since all the members of this record type have `DecodeJSON` instances, we don't have to write
-- | any manual encoding or decoding! This can be translated to and from JSON with no additional
-- | effort on our part.
type PaginatedArray a =
  { total :: Int
  , body :: Array a
  }
