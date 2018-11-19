-- | We'd like to ensure that requests always use a valid endpoint, and that those
-- | endpoints are representative of the only possible ways to access the API. We
-- | can offer some escape hatches or more flexible data types in the future, but
-- | for the time being we only have a few endpoints to support.

module Data.Endpoint where

import Data.Username (Username)
