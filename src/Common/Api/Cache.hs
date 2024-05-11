module Api.Cache
( Cache
, empty
, withCache
) where

import Data.Dynamic
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Network.HTTP.Req (Url, Option, queryParamToList, renderUrl)

type Cache scheme = Map (Url scheme, [(Text, Maybe Text)]) Dynamic

empty :: Cache scheme
empty = Map.empty

withCache
  :: (Typeable a) => Cache scheme -> Url scheme -> Option scheme -> IO a
  -> IO (Cache scheme, a)
withCache cache url opts f =
  let queryParams = queryParamToList opts in
  case Map.lookup (url, queryParams) cache of
    Nothing -> do
      result <- f
      let cache' = Map.insert (url, queryParams) (toDyn result) cache
      pure (cache', result)
    Just result ->
      case fromDynamic result of
        Nothing -> fail $ "Cached value of URL " ++ show (renderUrl url) ++ " with query params '" ++ show queryParams ++ "' has wrong type"
        Just result' -> pure (cache, result')
