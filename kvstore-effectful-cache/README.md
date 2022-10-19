# kvstore-effectful-cache

`kvstore-effectful-cache` is a `kvstore-effectful` interpreter that uses `effectful-cache` as a lower-level language.

## Example usage

```
program ::
     (KVStore k v :> es)
  => k -> v -> Eff es Bool
program k v = do
  update k v
  res <- lookup k
  pure $ res == Just v

main :: IO ()
main = do
  cache <- newCache Nothing :: IO (Data.Cache Int String)
  (program 10 "test")
    & runEff              -- from effectful
	& runCacheIO cache    -- from effectful-cache
	& runKVStoreWithCache -- from kvstore-effectful-cache
```
