# Loop

A loop dsl for monadic actions.

### Motivation

Looping in monadic actions always feel awkward. Hope this dsl provides a simple yet powerful enough solution for most use cases. This dsl is only aim for monadic actions.

The design is losely influenced by common lisp loop macro.


```haskell

main = do
  -- loop over container and return ()
  loop `across` [1, 2, 3] `with` \i -> do print i

  -- loop until some condition, return ()
  loop `across` [1, 2, 3] `while` (/=10) `with` \i -> do print i

  -- loop with access to index.
  loop `across` [1, 2, 3] `iwith` \(i,v) -> do print i

  -- loop, break out on condition
  loop `across` [1, 2, 3] `with` \i -> do if i = 10 then break else print v
```
