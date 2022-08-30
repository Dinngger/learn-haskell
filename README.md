## functor laws:
- fmap id = id
- fmap (f . g) = fmap f . fmap g

## applicative functors laws:
- pure f <*> x = fmap f x
- => pure id <*> v = fmap id x = v
- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
- pure f <*> pure x = fmap f $ pure x = pure (f x)
- u <*> pure y = pure ($ y) <*> u

## Monoid laws:
- mempty `mappend` x = x
- x `mappend` mempty = x
- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

## Monad laws:
- return x >>= f = fx
- m >>= return = m
- (m >>= f) >>= g = m >>= (\x -> f x >>= g)


http://cnhaskell.com/
https://github.com/acowley/roshask/wiki
http://learnyouahaskell.com/for-a-few-monads-more

cabal build
cabal run

profile:
    cabal build --enable-profiling --ghc-options="-fprof-auto -rtsopts"
    ./dist-newstyle/build/x86_64-linux/ghc-8.10.7/xvm-0.1.0.0/x/xvm/build/xvm/xvm +RTS -hd -i0.001 -p
    hp2ps -e8in -c xvm.hp
