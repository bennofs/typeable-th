{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Typeable.TH

import System.Exit

data T1 a (b :: * -> *) c d e = T1
makeTypeable ''T1
data T2 a b = T2
makeTypeable ''T2
data T3 (a :: * -> * -> (* -> (* -> *)) -> *) b = T3
makeTypeable ''T3

data Complicated
  (a :: * -> (* -> *) -> * -> * -> (* -> *))
  (b :: * -> * -> *)
  (c :: (* -> * -> (* -> (* -> *)) -> *) -> * -> *)
  d
  e
  f = Complicated
makeTypeable ''Complicated

main :: IO ()
main = if b then exitSuccess else exitFailure
  where b = typeOf (Complicated :: Complicated T1 T2 T3 Int String Bool) == typeOf (Complicated :: Complicated T1 T2 T3 Int String Bool)
