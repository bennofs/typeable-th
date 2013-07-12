typeable-th [![Build Status](https://travis-ci.org/bennofs/typeable-th.png?branch=master)](https://travis-ci.org/bennofs/typeable-th)
===========

There is no need to manually write TypeableN instances anymore, even for complex data types! This package derives typeable 
instances automatically. To use it, you just have to enable a few extensions and import the package:

```haskell
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Typeable.TH
```

And then define your data type and call makeTypeable ''YourDataTypeName:

```haskell
data Weird a (b :: * -> *) c d = Weird

makeTypeable ''Weird
```

And that's it!

Contributions
-------------

Contributions and bug reports are welcome! Just fork and then sumbit a pull request if you want to implement some
feature, or open a bug report to discuss the feature or bug. 
