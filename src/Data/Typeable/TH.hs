{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE CPP             #-}
module Data.Typeable.TH
  ( -- * Intro
    -- $intro

    -- * User interface
    makeTypeable
  , makeTypeableN

    -- * Utility and internal functions
  , dropEnd
  , bestTypeable
  , typeableBody
  , typeRepOf
  , splitKind
  , params
  , typeOfKind
  , expectTyCon
    -- * Error messages
    -- $errors

    -- * Reexports
  , module Data.Typeable
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
import           Control.Monad.Trans.State
import           Data.List
import           Data.Typeable
import           Language.Haskell.TH

{- $intro
    This module provides Template Haskell functions that derive TypeableN instances. They are smart
    in that they try define the best possible TypeableN instance, where a higher N is better.
    The best N is given by the number of parameters before the first parameter not of kind *, reading
    backwards.

    Maybe an example can explain this better:

    First, you need to enable some extensions and import Data.Typeable.TH to use this package:

> {{--}-# LANGUAGE EmptyDataDecls       #-{--}}
> {{--}-# LANGUAGE KindSignatures       #-{--}}
> {{--}-# LANGUAGE ScopedTypeVariables  #-{--}}
> {{--}-# LANGUAGE TemplateHaskell      #-{--}}
> {{--}-# LANGUAGE UndecidableInstances #-{--}}
> 
> import Data.Typeable
> import Data.Typeable.TH
  
     Now, we have some weird data type:
 
> data Weird (a :: *) (b :: (* -> *)) (c :: *) (d :: *) (e :: *)

     which has the kind @* -> (* -> *) -> * -> * -> * -> *@, then the best N we can pick for our TypeableN instance
     is 3, because there are 3 parameters of kind * (from the back) until we hit a parameter that isn't of kind *.
     Remember that the last * is not a kind of a parameter, but instead the kind of the data type when it has been applied
     to all parameters it needs.
   
     To derive a 'Typeable3' instance for this data type, we can use the following code:
     
> makeTypeable ''Weird
   
     This also gives use 'Typeable2', 'Typeable1' and 'Typeable', because those have default instances in terms of Typeable3.
   
     We can also test our instance:

>>> typeOf3 (undefined :: Weird Int Maybe [Char] Int Float)
Weird Int Maybe

>>> typeOf2 (undefined :: Weird Int Maybe [Char] Int Float)
Weird Int Maybe [Char]
   
     No more manual writing of TypeableN instances!
     
-}

{- $errors

  If you get an error like this,

  @
   test3.hs:6:1:
    The exact Name `p_a2t1' is not in scope
      Probable cause: you used a unique Template Haskell name (NameU), 
      perhaps via newName, but did not bind it
      If that's it, then -ddump-splices might be useful
  @

  check if you have enabled all extensions that are needed (a list is in the intro), in particular
  ScopedTypeVariables.  
  
  If you get a different error, it should tell you which extension you need to enable. If it doesn't,
  please file a bug report.

-}

-- | Derive the "best" typeable instance for a given data type.
makeTypeable :: Name -> Q [Dec]
makeTypeable typeName = bestTypeable `fmap` kindOf typeName >>= makeTypeableN typeName

-- | Derive the given TypeableN instance for a data type. Using N=0 generates a plain Typeable instance. Note that this function may
-- fail if it's not possible to derive the requested TypeableN instance.
makeTypeableN :: Name -> Int -> Q [Dec]
makeTypeableN name n = kindOf name >>= makeTypeableNWithKind name n 

-- | Derives the best typeable instance for a type with the given kinds. This won't call reify, so it's possible to use it
-- before the type is in scope.
makeTypeableWithKind :: Name -> Kind -> Q [Dec]
makeTypeableWithKind name kind = makeTypeableNWithKind name (bestTypeable kind) kind

-- | This function generates a TypeableN instance with the given N and kind. This won't call reify, so it can be used on types that
-- aren't in scope when the function is called.
makeTypeableNWithKind :: Name -> Int -> Kind -> Q [Dec]
makeTypeableNWithKind name n kind = do
  unless (all (== starK) $ take n $ reverse $ params kind) $
    fail $ "Can't generate a Typeable" ++ show (typeableSuffix n)
        ++ " instance for a type of kind "
        ++ pprint kind
  names <- replicateM (length (params kind) - n) $ newName "p"
  (body,(decs, context)) <- flip evalStateT 0 $ runWriterT $ typeableBody name kind n names
  let typ = conT (mkName ("Typeable" ++ typeableSuffix n)) `appT` foldl appT (conT name) (map varT names)
  let funName = mkName $ "typeOf" ++ typeableSuffix n
  
  fmap (:decs) $ instanceD (return context) typ $ [ 
    funD funName [clause [wildP] (normalB $ return body) []]
   ]


-- | Generate the typeOfN function of TypeableN, tell'ing all instance context predicates and declarations we need. We also update
-- a state to have a counter for generating unique names for data types we declare.
typeableBody :: Name -> Kind -> Int -> [Name] -> WriterT ([Dec],[Pred]) (StateT Integer Q) Exp
typeableBody datatype kind n vars = do
  exps <- fmap (map return) $ sequence $ zipWith typeRepOf vars $ dropEnd n $ params kind
  loc <- lift $ lift $ location
  let p = loc_package loc
  let m = loc_module loc
  let s = nameBase datatype
  lift $ lift $ [| mkTyCon3 p m s `mkTyConApp` $(listE exps) |]

-- | Returns the expression to get the TypeRep of a given type variable with a given kind.
typeRepOf :: Name -> Kind -> WriterT ([Dec],[Pred]) (StateT Integer Q) Exp
typeRepOf n k = do
  let bn = bestTypeable k
      pars = params k
      typeOfName = mkName $ "typeOf" ++ typeableSuffix bn

  -- Use a dummy data type for each parameter
  dummies <- lift $ mapM typeOfKind pars
  let decs = concatMap snd dummies
      dummyNames = map fst dummies
      typ = foldl AppT (VarT n) $ map ConT $ dropEnd bn dummyNames
      typ' = foldl AppT (VarT n) $ map ConT $ dummyNames

  tell (decs, [])
  tell $ ([], [ClassP (mkName $ "Typeable" ++ typeableSuffix bn) [typ]])
  lift $ lift $ 
   [| let r = $(varE typeOfName) (undefined :: $(return typ'))
          a = typeRepArgs r
          a' = dropEnd $(litE $ integerL $ fromIntegral $ length pars) $ a
      in  typeRepTyCon r  `mkTyConApp` a'
   |]

-- | Get the appendix to Typeable for a given N. For N=0, this is the empty string, for all other Ns, it's show N.
typeableSuffix :: Int -> String
typeableSuffix 0 = ""
typeableSuffix n = show n

-- | Calculate the maximum N for which a TypeableN instance is generatable for a given kind. How this works is explained in
-- the description at the top of this module.
bestTypeable :: Kind -> Int
bestTypeable = length . takeWhile (==starK) . reverse . params

-- | Split a kind into a list of kinds, where each list element is a kind of the parameter of the orginal kind.
-- The list is ordered, a parameter which comes first comes first in the list too.
params :: Kind -> [Kind]
params = reverse . drop 1 . reverse . unfoldr (fmap splitKind) . Just

-- | Split the part in front of the arrow from a kind, and return the rest (if there is any rest).
-- Example: splitKind (* -> *) -> * -> * will return ((* -> *),Just * -> *).
-- This is used to implement 'params'.
splitKind :: Kind -> (Kind, Maybe Kind)

#if MIN_VERSION_template_haskell(2,8,0)
splitKind (AppT (AppT ArrowT x) z) = (x,Just z)
#else
splitKind (ArrowK x z) = (x,Just z)
#endif

splitKind k = (k, Nothing)

-- | Generate a data type with the given kind that has no constructor and return the name of it.
-- The state is used for generating unqiue names for the data type.
typeOfKind :: Kind -> StateT Integer Q (Name,[Dec])

-- For some special cases that are needed very often, we can use already existing data types.
#if MIN_VERSION_template_haskell(2,8,0)
typeOfKind StarT = return $ (''(),[])
typeOfKind (AppT (AppT ArrowT StarT) StarT) = return $ (''Maybe,[])
typeOfKind (AppT (AppT ArrowT StarT) (AppT (AppT ArrowT StarT) StarT)) = return (''Either,[])
#else
typeOfKind StarK = return $ (''(),[])
typeOfKind (ArrowK StarK StarK) = return (''Maybe,[])
typeOfKind (ArrowK StarK (ArrowK StarK StarK)) = return (''Either,[])
#endif

-- The general case
typeOfKind kind = do
  x <- get
  let name = mkName $ "DeriveTypeableDummy" ++ show x
 -- Check whether there is already a data type with that name
  exists <- lift $ recover (return False) $ (True <$) $ reify name
  modify (+1)
  if exists 
    then typeOfKind kind
    else do
         dec <- lift $ mapM kindedTV (params kind) >>= \tvs -> dataD (return []) name tvs [normalC name []] []
         decs <- lift $ makeTypeableWithKind name kind
         return (name, dec : decs)
  where kindedTV k 
          | k == starK = PlainTV <$> newName "p"
          | otherwise = (`KindedTV` k) <$> newName "k"

-- | Get the kind of the given newtype or data type. If the given name does not refer to
-- a newtype or a data type, the function fails with an error message.
kindOf :: Name -> Q Kind
kindOf n = do
  dec <- reify n >>= expectTyCon "Not a newtype or a data type declaration"
  ks <- case dec of
    (DataD _ _ tv _ _) -> return $ map tvKind tv
    (NewtypeD _ _ tv _ _) -> return $ map tvKind tv
    _ -> fail "Not a newtype or data type declaration"
  return $ foldr chainK starK ks

  where tvKind (KindedTV _ k) = k
        tvKind (PlainTV _) = starK

-- | A helper function that makes sure the info is a TyConI, and throws an error otherwise.
expectTyCon :: String -> Info -> Q Dec
expectTyCon _ (TyConI dec) = return dec
expectTyCon err _ = fail err

-- | @dropEnd n l@ drops @n@ items from the end of the list @l@. This function is implemented the naive way, it might not
-- be the fastest.
dropEnd :: Int -> [a] -> [a]
dropEnd n l = zipWith const l (drop n l)

#if MIN_VERSION_template_haskell(2,8,0)
-- | Chain two kinds with (->)
chainK :: Kind -> Kind -> Kind
chainK a b = AppT (AppT ArrowT a) b

#else
-- | Chain two kinds with (->)
chainK :: Kind -> Kind -> Kind
chainK = ArrowK

-- | The kind @*@
starK :: Kind
starK = StarK
#endif
