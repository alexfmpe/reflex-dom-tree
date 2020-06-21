{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Werror=inaccessible-code #-}
{-# OPTIONS_GHC -Werror=overlapping-patterns #-}

module Reflex.Dom.Tree where

import Data.Functor.Identity
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core

import Prelude hiding (div)


main :: IO ()
main = putStrLn "Hello, Haskell!"

data Payload a m b = Payload a

data TTag
  = TTagLeaf
  | TTagBranch

data T (tag :: TTag) (children :: [(* -> *) -> * -> *]) (m :: * -> *) (spine :: *) where
  TLeaf :: m x -> T 'TTagLeaf '[Payload x] m spine
  TBranch :: spine -> V (x ': xs) T m spine -> T 'TTagBranch (x ': xs) m spine

deriving instance Functor (T tag children m)
deriving instance Foldable (T tag children m)
deriving instance Traversable (T tag children m)

data V (shape :: [(* -> *) -> * -> *]) (f :: k -> [(* -> *) -> * -> *] -> (* -> *) -> * -> *) (m :: * -> *) a where
  VNil :: V '[] f m a
  VCons :: f t xs m a -> V xss f m a -> V (f t xs : xss) f m a

deriving instance (forall tag xs. Functor (f tag xs m)) => Functor (V l f m)
deriving instance (forall tag xs. Foldable (f tag xs m)) => Foldable (V l f m)
deriving instance (forall tag xs. Traversable (f tag xs m)) => Traversable (V l f m)

mapVT :: (forall tag xs. T tag xs m a -> T tag xs m b) -> V shape T m a -> V shape T m b
mapVT f = \case
  VNil -> VNil
  VCons t xs -> VCons (f t) (mapVT f xs)

traverseVT :: Applicative m => (forall tag xs. T tag xs m a -> m (T tag xs n b)) -> V xss T m a -> m (V xss T n b)
traverseVT f = \case
  VNil -> pure VNil
  VCons t v -> VCons <$> f t <*> traverseVT f v

elTree :: DomBuilder t m => T tag xs m (Text, Map Text Text) -> m (T tag xs Identity (Element EventResult (DomBuilderSpace m) t))
elTree = \case
  TLeaf x -> TLeaf . Identity <$> x
  TBranch (tg, attrs) xs -> do
    (n, cs) <- elAttr' tg attrs $ traverseVT elTree xs
    pure $ TBranch n cs

leaf :: m x -> T 'TTagLeaf '[Payload x] m spine
leaf = TLeaf

branch :: spine -> V (x ': xs) T m spine -> T 'TTagBranch (x ': xs) m spine
branch = TBranch

{-# COMPLETE Leaf #-}
pattern Leaf
  :: x
  -> T 'TTagLeaf '[Payload x] Identity spine
pattern Leaf x = TLeaf (Identity x)

-- Needed?
-- {-# COMPLETE Branch #-}
pattern Branch
  :: spine
  -> V (x ': xs) T Identity spine
  -> T 'TTagBranch (x ': xs) Identity spine
pattern Branch node children = TBranch node children

-- Needed?
-- {-# COMPLETE Nil #-}
pattern Nil
  :: V '[] T Identity a
pattern Nil = VNil

nil :: V '[] T m a
nil = VNil

-- Mirrors https://developer.mozilla.org/en-US/docs/Web/CSS/Adjacent_sibling_combinator
pattern (:+) :: T tag xs m a -> V xss T m a -> V (T tag xs : xss) T m a
pattern h :+ t = VCons h t
infixr 5 :+


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
examples :: DomBuilder t m => m ()
examples = do
  t0 <- elTree $ leaf $ el "div" $ pure (0 :: Int)

  let Leaf zero = t0
  text $ T.pack $ show zero

  elTree (leaf (el "div" blank)) >>= \case
    Leaf () -> pure ()

  t1 <- elTree $ branch ("div", mempty) $ leaf (el "div" blank) :+ nil
  let Branch _div _ = t1

  t2 <- elTree $ branch ("div", mempty)
    $ leaf (el "img" blank)
    :+ branch ("div", mempty) (leaf (el "br" blank) :+ nil)
    :+ nil

  let Branch _div
        (Leaf ()
         :+ Branch __div (_br :+ Nil)
         :+ Nil) = t2

  t4 <- elTree $ branch ("div", mempty)
    $  leaf (el "img" blank)
    :+ leaf (el "br" $ pure (3 :: Int))
    :+ nil

  let
    (Branch _div
     (Leaf ()
      :+ Leaf three
      :+ Nil)) = t4

  text $ T.pack $ show $ three
