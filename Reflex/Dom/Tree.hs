{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Werror=inaccessible-code #-}
{-# OPTIONS_GHC -Werror=overlapping-patterns #-}

module Reflex.Dom.Tree
  ( elTree
  , nil
  , node
  , widget
  ) where

import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Reflex.Dom.Core

import Prelude hiding (div)

data T (payload :: *) (children :: [(* -> *) -> * -> *]) (m :: * -> *) (spine :: *) where
  Leaf :: m a -> T (Identity a) '[] m spine
  Branch :: spine -> V xs T m spine -> T Void xs m spine

--type Leaf a = T (Identity a) '[]
--type Branch xs = T Void xs

deriving instance Functor (T payload children m)
deriving instance Foldable (T payload children m)
deriving instance Traversable (T payload children m)

data V (shape :: [(* -> *) -> * -> *]) (f :: * -> [(* -> *) -> * -> *] -> (* -> *) -> * -> *) (m :: * -> *) a where
  VNil :: V '[] f m a
  VCons :: f t xs m a -> V xss f m a -> V (f t xs : xss) f m a

deriving instance (forall payload xs. Functor (f payload xs m)) => Functor (V l f m)
deriving instance (forall payload xs. Foldable (f payload xs m)) => Foldable (V l f m)
deriving instance (forall payload xs. Traversable (f payload xs m)) => Traversable (V l f m)

mapVT :: (forall payload xs. T payload xs m a -> T payload xs m b) -> V shape T m a -> V shape T m b
mapVT f = \case
  VNil -> VNil
  VCons t xs -> VCons (f t) (mapVT f xs)

traverseVT :: Applicative m => (forall payload xs. T payload xs m a -> m (T payload xs n b)) -> V xss T m a -> m (V xss T n b)
traverseVT f = \case
  VNil -> pure VNil
  VCons t v -> VCons <$> f t <*> traverseVT f v

elTree :: DomBuilder t m => T payload xs m (Text, Map Text Text) -> m (T payload xs Identity (Element EventResult (DomBuilderSpace m) t))
elTree = \case
  Leaf x -> Leaf . Identity <$> x
  Branch (tg, attrs) xs -> do
    (n, cs) <- elAttr' tg attrs $ traverseVT elTree xs
    pure $ Branch n cs

widget :: m a -> T (Identity a) '[] m spine
widget = Leaf

node :: spine -> V (x ': xs) T m spine -> T Void (x ': xs) m spine
node = Branch

-- Needed?
-- {-# COMPLETE Widget #-}
pattern Widget
  :: a
  -> T (Identity a) '[] Identity spine
pattern Widget a = Leaf (Identity a)

-- Needed?
-- {-# COMPLETE Node #-}
pattern Node
  :: spine
  -> V xs T Identity spine
  -> T Void xs Identity spine
pattern Node node children = Branch node children

-- Needed?
-- {-# COMPLETE Nil #-}
pattern Nil
  :: V '[] T Identity a
pattern Nil = VNil

nil :: V '[] T m a
nil = VNil

-- Mirrors https://developer.mozilla.org/en-US/docs/Web/CSS/Adjacent_sibling_combinator
pattern (:+) :: T payload xs m a -> V xss T m a -> V (T payload xs : xss) T m a
pattern h :+ t = VCons h t
infixr 5 :+

examples :: DomBuilder t m => m ()
examples = do
  t <- elTree $ node ("div", mempty) $ widget blank :+ nil
--  let t' = t :: Int

  tt <- elTree $ widget blank
--  let tt' = tt :: Int
--  let Node _ _ = tt
  let Widget () = tt


  t0 <- elTree $ widget $ el "div" $ pure (0 :: Int)

  let Widget zero = t0
  text $ T.pack $ show zero

  elTree (widget (el "div" blank)) >>= \case
    Widget () -> pure ()

  t1 <- elTree $ node ("div", mempty) $ widget (el "div" blank) :+ nil
  let Node _div _ = t1
--  let Widget _ = t1

  t2 <- elTree $ node ("div", mempty)
    $ widget (el "img" blank)
    :+ node ("div", mempty) (widget (el "br" blank) :+ nil)
    :+ nil

  let Node _div
        (Widget ()
         :+ Node __div (_br :+ Nil)
--         :+ Node _ _
         :+ Nil) = t2

  t4 <- elTree $ node ("div", mempty)
    $  widget (el "img" blank)
    :+ widget (el "br" $ pure (3 :: Int))
    :+ nil

  let
    (Node _div
     (Widget ()
      :+ Widget three
      :+ Nil)) = t4

  text $ T.pack $ show $ three
