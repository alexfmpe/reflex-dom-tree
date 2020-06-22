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

data T (fruit :: *) (branches :: [(* -> *) -> * -> *]) (m :: * -> *) (sap :: *) where
  Leaf :: m a -> T (Identity a) '[] m sap
  Branch :: sap -> V branches T m sap -> T Void branches m sap

deriving instance Functor (T fruit branches m)
deriving instance Foldable (T fruit branches m)
deriving instance Traversable (T fruit branches m)

data V (shape :: [(* -> *) -> * -> *]) (f :: * -> [(* -> *) -> * -> *] -> (* -> *) -> * -> *) (m :: * -> *) a where
  VNil :: V '[] f m a
  VCons :: f t xs m a -> V xss f m a -> V (f t xs : xss) f m a

deriving instance (forall fruit branches. Functor (f fruit branches m)) => Functor (V shape f m)
deriving instance (forall fruit branches. Foldable (f fruit branches m)) => Foldable (V shape f m)
deriving instance (forall fruit branches. Traversable (f fruit branches m)) => Traversable (V shape f m)

traverseVT :: Applicative m => (forall fruit xs. T fruit xs m a -> m (T fruit xs n b)) -> V xss T m a -> m (V xss T n b)
traverseVT f = \case
  VNil -> pure VNil
  VCons t v -> VCons <$> f t <*> traverseVT f v

elTree
  :: (DomBuilder t m, PostBuild t m)
  => T fruit xs m (Text, Dynamic t (Map Text Text))
  -> m (T fruit xs Identity (Element EventResult (DomBuilderSpace m) t))
elTree = \case
  Leaf x -> Leaf . Identity <$> x
  Branch (tg, attrs) xs -> fmap (uncurry Branch) $ elDynAttr' tg attrs $ traverseVT elTree xs

widget :: m a -> T (Identity a) '[] m sap
widget = Leaf

-- Needed?
-- {-# COMPLETE Widget #-}
pattern Widget :: a -> T (Identity a) '[] Identity sap
pattern Widget a = Leaf (Identity a)

node :: tag -> attrs -> V (x ': xs) T m (tag, attrs)-> T Void (x ': xs) m (tag, attrs)
node = curry Branch

-- Needed?
-- {-# COMPLETE Node #-}
pattern Node :: sap -> V xs T Identity sap -> T Void xs Identity sap
pattern Node node children = Branch node children

-- Needed?
-- {-# COMPLETE Nil #-}
pattern Nil :: V '[] T Identity a
pattern Nil = VNil

nil :: V '[] T m a
nil = VNil

-- Mirrors https://developer.mozilla.org/en-US/docs/Web/CSS/Adjacent_sibling_combinator
pattern (:+) :: T fruit xs m a -> V xss T m a -> V (T fruit xs : xss) T m a
pattern h :+ t = VCons h t
infixr 5 :+

examples :: (DomBuilder t m, PostBuild t m) => m ()
examples = do
  t <- elTree $ node "div" mempty $ widget blank :+ nil
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

  t1 <- elTree $ node "div" mempty $ widget (el "div" blank) :+ nil
  let Node _div _ = t1
--  let Widget _ = t1

  t2 <- elTree $ node "div" mempty
    $ widget (el "img" blank)
    :+ node "div" mempty (widget (el "br" blank) :+ nil)
    :+ nil

  let Node _div
        (Widget ()
         :+ Node __div (_br :+ Nil)
--         :+ Node _ _
         :+ Nil) = t2

  t4 <- elTree $ node "div" mempty
    $  widget (el "img" blank)
    :+ widget (el "br" $ pure (3 :: Int))
    :+ nil

  let
    (Node _div
     (Widget ()
      :+ Widget three
      :+ Nil)) = t4

  text $ T.pack $ show $ three
