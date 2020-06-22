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
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Dom.Tree
  ( elTree
  , pattern (:+)
  , nil
  , pattern Nil
  , node
  , nodeClass
  , pattern Node
  , widget
  , pattern Widget
  ) where

import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Reflex.Dom.Core

data T (fruit :: *) (branches :: [(* -> *) -> * -> *]) (m :: * -> *) (sap :: *) where
  Spur :: m a -> T (Identity a) '[] m sap
  Branches :: sap -> V branches T m sap -> T Void branches m sap

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
  Spur x -> Spur . Identity <$> x
  Branches (tg, attrs) xs -> fmap (uncurry Branches) $ elDynAttr' tg attrs $ traverseVT elTree xs

widget :: m a -> T (Identity a) '[] m sap
widget = Spur

-- {-# COMPLETE Widget #-} -- Needed?
pattern Widget :: a -> T (Identity a) '[] Identity sap
pattern Widget a = Spur (Identity a)

node :: tag -> attrs -> V xs T m (tag, attrs) -> T Void xs m (tag, attrs)
node = curry Branches

-- {-# COMPLETE Node #-} -- Needed?
pattern Node :: sap -> V xs T Identity sap -> T Void xs Identity sap
pattern Node node children = Branches node children

-- {-# COMPLETE Nil #-} -- Needed?
pattern Nil :: V '[] T Identity a
pattern Nil = VNil

nil :: V '[] T m a
nil = VNil

-- Mirrors https://developer.mozilla.org/en-US/docs/Web/CSS/Adjacent_sibling_combinator
pattern (:+) :: T fruit xs m a -> V xss T m a -> V (T fruit xs : xss) T m a
pattern h :+ t = VCons h t
infixr 5 :+

nodeClass :: Reflex t => Text -> Text -> V xs T m (Text, Dynamic t (Map Text Text)) -> T Void xs m (Text, Dynamic t (Map Text Text))
nodeClass tag cls = node tag $ pure $ "class" =: cls
