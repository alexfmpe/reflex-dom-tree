{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad.Fix
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core
import Reflex.Dom.Tree

main :: IO ()
main = pure ()

patternMatches :: (DomBuilder t m, PostBuild t m) => m ()
patternMatches = do
  t <- elTree $ node "div" mempty $ widget blank :+ nil

  tt <- elTree $ widget blank
  let Widget () = tt

  t0 <- elTree $ widget $ el "div" $ pure (0 :: Int)

  let Widget zero = t0
  text $ T.pack $ show zero

  elTree (widget (el "div" blank)) >>= \case
    Widget () -> pure ()

  t1 <- elTree $ node "div" mempty $ widget (el "div" blank) :+ nil
  let Node _div _ = t1

  t2 <- elTree $ node "div" mempty
    $ widget (el "img" blank)
    :+ node "div" mempty (widget (el "br" blank) :+ nil)
    :+ nil

  let Node _div
        (Widget ()
         :+ Node __div (_br :+ Nil)
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


formDo
  :: (DomBuilder t m, MonadFix m, PostBuild t m)
  => m
     ( Dynamic t Text
     , Dynamic t Text
     , Dynamic t Text
     , Dynamic t Text
     , Dynamic t Text
     , Event t ()
     )
formDo = do
  let validateA f = mempty -- some standalone validation
      validateB f f' = mempty -- some conditional validation

  elClass "div" "form" $ do
    (input1, input2) <- elClass "div" "row" $ mdo
      input1 <- elClass "div" "field" $ inputElement def
      input2 <- elDynAttr "div" (validateA input2) $ inputElement def
      pure (input1, input2)
    (input3, input4) <- elClass "div" "row" $ mdo
      input3 <- elClass "div" "field" $ inputElement def
      input4 <- elDynAttr "div" (validateB input2 input4) $ inputElement def
      pure (input3, input4)
    (input5, clicks) <- elClass "div" "row" $ do
      input5 <- elClass "div" "field" $ inputElement def
      (btn, _) <- elAttr' "div" ("class" =: "button") $ text "Click me"
      pure (input5, domEvent Click btn)

    let (value1, value2, value3, value4, value5) = (value input1, value input2, value input3, value input4, value input5)
    pure (value1, value2, value3, value4, value5, clicks)


formTree
  :: (DomBuilder t m, MonadFix m, PostBuild t m)
  => m
     ( Dynamic t Text
     , Dynamic t Text
     , Dynamic t Text
     , Dynamic t Text
     , Dynamic t Text
     , Event t ()
     )
formTree = mdo
  let validateA v = mempty -- some standalone validation
      validateB v v' = mempty -- some conditional validation

  t <- elTree $ nodeClass "div" "form"
         $  nodeClass "div" "row" (  nodeClass "div" "field" (widget (inputElement def) :+ nil)
                                  :+ node "div" (validateA value2) (widget (inputElement def) :+ nil)
                                  :+ nil
                                  )
         :+ nodeClass "div" "row" (  nodeClass "div" "field" (widget (inputElement def) :+ nil)
                                  :+ node "div" (validateB value2 value4) (widget (inputElement def) :+ nil)
                                  :+ nil
                                  )
         :+ nodeClass "div" "row" (  nodeClass "div" "field" (widget (inputElement def) :+ nil)
                                  :+ nodeClass "div" "button" (widget (text "Click me") :+ nil)
                                  :+ nil
                                  )
         :+ nil

  let Node _
        (  Node _ (  Node _ (Widget input1 :+ Nil)
                  :+ Node _ (Widget input2 :+ Nil)
                  :+ Nil
                  )
        :+ Node _ (  Node _ (Widget input3 :+ Nil)
                  :+ Node _ (Widget input4 :+ Nil)
                  :+ Nil
                  )
        :+ Node _ (  Node _ (Widget input5 :+ Nil)
                  :+ Node button _
                  :+ Nil
                  )
        :+ Nil
        ) = t

      (value1, value2, value3, value4, value5) = (value input1, value input2, value input3, value input4, value input5)
      clicks = domEvent Click button

  pure (value1, value2, value3, value4, value5, clicks)
