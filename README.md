Reflex-dom experiment: decoupling dom layout from the actual logic by binding a whole sub-tree at once and then using the `Element er s t` at the same scope.

It's basically
```haskell
elTree :: DomBuilder t m => Tree (Text, Map Text Text) -> m (Tree (Element EventResult (DomBuilderSpace m) t))
elTree (Node (tg, attrs) children) = do
  (n, cs) <- elAttr' tg attrs $ traverse elTree children
  pure $ Node n cs

t <- elTree $ blabla
case t of ...
```
but with fancier types to make it safe. Internal nodes are tags/attrs/etc and leaf nodes are arbitrary widgets.

Horrible ergonomics though - guess we'll need to wait for linear types.
