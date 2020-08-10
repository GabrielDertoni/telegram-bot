module Interface.ToMarkdown where

class ToMarkdown a where
  markdown :: a -> String