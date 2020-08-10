module Interface.GetAnswer where

import Entity.Answer
import Entity.Question

class GetAnswer a where
  getAnswer :: a -> Question -> IO Answer