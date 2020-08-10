module UseCase.Ask where

import Entity.Question
import Entity.Answer
import Interface.GetAnswer

ask :: GetAnswer a => a ->  Question -> IO Answer
ask wolfram question = getAnswer wolfram question