module Interface.Entrypoint where

import qualified Entity.Message as M

type BotEntrypoint = String -> IO M.Message