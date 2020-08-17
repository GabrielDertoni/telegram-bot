module Helper.Telegram.EditMessageText
  ( EditMessageText
  , editMessageText
  ) where

import           GHC.Generics (Generic)
import           Data.Aeson ((.=))
import qualified Data.Aeson         as Aeson
import qualified Network.URI.Encode as URI

import           Helper.Query
import           Helper.Maybe ((??))
import qualified Helper.Telegram    as Telegram

data EditMessageText
  = EditMessageText { chat_id :: Int
                    , message_id :: Int
                    , edited_text :: String
                    , parse_mode :: Maybe String
                    }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON EditMessageText where
  toJSON edit = Aeson.object [ "chat_id"    .= chat_id     edit
                             , "message_id" .= message_id  edit
                             , "text"       .= edited_text edit
                             , "parse_mode" .= parse_mode  edit
                             ]

instance Query EditMessageText where
  getURL edit = (<> editMessageQuery edit) <$> Telegram.getEndpointURL "editMessageText"

editMessageQuery :: EditMessageText -> String
editMessageQuery edit = fromPairs [ ("chat_id"   , show $ chat_id           edit)
                                  , ("message_id", show $ message_id        edit)
                                  , ("text"      , URI.encode $ edited_text edit)
                                  , ("parse_mode", parse_mode edit ?? "")
                                  ]

editMessageText :: Int -> Int -> String -> EditMessageText
editMessageText cid mid text = EditMessageText cid mid text Nothing