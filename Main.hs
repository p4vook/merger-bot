{-# LANGUAGE DeriveGeneric     #-}
module Main where


import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap
import           Data.Maybe
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Text.Read
import           Control.Monad.STM
import           Control.Monad.IO.Class
import           Control.Concurrent.STM.TVar
import           GHC.Generics
import           System.Envy

import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser (updateMessageText)

type MyMap = HashMap (ChatId, MessageId) MessageId
data Model = Model { messageIds :: TVar MyMap, mergeChatId :: ChatId }

data Action
  = NoOp
  | Fwd Message
  | Edit Message

data BotConfig = BotConfig {
    botToken :: Token -- "BOT_TOKEN"
  , botMergeChatId :: ChatId -- "BOT_MERGE_CHAT_ID"
} deriving (Generic, Show)

instance Var Token where
  fromVar x = Just $ Token $ Text.pack x
instance Var ChatId where
  fromVar x = ChatId <$> readMaybe x
instance FromEnv BotConfig

echoBot :: ChatId -> TVar MyMap -> BotApp Model Action
echoBot mergechatid map = BotApp
  { botInitialModel = Model { messageIds = map, mergeChatId = mergechatid}
  , botAction = updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

goodChatId :: ChatId -> ChatId -> Bool
goodChatId = (/=)

goodUpdate :: Model -> Maybe Message -> Bool
goodUpdate model x = isJust x && goodChatId (mergeChatId model) (chatId $ messageChat $ fromJust x)

updateToAction :: Update -> Model -> Maybe Action
updateToAction update model
  | goodUpdate model $ updateChannelPost update = Just $ Fwd (fromJust $ updateChannelPost update)
  | goodUpdate model $ updateEditedChannelPost update = Just $ Edit (fromJust $ updateEditedChannelPost update)
  | otherwise = Nothing

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoOp -> pure model
  Fwd msg -> model <# do
    let fromChatId = chatId $ messageChat msg
    res <- liftClientM $ forwardMessage $ ForwardMessageRequest {
                                            forwardMessageChatId = SomeChatId (mergeChatId model)
                                           ,forwardMessageFromChatId = SomeChatId $ chatId $ messageChat msg
                                           ,forwardMessageDisableNotification = Nothing
                                           ,forwardMessageMessageId = messageMessageId msg }
    if responseOk res then
        liftIO $ atomically $ modifyTVar' (messageIds model) (HashMap.insert (fromChatId, messageMessageId msg) (messageMessageId $ responseResult res))
    else
        undefined
    return NoOp
  Edit msg -> model <# do
    ids <- liftIO $ readTVarIO (messageIds model)
    let toEdit = HashMap.lookup (chatId $ messageChat msg, messageMessageId msg) ids
    case toEdit of
        Just id -> liftClientM $ deleteMessage (mergeChatId model) id
        Nothing -> undefined
    return $ Fwd msg

run :: BotConfig -> IO ()
run config = do
  env <- defaultTelegramClientEnv (botToken config)
  res <- newTVarIO HashMap.empty
  startBot_ (conversationBot updateChatId $ echoBot (botMergeChatId config) res) env

main :: IO ()
main = do
        config <- decodeEnv :: IO (Either String BotConfig)
        case config of
            Right conf -> run conf
            Left err -> error err
