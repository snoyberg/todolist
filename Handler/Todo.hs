module Handler.Todo
    ( getTodoListR
    , postTodoListR
    , getTodoR
    , putTodoR
    , deleteTodoR
    ) where

import Import
import Network.HTTP.Types (status204)
import Data.Conduit (runResourceT, ($$))
import Data.Conduit.List (consume)

getTodoListR :: Handler RepJson
getTodoListR = do
    render <- getUrlRender
    tids <- runDB $ runResourceT $ selectKeys [] $$ consume
    jsonToRepJson $ array $ map (render . TodoR) tids

postTodoListR :: Handler ()
postTodoListR = do
    t <- parseJsonBody_
    tid <- runDB $ insert t
    sendResponseCreated $ TodoR tid

getTodoR :: TodoId -> Handler RepJson
getTodoR tid = runDB (get404 tid) >>= jsonToRepJson

putTodoR :: TodoId -> Handler ()
putTodoR tid = do
    t <- parseJsonBody_
    runDB $ repsert tid t
    sendResponseStatus status204 ()

deleteTodoR :: TodoId -> Handler ()
deleteTodoR tid = do
    runDB $ delete tid
    sendResponseStatus status204 ()
