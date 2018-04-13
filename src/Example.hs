{-# LANGUAGE OverloadedStrings,DeriveGeneric  #-}
-- Author: Dante Elrik
module Example (runApp, app) where

import           Data.Aeson
import           Network.Wai (Application)
import qualified Web.Scotty          as S
import qualified Data.Text
import qualified Data.HashMap.Strict as HM
import Data.Default
import GHC.Generics
import Data.IORef

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as CBS

import Control.Monad (liftM)
import Data.Text (Text)
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Maybe
import Control.Applicative


data Action  =
  Add
 |Remove
 |List
 |UnknownAction deriving (Show, Read, Generic)


instance Default Action where
  def = UnknownAction

type Params = Value
type Queue  = IORef [Value] 

data Speech =
  Speech Value Action (Action -> Queue -> IO ())

instance Default Speech where
  def = Speech Null UnknownAction (const . const (pure ()))

newtype Hook =
  Hook Params

instance Default Hook where
  def = Hook defaultResult

instance ToJSON Speech where
  toJSON (Speech v _ _) =
    object [
      "speech" .= v
    ]

defaultResult =   
  object ["result" .= object [
             "action" .= Null
             ,"parameters" .= Null
           ]
         ]

testHook :: Hook
testHook = def

testHook' :: Hook
testHook' = Hook (
  object ["result" .= object [
             "action" .= String "item.add"
             ,"parameters" .= object [
                 "item" .= Number 123
                 ]
           ]
          ]
  )  
dialogue :: Hook -> Speech
dialogue (Hook (Object params)) =  
  Speech (speechText action item) 
         actionType 
         postProcess
  where

    actionType
      | action == String "item.add"    = Add
      | action == String "item.remove" = Remove
      | action == String "item.list"   = List

    postProcess Add q = do
      items <- readIORef q
      writeIORef q (item:items)

    postProcess Remove q = do
      items <- readIORef q 
      case items of
        _:rest -> writeIORef q items
        []     -> pure ()
                           
    postProcess List q = do
      items <- readIORef q
      putStrLn (show items)
      
    Object params' =
           (HM.lookup "result" params
       <|> pure (Object HM.empty))
        & fromJust

    action =
           HM.lookup "action" params'
       <|> pure (String "unknown action")
        & fromJust

    item =
          ((HM.lookup "parameters" params' >>=
          \(Object v)                      ->
          HM.lookup "item" v))
      <|> pure (String "unknown parameters")
       & fromJust

    speechText (String actionValue) (String itemValue)  =
      String $ "I received " <> actionValue <> " with " <> itemValue

    speechText (String actionValue) (Number itemValue)  =
      String $ "I received " <> actionValue <> " with " <> Data.Text.pack (show itemValue)
    speechText (String actionValue) itemValue  =
      String $ "I received " <> actionValue <> " with " <> Data.Text.pack (show itemValue)
    speechText _ _ = 
      String "I have no idea what I'm doing"

dialogue (Hook _)  = def
 
app' :: Queue -> S.ScottyM ()
app' queueRef =
  S.post "/" $ do
    payloadValue <- S.body        >>=
                    pure
                    . fromJust
                    . (<|> (Just defaultResult))
                    . decode

    S.addHeader "Server" "Helios"
    (Speech resp actionHandle postAction) <- pure (dialogue $ Hook payloadValue)
    S.liftAndCatchIO (postAction actionHandle queueRef)
    S.json $ resp

queueRef :: IO Queue  
queueRef = newIORef []

app :: IO Application
app = do
  queue <- queueRef
  S.scottyApp (app' queue)

runApp :: IO ()
runApp = do
  queue <- queueRef
  S.scotty 1233 (app' queue)
