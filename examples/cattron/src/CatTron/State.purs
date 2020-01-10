module Examples.CatTron.State where

import Prelude

import Affjax (printError)
import Affjax as AX
import Affjax.ResponseFormat (json)
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff (Aff)

data State = Loading | Error String | Gif String

derive instance eqState :: Eq State
derive instance genericState :: Generic State _

instance showState :: Show State where
  show = genericShow

initialState :: State
initialState = Loading

topic :: String
topic = "cats"

randomGifUrl :: Aff (Either String String)
randomGifUrl = do
  r <- (map <<< map) _.body $ AX.get json $ baseUrl <> topic
  pure $ either (Left <<< printError) decodeImageUrl r

  where
  baseUrl = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag="

decodeImageUrl :: Json -> Either String String
decodeImageUrl s = do
  obj <- decodeJson s
  dat <- obj .: "data"
  url <- dat .: "image_url"
  pure url
