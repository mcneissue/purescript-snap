module Examples.CatTron.State where

import Prelude

import Affjax (printError)
import Affjax as AX
import Affjax.ResponseFormat (json)
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Either (Either(..), either)
import Effect.Aff (Aff)

data State = Start | Loading | Error String | Gif String

initialState :: State
initialState = Start

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
