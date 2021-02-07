module Examples.CatTron.State where

import Prelude

import Affjax (printError)
import Affjax as AX
import Affjax.ResponseFormat (json)
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Bifunctor as Bifunctor
import Data.Either (Either(..), either)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Snap.Machine.FeedbackLoop (FeedbackLoop)
import Snap.Machine.FeedbackLoop as FeedbackLoop

type AppState = FeedbackLoop.State String String
type AppTransition = FeedbackLoop.Transition String String

topic :: String
topic = "cats"

randomGifUrl :: Aff (Either String String)
randomGifUrl = do
  r <- (map <<< map) _.body $ AX.get json $ baseUrl <> topic
  pure $ either (Left <<< printError) decodeImageUrl r
  where
  baseUrl = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag="

decodeImageUrl :: Json -> Either String String
decodeImageUrl s = Bifunctor.lmap show $ do
  obj <- decodeJson s
  dat <- obj .: "data"
  url <- dat .: "image_url"
  pure url

loadGif :: Aff (Either String String)
loadGif = do
  delay $ Milliseconds $ 1000.0
  result <- randomGifUrl
  pure result

gifLoader :: FeedbackLoop Unit Aff AppState AppTransition
gifLoader = FeedbackLoop.loader loadGif