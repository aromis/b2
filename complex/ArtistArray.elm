module ArtistArray where

import Maybe exposing (..)
import Array exposing(Array)
import Album
import Artist
import Html exposing (Html, div, text, button, ul, li)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)

-- MODEL contains a list of albums, one of which may be selected

type alias Model =
  { artists : Array Artist.Model
  , selected : Maybe Artist.Model
  }

init : List { artist: String, titles: List String } -> Model
init details =
  { artists = Array.fromList ( List.map (\a -> Artist.init a.artist a.titles) details )
  , selected = Nothing
  }

-- UPDATE : trigger action if album clicked.

resetArtist :Artist.Model ->Artist.Model
resetArtist a = Artist.reset a

type Action = ArtistClicked Int Artist.Action | AlbumClicked Album.Action

update : Action -> Model -> Model
update action model =
  case action of
    ArtistClicked id artistAction ->
      let
        artist = Array.get id model.artists
        artist' =
          case artist of
            Nothing -> Nothing
            Just a -> Just ( Artist.update artistAction a )
      in
        case artist' of
          Nothing -> model
          Just a -> { model | artists = Array.set id a model.artists }
    AlbumClicked albumAction -> model

-- VIEW a list of album names as bullets

view : Signal.Address Action -> Model -> Html
view address model =
  let artists = Array.toIndexedList model.artists
      artists' = List.map (\(i,a) -> viewArtist address (i,a)) artists
  in
      ul [ style [ ("margin", "0px")  ] ]  artists'

viewArtist : Signal.Address Action -> (Int, Artist.Model) -> Html
viewArtist address (id, artist) =
  let context =
    { artist = (Signal.forwardTo address (ArtistClicked id))
    , albums = (Signal.forwardTo address (AlbumClicked)) }
  in
    Artist.viewInContext context artist

-- MAIN with sample data

examples : Model
examples = init
  [
    { artist= "Askew Sisters"
    , titles = [ "Red Rice", "Back in Black" ]
     }
  , { artist = "Peking Bros"
    , titles = [ "In the Garden Green" ]
    }
  ]

main =
  start { model = examples, update = update, view = view }


