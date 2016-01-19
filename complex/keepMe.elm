module AlbumList where

import Maybe exposing (..)
import Album
import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)

type alias Model = 
  {
    albums: List Album.Model
  , playing: Maybe Album.Model
  }
  
init : List String -> List Album.Model
init strings =
  List.map Album.init strings
  
type Action = Play Album.Model Album.Action | Stop
  
update: Action -> Model -> Model
update action model = 
  case action of
    Play album _ ->
      Debug.log "model" { model | playing = Just album}
    Stop -> 
      Debug.log "model" { model | playing = Nothing }
      
      
view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [
      div [] (List.map (\a -> Album.view (Signal.forwardTo address (Play a)) a) model.albums)
    , div []
      [
        button [onClick address Stop] [text "Stop"]
      ]
    ]

examples : List Album.Model
examples = init ["Back in Black", "In the Garden Green", "Red Rice"]
  
main =
  start { model = { albums = examples, playing = Nothing }, update = update, view = view }
  

---------------------------------------------------------------------------------------------#

module AlbumArray where

import Maybe exposing (..)
import Array exposing(Array)
import Album
import Html exposing (Html, div, text, button, ul, li)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)

-- MODEL contains a list of albums, one of which may be selected

type alias Model =
  { albums: Array Album.Model
  , selected: Maybe Album.Model
  }

init : List String -> Model
init strings =
    Debug.log "init: " { albums = Array.fromList (List.map Album.init (List.sort strings))
  , selected = Nothing
  }

resetAll : Model -> Model
resetAll model = { model | albums = Array.map Album.reset model.albums }

-- UPDATE : trigger action if album clicked.
-- actually, reset (and probably selected) are useful only at a higher level still

type Action = Clicked Int Album.Action | Reset

updateEntry : Maybe Album.Model -> Album.Action -> Maybe Album.Model
updateEntry album action =
    case album of
      Nothing -> Nothing
      Just a -> Just (Album.update action a)

update : Action -> Model -> Model
update action model =
  let
     albums' = resetAll
  in
    case action of
      Reset -> Debug.log "update: " { model | selected = Nothing , albums = albums' }

      Clicked id albumAction ->
      let
        album = Array.get id model.albums
        album' = updateEntry album albumAction
      in
        case album' of
          Nothing ->
            Debug.log "update: " { model | selected = Nothing , albums = albums' }
          Just a ->
            Debug.log "update: " {
              model | selected = album', albums = Array.set id a albums' }

-- VIEW a list of album names as bullets

view : Signal.Address Action -> Model -> Html
view address model =
  let albums = Array.toIndexedList model.albums
      albums' = List.map (\(i,a) -> viewAlbum address (i,a)) albums
  in
      ul [ style [ ("margin", "0px")  ] ]  albums'

viewAlbum : Signal.Address Action -> (Int, Album.Model) -> Html
viewAlbum address (id, album) =
  Album.view (Signal.forwardTo address (Clicked id)) album

-- MAIN with sample data

examples : Model
examples = init [ "Red Rice", "Back in Black", "In the Garden Green" ]

main =
  start { model = examples, update = update, view = view }


