module AlbumArray where

import Maybe exposing (..)
import Array exposing(Array)
import Album
import Html exposing (Html, div, text, button, ul, li)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)

-- MODEL contains a list of albums, one of which may be selected

type alias Model = Array Album.Model

init : List String -> Model
init strings =
    Debug.log "init: " Array.fromList (List.map Album.init (List.sort strings))

resetAll : Model -> Model
resetAll model = Array.map Album.reset model

-- UPDATE : trigger action if album clicked.
-- actually, reset (and probably selected) are useful only at a higher level still

type Action = Clicked Int Album.Action

updateEntry : Maybe Album.Model -> Album.Action -> Maybe Album.Model
updateEntry album action =
    case album of
      Nothing -> Nothing
      Just a -> Just (Album.update action a)
  
update : Action -> Model -> Model
update (Clicked id albumAction) model =
  let albums' = resetAll model -- radio button - all others must be normal
      album' = updateEntry (Array.get id model) albumAction
  in
    case album' of
      Nothing -> Debug.log "update: "  albums'
      Just a ->  Debug.log "update: " (Array.set id a albums')

-- VIEW a list of album names as bullets

view : Signal.Address Action -> Model -> Html
view address model =
  let albums = Array.toIndexedList model
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
  

