module AlbumList where

import Maybe exposing (..)
import Album
import Html exposing (Html, div, text, button, ul, li)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)

-- wrap up what we need to maintain and identify albums in a list

type alias ID = Int
type alias Entry = (ID, Album.Model)

-- get the 'n'th item
select : Int -> Entry -> Bool
select n (id, album) = (n == id)

-- replace the album (whose state has changed) in 'n'th place
replace : List Entry -> Int -> Album.Model -> List Entry
replace list indexToFocusOn newEntry =
  let
    checkAndReplace ( id, album ) =
      if id == indexToFocusOn then
        ( id, newEntry )
      else
        ( id, album )
  in
    List.map checkAndReplace  list

-- MODEL contains a list of albums, one of which may be selected

type alias Model =
  { albums: List Entry
  , selected: Maybe Entry
  }
  
init : List String -> Model
init strings =
  let
    newAlbums = List.map Album.init (List.sort strings)
  in
    Debug.log "init: " { albums = (List.indexedMap (,) newAlbums)
  , selected = Nothing
  }

-- UPDATE : trigger action is album clicked.
-- actually, reset (and probably selected) are useful only at a higher level still

type Action = Clicked ID Album.Action | Reset

updateEntry : Maybe Entry -> Album.Action -> Maybe Entry
updateEntry entry action=
    case entry of
      Nothing -> Nothing
      Just (id, album) -> Just (id, Album.update action album)
  
update : Action -> Model -> Model
update action model =
  let
     albums' = List.map (\(id, album) -> (id, Album.reset album)) model.albums
  in
    case action of
      Reset -> Debug.log "update: " { model | selected = Nothing , albums = albums' }

      Clicked id albumAction ->
      let
        entry = List.head (List.filter (select id) model.albums)
        entry' = updateEntry entry albumAction
      in
        case entry' of
          Nothing ->
            Debug.log "update: " { model | selected = Nothing , albums = albums' }
          Just ( id, album ) ->
            Debug.log "update: " {
              model | selected = entry', albums = replace albums' id album }

-- VIEW a list of album names as bullets

view : Signal.Address Action -> Model -> Html
view address model =
  let albums = List.map (viewAlbum address) model.albums
  in
  div []
    [
      div [] [ ul [] (List.map (\a -> li [] [a]) albums) ]
    , div [] [ button [ onClick address Reset ] [ text "Reset" ] ]
    ]

viewAlbum : Signal.Address Action -> Entry -> Html
viewAlbum address (id, album) =
  Album.view (Signal.forwardTo address (Clicked id)) album

-- MAIN with sample data

examples : Model
examples = init [ "Red Rice", "Back in Black", "In the Garden Green" ]
  
main =
  start { model = examples, update = update, view = view }
  

