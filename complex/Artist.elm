module Artist where

import Album
import AlbumArray

import StartApp.Simple exposing (start)
import Html exposing (Html, div, span, li, a, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

type Detail = Short | Full

type alias Model =
  { artist: String
  , albums: AlbumArray.Model
  , detail: Detail
  }

type alias Context =
    { artist : Signal.Address Action
    , albums  : Signal.Address Album.Action
    }

init : String -> List String -> Model
init name titles =
  { artist = name
  , albums = AlbumArray.init titles
  , detail = Short
  }

reset : Model -> Model
reset model = { model | detail = Short }

-- UPDATE

resetAlbums : Model -> Model
resetAlbums model = { model | albums = AlbumArray.resetAll model.albums }

type Action = Expand | Play AlbumArray.Action

update: Action -> Model -> Model
update action model = 
  case action of
    Expand ->
      case model.detail of
        Short -> { model | detail = Full }
        Full ->  { model | detail = Short }
    Play arrayAction ->
      { model | albums = AlbumArray.update arrayAction model.albums }

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let context = { artist = address, albums = address }
  in viewInContext context model

viewInContext : Context -> Model -> Html
viewInContext context model =
  let
    clickable name =
      li [] [a [onClick context.artist Expand ] [text name]]
    view =
      case model.detail of
        Short -> div [] [clickable model.artist]
        Full -> div []
          [ clickable model.artist
          , span [ style [ ("text-indent", "2em") ] ]
            [ AlbumArray.view (Signal.forwardTo context.albums Play) model.albums ]
          ]
  in
    div [] [ view ]

example: Model
example = init "Askew Sisters" ["One", "Two"]

main =
  start { model = example, update = update, view = view }

