module Album where

import StartApp.Simple exposing (start)
import Html exposing (Html, div, a, li, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

main =
  start { model = (init "Highway to Hell"), update = update, view = view }
  
type alias Model =
  { name: String
  , bold: Bool
  }

init: String -> Model
init name = { name = name, bold = False }

highlight album =
  if album.bold then
    [ ("font-weight", "bold") ]
   else
    [ ("font-weight", "normal") ]
  
-- UPDATE

reset : Model -> Model
reset album = { album | bold = False }

type Action = Clicked

update: Action -> Model -> Model
update action album =
  Debug.log "album update: " { album | bold = not album.bold }

-- VIEW

view : Signal.Address Action -> Model -> Html
view address album =
      li [ style (highlight album) ] [ a [onClick address Clicked] [text album.name] ]
