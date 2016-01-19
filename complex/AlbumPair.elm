module AlbumPair where

import AlbumEntry exposing (Album, Action, update)
import Html exposing (Html, div)
import StartApp.Simple exposing (start)

type alias Model = 
  { topEntry: AlbumEntry.Album
  , bottomEntry:  AlbumEntry.Album
  }
  
type Action = Hi Int AlbumEntry.Action | Lo Int AlbumEntry.Action
  
update: Action -> Model -> Model
update action model =
  case action of
    Hi i albumAction -> { model | 
      topEntry = AlbumEntry.update albumAction model.topEntry
    , bottomEntry = AlbumEntry.update (not albumAction) model.bottomEntry
    }
    Lo i albumAction -> { model | 
      bottomEntry = AlbumEntry.update albumAction model.bottomEntry
    , topEntry = AlbumEntry.update (not albumAction) model.topEntry
    }

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ AlbumEntry.view (Signal.forwardTo address (Hi 42)) model.topEntry
    , AlbumEntry.view (Signal.forwardTo address (Lo 44)) model.bottomEntry
    ]

ex1 : Album
ex1 = AlbumEntry.init "Back in Black"
ex2 : Album
ex2 = AlbumEntry.init "In the Garden Green"
examples : Model
examples = { topEntry = ex1, bottomEntry = ex2 }
  
main =
  start { model = examples, update = update, view = view }
  

