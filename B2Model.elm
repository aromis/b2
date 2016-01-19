module B2Model where

-- MODEL

type alias Album = String

type alias Artist =
  { name     : String
  , albums   : List Album
  }

initArtist : String -> List String -> Artist
initArtist name titles =
  { name = name
  , albums = titles
  }

-- results used elsewhere, e.g. colour of input field
type Search = Found String | NoSuch String | Empty

type alias Model =
  { artists  : List Artist
  , expanded : Maybe Artist
  , selected : Maybe Album
  , search: Search
  }

type Action = Expand Artist | Play Album | Find String | Stop | Reset

