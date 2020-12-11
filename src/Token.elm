module Token exposing (..)

import Url.Parser as Parser exposing (Parser)


type alias Token =
    String


urlParser : Parser (Token -> a) a
urlParser =
    Parser.string
