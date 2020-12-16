module Post.PostId exposing (PostId(..), getId, toString, urlParser)

import Url.Parser exposing (Parser)



-- TYPES


type PostId
    = PostId Int



-- CREATE


urlParser : Parser (PostId -> a) a
urlParser =
    Url.Parser.custom "POSTID" (Maybe.map PostId << String.toInt)



-- TRANSFORM


getId : PostId -> Int
getId (PostId x) =
    x


toString : PostId -> String
toString (PostId x) =
    String.fromInt x
