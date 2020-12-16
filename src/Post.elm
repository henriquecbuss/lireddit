module Post exposing (..)

import User exposing (User)


type alias Post =
    { id : Float
    , title : String
    , text : String
    , points : Float
    , voteStatus : Maybe Bool
    , createdAt : String
    }


type alias PostWithUser =
    { id : Float
    , title : String
    , text : String
    , points : Float
    , voteStatus : Maybe Bool
    , creator : User
    , createdAt : String
    }


type alias PaginatedPosts =
    { posts : List PostWithUser, hasMore : Bool }
