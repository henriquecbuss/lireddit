module Post exposing (..)

import Post.PostId exposing (PostId)
import User exposing (User)


type alias Post =
    { id : PostId
    , title : String
    , text : String
    , points : Float
    , voteStatus : Maybe Bool
    , createdAt : String
    }


type alias PostWithUser =
    { id : PostId
    , title : String
    , text : String
    , points : Float
    , voteStatus : Maybe Bool
    , creator : User
    , createdAt : String
    }


type alias PaginatedPosts =
    { posts : List PostWithUser, hasMore : Bool }
