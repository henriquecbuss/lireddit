module Post exposing (..)


type alias Post =
    { id : Float, title : String, text : String, createdAt : String }


type alias PaginatedPosts =
    { posts : List Post, hasMore : Bool }
