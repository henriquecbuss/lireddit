module GraphQL exposing (mutation, query)

import Graphql.Http exposing (Request)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)



-- ENDPOINT


endpoint : String
endpoint =
    "http://localhost:4000/graphql"



-- MUTATION


mutation :
    SelectionSet decodesTo RootMutation
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg
mutation selectionSet toMsg =
    selectionSet
        |> Graphql.Http.mutationRequest endpoint
        |> Graphql.Http.send toMsg



-- QUERY


query :
    SelectionSet decodesTo RootQuery
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg
query selectionSet toMsg =
    selectionSet
        |> Graphql.Http.queryRequest endpoint
        |> Graphql.Http.send toMsg
