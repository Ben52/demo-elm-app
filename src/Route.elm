module Route exposing (Route(..), fromUrl)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, s)


type Route
    = Home
    | Post
    | NotFound


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Home (s "home")
        , Parser.map Post (s "post")
        ]


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault NotFound (Parser.parse parser url)
