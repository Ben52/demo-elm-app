module Utils exposing (ternary)


ternary : a -> a -> Bool -> a
ternary a b condition =
    case condition of
        True ->
            a

        False ->
            b

