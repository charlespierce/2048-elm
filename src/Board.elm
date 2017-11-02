module Board
    exposing
        ( Tile(..)
        , Board
        , Direction(..)
        , shift
        , isWinningBoard
        , isLosingBoard
        , replace
        , getEmptyIndices
        )

import List exposing (..)


type Tile
    = Empty
    | Tile Int


type alias Board =
    List (List Tile)


type alias Row =
    List Tile


type Direction
    = Left
    | Right
    | Up
    | Down


shift : Direction -> Board -> Board
shift direction =
    case direction of
        Left ->
            shiftRows collapse

        Right ->
            shiftRows collapseReverse

        Up ->
            shiftColumns collapse

        Down ->
            shiftColumns collapseReverse


isWinningBoard : Board -> Bool
isWinningBoard =
    (==) (Tile 2048) |> List.any |> List.any


isLosingBoard : Board -> Bool
isLosingBoard board =
    let
        rowLocked row =
            row == collapse row

        locked =
            List.all rowLocked
    in
        locked board && (transpose >> locked) board


collapse : Row -> Row
collapse row =
    case row of
        [] ->
            []

        [ x ] ->
            [ x ]

        Empty :: xs ->
            collapse xs ++ [ Empty ]

        x :: Empty :: xs ->
            collapse (x :: xs) ++ [ Empty ]

        (Tile x) :: (Tile y) :: xs ->
            if x == y then
                Tile (x + y) :: collapse xs ++ [ Empty ]
            else
                Tile x :: collapse (Tile y :: xs)


collapseReverse : Row -> Row
collapseReverse =
    List.reverse >> collapse >> List.reverse


shiftRows : (Row -> Row) -> Board -> Board
shiftRows =
    List.map


shiftColumns : (Row -> Row) -> Board -> Board
shiftColumns f =
    transpose >> List.map f >> transpose


transpose : Board -> Board
transpose board =
    case board of
        [] ->
            []

        _ ->
            filterMap head board :: transpose (filterMap tail board)


replace : Int -> Int -> Tile -> Board -> Board
replace x y newElement matrix =
    let
        replaceCol col element =
            if col /= y then
                element
            else
                newElement

        replaceRow row list =
            if row /= x then
                list
            else
                indexedMap replaceCol list
    in
        indexedMap replaceRow matrix


boardIndexedMap : (Int -> Int -> Tile -> a) -> Board -> List (List a)
boardIndexedMap f board =
    let
        mapRows row list =
            indexedMap (f row) list
    in
        indexedMap mapRows board


getEmptyIndices : Board -> List ( Int, Int )
getEmptyIndices board =
    let
        findEmpty row col tile =
            case tile of
                Empty ->
                    Just ( row, col )

                Tile _ ->
                    Nothing
    in
        board |> boardIndexedMap findEmpty >> concat >> filterMap identity
