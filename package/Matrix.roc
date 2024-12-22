module [
    Matrix,
    Index,
    fromList,
    fromRows,
    repeat,
    toList,
    toRows,
    width,
    height,
    indexFromVec2,
    indexToVec2,
    get,
    set,
    swap,
    map,
    reverse,
    reverseEachRow,
    transpose,
    findPositions,
    nextIndex,
]

import Vec2 exposing [Vec2]

Matrix a := {
    width : U64,
    height : U64,
    buffer : List a,
}
    implements [Inspect]

Index : {
    row : U64,
    column : U64,
}

fromList : List a, { width : U64, height : U64 } -> Result (Matrix a) [InvalidDimensions]
fromList = \buffer, { width: w, height: h } ->
    if List.len buffer != w * h then
        Ok (@Matrix { buffer, width: w, height: h })
    else
        Err InvalidDimensions

fromRows : List (List a) -> Result (Matrix a) [InvalidDimensions]
fromRows = \rows ->
    when rows is
        [] -> Ok (@Matrix { buffer: [], width: 0, height: 0 })
        [first, ..] ->
            w = List.len first
            h = List.len rows
            buffer =
                rows
                |> List.mapTry? \row ->
                    if List.len row != w then
                        Err InvalidDimensions
                    else
                        Ok row
                |> List.join
            Ok (@Matrix { buffer, width: w, height: h })

repeat : a, { width : U64, height : U64 } -> Matrix a
repeat = \x, { width: w, height: h } -> @Matrix {
        buffer: List.repeat x (w * h),
        width: w,
        height: h,
    }

toList : Matrix a -> List a
toList = \@Matrix { buffer } -> buffer

toRows : Matrix a -> List (List a)
toRows = \@Matrix { buffer, width: w, height: h } ->
    List.range { start: At 0, end: Length h }
    |> List.map \index ->
        List.sublist buffer { start: index * w, len: w }

height : Matrix * -> U64
height = \@Matrix { height: h } -> h

width : Matrix * -> U64
width = \@Matrix { width: w } -> w

indexFromVec2 : Vec2 U64 -> Index
indexFromVec2 = \{ x, y } -> {
    row: y,
    column: x,
}

indexToVec2 : Index -> Vec2 U64
indexToVec2 = \{ row, column } -> {
    x: column,
    y: row,
}

get : Matrix a, Index -> Result a [RowOutOfBounds, ColumnOutOfBounds]
get = \@Matrix matrix, index ->
    bufferIndex = try listIndex (@Matrix matrix) index
    when List.get matrix.buffer bufferIndex is
        Ok x -> Ok x
        Err OutOfBounds -> crash "unreachable, dims already checked"

set : Matrix a, Index, a -> Matrix a
set = \@Matrix matrix, index, x ->
    when listIndex (@Matrix matrix) index is
        Err _ -> @Matrix matrix
        Ok bufferIndex ->
            @Matrix
                { matrix &
                    buffer: List.set matrix.buffer bufferIndex x,
                }

swap : Matrix a, Index, Index -> Matrix a
swap = \@Matrix matrix, index1, index2 ->
    bufferIndex1 = listIndex (@Matrix matrix) index1
    bufferIndex2 = listIndex (@Matrix matrix) index2
    when (bufferIndex1, bufferIndex2) is
        (Ok i1, Ok i2) ->
            @Matrix
                { matrix &
                    buffer: List.swap matrix.buffer i1 i2,
                }

        _ ->
            @Matrix matrix

map : Matrix a, (a -> b) -> Matrix b
map = \@Matrix { buffer, width: w, height: h }, func ->
    @Matrix {
        buffer: List.map buffer func,
        width: w,
        height: h,
    }

reverse : Matrix a -> Matrix a
reverse = \matrix ->
    when
        matrix
        |> toRows
        |> List.reverse
        |> fromRows
    is
        Ok m -> m
        Err InvalidDimensions -> crash "reverse is valid matrix"

reverseEachRow : Matrix a -> Matrix a
reverseEachRow = \matrix ->
    when
        matrix
        |> toRows
        |> List.map List.reverse
        |> fromRows
    is
        Ok m -> m
        Err InvalidDimensions -> crash "reverseEachRow is valid matrix"

transpose : Matrix a -> Matrix a
transpose = \@Matrix { buffer, width: w, height: h } ->
    newBuffer =
        List.range { start: At 0, end: Before h }
        |> List.walk buffer \acc, row ->
            List.range { start: At 0, end: Before w }
            |> List.walk acc \acc1, column ->
                index =
                    when
                        listIndex
                            (@Matrix { buffer: [], width: w, height: h })
                            { row, column }
                    is
                        Ok i -> i
                        Err _ -> crash "transpose is valid matrix"
                value =
                    when List.get buffer index is
                        Ok x -> x
                        Err OutOfBounds -> crash "transpose is valid matrix"
                List.set acc1 (column * w + row) value
    @Matrix { buffer: newBuffer, width: h, height: w }

findPositions : Matrix a, a -> List Matrix.Index where a implements Eq
findPositions = \matrix, value ->
    List.range { start: At 0, end: Before (Matrix.height matrix) }
    |> List.map \row ->
        List.range { start: At 0, end: Before (Matrix.width matrix) }
        |> List.keepOks \column ->
            if Matrix.get matrix { row, column } == Ok value then
                Ok { column, row }
            else
                Err NotX
    |> List.join

nextIndex :
    Index,
    Matrix *
    ->
    Result Index [RowOutOfBounds, ColumnOutOfBounds, FinalIndex]
nextIndex = \{ row, column }, @Matrix { width: w, height: h } ->
    if column >= w then
        Err ColumnOutOfBounds
    else if row >= h then
        Err RowOutOfBounds
    else if column + 1 >= w then
        if row + 1 >= h then
            Err FinalIndex
        else
            Ok { row: row + 1, column }
    else
        Ok { row, column: column + 1 }

listIndex : Matrix *, Index -> Result U64 [RowOutOfBounds, ColumnOutOfBounds]
listIndex = \@Matrix { width: w, height: h }, { row, column } ->
    if row >= h then
        Err RowOutOfBounds
    else if column >= w then
        Err ColumnOutOfBounds
    else
        Ok (row * w + column)
