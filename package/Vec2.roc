module [
    Vec2,
    IVec2,
    UVec2,
    zero,
    add,
    addElem,
    sub,
    subElem,
    mul,
    mulElem,
    div,
    divElem,
    divTrunc,
    divTruncElem,
    map,
    turnRight,
    turnLeft,
    manhattan,
]

Vec2 a : {
    x : a,
    y : a,
}

IVec2 : Vec2 I64
UVec2 : Vec2 U64

zero : Vec2 (Num *)
zero = {
    x: 0,
    y: 0,
}

add : Vec2 (Num a), Vec2 (Num a) -> Vec2 (Num a)
add = \v1, v2 -> {
    x: v1.x + v2.x,
    y: v1.y + v2.y,
}

addElem : Vec2 (Num a), Num a -> Vec2 (Num a)
addElem = \v, t -> {
    x: v.x + t,
    y: v.y + t,
}

sub : Vec2 (Num a), Vec2 (Num a) -> Vec2 (Num a)
sub = \v1, v2 -> {
    x: v1.x - v2.x,
    y: v1.y - v2.y,
}

subElem : Vec2 (Num a), Num a -> Vec2 (Num a)
subElem = \v, t -> {
    x: v.x - t,
    y: v.y - t,
}

mul : Vec2 (Num a), Vec2 (Num a) -> Vec2 (Num a)
mul = \v1, v2 -> {
    x: v1.x * v2.x,
    y: v1.y * v2.y,
}

mulElem : Vec2 (Num a), Num a -> Vec2 (Num a)
mulElem = \v, t -> {
    x: v.x * t,
    y: v.y * t,
}

div : Vec2 (Frac a), Vec2 (Frac a) -> Vec2 (Frac a)
div = \v1, v2 -> {
    x: v1.x / v2.x,
    y: v1.y / v2.y,
}

divElem : Vec2 (Frac a), Frac a -> Vec2 (Frac a)
divElem = \v, t -> {
    x: v.x / t,
    y: v.y / t,
}

divTrunc : Vec2 (Int a), Vec2 (Int a) -> Vec2 (Int a)
divTrunc = \v1, v2 -> {
    x: v1.x // v2.x,
    y: v1.y // v2.y,
}

divTruncElem : Vec2 (Int a), Int a -> Vec2 (Int a)
divTruncElem = \v, t -> {
    x: v.x // t,
    y: v.y // t,
}

map : Vec2 a, (a -> b) -> Vec2 b
map = \v, func -> {
    x: func v.x,
    y: func v.y,
}

turnRight : Vec2 (Num a) -> Vec2 (Num a)
turnRight = \v -> {
    x: v.y,
    y: -v.x,
}

turnLeft : Vec2 (Num a) -> Vec2 (Num a)
turnLeft = \v -> {
    x: -v.y,
    y: v.x,
}

manhattan : Vec2 (Num a), Vec2 (Num a) -> Num a
manhattan = \v1, v2 ->
    Num.absDiff v1.x v2.x + Num.absDiff v1.y v2.y
