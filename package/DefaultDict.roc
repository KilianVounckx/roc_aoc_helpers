module [
    DefaultDict,
    empty,
    single,
    insert,
    get,
    remove,
    update,
]

DefaultDict k v : {
    dict : Dict k v,
    default : v,
}

empty : v -> DefaultDict k v
empty = \default -> {
    dict: Dict.empty {},
    default,
}

single : v, k, v -> DefaultDict k v
single = \default, key, value -> {
    dict: Dict.single key value,
    default,
}

insert : DefaultDict k v, k, v -> DefaultDict k v
insert = \dict, key, value -> { dict &
        dict: Dict.insert dict.dict key value,
    }

get : DefaultDict k v, k -> v
get = \{ dict, default }, key ->
    when Dict.get dict key is
        Ok value -> value
        Err KeyNotFound -> default

remove : DefaultDict k v, k -> DefaultDict k v
remove = \dict, key -> { dict &
        dict: Dict.remove dict.dict key,
    }

update :
    DefaultDict k v,
    k,
    (v -> v)
    -> DefaultDict k v
update = \dict, key, func -> { dict &
        dict: Dict.update dict.dict key \old ->
            old
            |> Result.withDefault dict.default
            |> func
            |> Ok,
    }
