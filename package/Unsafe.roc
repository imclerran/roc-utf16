module [unwrap]

unwrap : Result val err, Str -> val
unwrap = |res, msg|
    when res is
        Ok x -> x
        Err _ -> crash msg
