elm-csv-decode
====

[![Build Status](https://travis-ci.org/jinjor/elm-csv-decode.svg)](https://travis-ci.org/jinjor/elm-csv-decode)

CSV Decoder for Elm.

```elm
type alias User =
    { id : String
    , name : String
    , age : Int
    , mail : Maybe String
    }


userDecoder : Decoder User
userDecoder =
    succeed User
        |= field "id"
        |= field "name"
        |= int (field "age")
        |= optional (field "mail")


source : String
source =
    """
id,name,age
1,John Smith,20
2,Jane Smith,19
"""


> CsvDecode.run userDecoder source
Ok
    [ { id = "1", name = "John Smith", age = 20, mail = Nothing }
    , { id = "2", name = "Jane Smith", age = 19, mail = Nothing }
    ]
```


## LICENSE

BSD-3-Clause
