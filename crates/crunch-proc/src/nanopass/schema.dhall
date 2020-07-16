{ config : { logging : Bool }
, passes :
    List
        { description : Optional Text
        , functionContext : < Immutable : Text | Mutable : Text | None >
        , functionName : Text
        , functionVis : < Crate | Private | Public | Super >
        , inputEnum : Text
        , name : Text
        , outputEnum : Optional Text
        , transformations :
            List
                { inputVariant : Text
                , operation :
                    < Create : Text | Merge : Text | Replace : Text | Scan >
                , userFunction : Text
                }
        }
}
