module Dataset exposing (Config, putOne, putMany)


type alias Config a id =
    { getId : a -> id
    }


putOne : Config a id -> a -> List a -> List a
putOne config item existingItems =
    putMany config [item] existingItems


putMany : Config a id -> List a -> List a -> List a
putMany config newItems existingItems =
    let
        existingIds =
            List.map config.getId existingItems

        isInExistingIds item =
            List.member (config.getId item) existingIds

        ( updates, added ) =
            newItems
                |> List.partition isInExistingIds

        _ =
            Debug.log "added" added
    in
        existingItems
            |> List.map (replaceItem config updates)
            |> List.append added


replaceItem : Config a id -> List a -> a -> a
replaceItem config updatedItems currentItem =
    let
        itemId =
            config.getId currentItem
    in
        case findOne config itemId updatedItems of
            Just item ->
                item

            Nothing ->
                currentItem


findOne : Config a id -> id -> List a -> Maybe a
findOne config id items =
    items
        |> List.filter (itemsHasId config id)
        |> List.head


itemsHasId : Config a id -> id -> a -> Bool
itemsHasId config id item =
    config.getId item == id



