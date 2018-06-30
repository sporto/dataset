module Dataset exposing (Config, putOne, putMany, deleteOne, deleteMany)


type alias Config a id =
    { getId : a -> id
    }


putOne : Config a id -> a -> List a -> List a
putOne config item existingItems =
    putMany config [ item ] existingItems


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

        updatedItems =
            existingItems
                |> List.map (replaceItem config updates)
    in
        updatedItems ++ added


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


deleteOne : Config a id -> id -> List a -> List a
deleteOne config id items =
    deleteMany config [ id ] items


deleteMany : Config a id -> List id -> List a -> List a
deleteMany config ids items =
    items |> List.filter (itemsHasIdIn config ids >> not)


findOne : Config a id -> id -> List a -> Maybe a
findOne config id items =
    items
        |> List.filter (itemsHasIdIn config [ id ])
        |> List.head



-- Utils


itemsHasIdIn : Config a id -> List id -> a -> Bool
itemsHasIdIn config ids item =
    List.member (config.getId item) ids
