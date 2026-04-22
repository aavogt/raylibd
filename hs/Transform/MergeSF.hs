module Transform.MergeSF where
import Transform.Common
import qualified Data.Set as S
import Data.List
import Control.Lens
import qualified Data.Map as M
import Data.Ord

mergeSF :: [StateField] -> [StateField] -> [StateField]
mergeSF old new = trimTrailingDummies (reused ++ remaining)
  where
    oldDeclSet = S.fromList oldDecl
    oldDecl = map show $ buildStateMembers old
    newDecl = map show $ buildStateMembers new
    (sameDecl, notFound) = zip newDecl new & partition ((`S.member` oldDeclSet) . fst)
    sameDeclSet = S.fromList $ map fst sameDecl
    expandedDummies = zipWith dummyWhenMissing old oldDecl
    (reused, remaining) = reuseDummies expandedDummies (map snd notFound)
    oldFieldMap =
      M.fromList
        [ (fieldOrigName f, f)
          | f <- old
        ]

    newFieldMap =
      M.fromList
        [ (fieldOrigName f, f)
          | f <- new
        ]

    lookupPrevField field = M.lookup (fieldOrigName field) oldFieldMap

    lookupNewField field = M.lookup (fieldOrigName field) newFieldMap

    preferNewInit field =
      case lookupNewField field of
        Just nf ->
          case fieldInit nf of
            Just _ -> field {fieldInit = fieldInit nf}
            Nothing -> field
        Nothing -> field

    dummyWhenMissing o@StateField {..} oStr
      | oStr `S.member` sameDeclSet = preferNewInit o
      | otherwise = StateField {fieldName = "", fieldInit = Nothing, fieldMoved = False, ..}

    reuseDummies fields newFields = foldl' reuse (fields, []) newFields
      where
        reuse (fieldsAcc, acc) newField =
          case bestDummyIndex newField fieldsAcc of
            Just idx -> (replaceAt idx (markMoved newField) fieldsAcc, acc)
            Nothing -> (fieldsAcc, acc ++ [newField])

    markMoved field =
      case lookupPrevField field of
        Just _ -> field {fieldMoved = True}
        Nothing -> field

    bestDummyIndex newField fields =
      let lastRealIndex = lastNonDummyIndex fields
          candidates =
            [ (idx, scoreDummy newField field)
              | (idx, field) <- zip [0 ..] fields,
                fieldName field == "",
                fitsInDummy newField field,
                idx > lastRealIndex || exactMatch newField field
            ]
       in case candidates of
            [] -> Nothing
            _ -> Just $ fst $ minimumBy (comparing snd) candidates

    lastNonDummyIndex fields =
      case [idx | (idx, field) <- zip [0 ..] fields, fieldName field /= ""] of
        [] -> -1
        xs -> last xs

    exactMatch newField field =
      sameTypeRank && sameSize
      where
        sameTypeRank = sameTypeAndRank newField field
        sameSize =
          case (mapM constToArrayLen (fieldArraySize field), mapM constToArrayLen (fieldArraySize newField)) of
            (Just ds, Just ns) -> ds == ns
            _ -> False

    fitsInDummy newField field =
      sameTypeAndRank newField field && sizeFits
      where
        sizeFits =
          case (mapM constToArrayLen (fieldArraySize field), mapM constToArrayLen (fieldArraySize newField)) of
            (Just ds, Just ns) | length ds == length ns -> and (zipWith (>=) ds ns)
            _ -> False

    sameTypeAndRank newField field =
      show (fieldType field) == show (fieldType newField)
        && length (fieldArraySize field) == length (fieldArraySize newField)

    scoreDummy newField field =
      let exactSizePenalty = if exactMatch newField field then 0 :: Int else 1
          sizePenalty = case (mapM constToArrayLen (fieldArraySize field), mapM constToArrayLen (fieldArraySize newField)) of
            (Just ds, Just ns) | length ds == length ns -> sum (zipWith (\a b -> abs (a - b)) ds ns)
            _ -> 1000000
       in (exactSizePenalty, sizePenalty)

    replaceAt idx newField fields =
      [ if i == idx then newField else field
        | (i, field) <- zip [0 ..] fields
      ]

    trimTrailingDummies = reverse . dropWhile ((== "") . fieldName) . reverse
