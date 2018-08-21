module ProteinTranslation(proteins) where

codonToProtein :: String -> Either String String
codonToProtein "AUG" = Right "Methionine"
codonToProtein "UUU" = Right "Phenylalanine"
codonToProtein "UUC" = Right "Phenylalanine"
codonToProtein "UUA" = Right "Leucine"
codonToProtein "UUG" = Right "Leucine"
codonToProtein "UCU" = Right "Serine"
codonToProtein "UCC" = Right "Serine"
codonToProtein "UCA" = Right "Serine"
codonToProtein "UCG" = Right "Serine"
codonToProtein "UAU" = Right "Tyrosine"
codonToProtein "UAC" = Right "Tyrosine"
codonToProtein "UGU" = Right "Cysteine"
codonToProtein "UGC" = Right "Cysteine"
codonToProtein "UGG" = Right "Tryptophan"
codonToProtein "UAA" = Left "STOP"
codonToProtein "UAG" = Left "STOP"
codonToProtein "UGA" = Left "STOP"
codonToProtein _ = Left "Bad Codon"

group :: String -> Maybe [String]
group ([]) = Just []
group str =
  if (length str) `rem` 3 /= 0
  then Nothing
  else
    let current = take 3 str in
    let rest = group $ drop 3 str in
    case rest of
      Nothing -> Nothing
      Just rest' -> Just $ [current] ++ rest'

convert ::  ([String], Bool) -> String -> ([String], Bool)
convert (acc, True) codon =
  let protein = codonToProtein codon in
  case protein of
    Left _ -> (acc, False)
    Right val ->  (acc ++ [val], True)
convert (acc, False) _ = (acc, False)

proteins :: String -> Maybe [String]
proteins proteinString =
  let codons = group proteinString in
  case codons of
    Nothing -> Nothing
    Just codons' ->
      let out = foldl convert ([], True) codons' in
      Just $ fst out