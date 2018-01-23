module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop =
  let gifts = ["Partridge in a Pear Tree", "Turtle Doves", "French Hens", "Calling Birds", "Gold Rings", "Geese-a-Laying", "Swans-a-Swimming", "Maids-a-Milking", "Ladies Dancing", "Lords-a-Leaping", "Pipers Piping", "Drummers Drumming"] in
  map (\n -> recite_line n gifts) [start..stop]

recite_line :: Int -> [String] -> String
recite_line num gifts =
  let num_str = num_to_num_str num
      day = num_str_to_day num_str
      out = "On the " ++ day ++ " day of Christmas my true love gave to me, "
  in
      foldl (\acc n -> acc ++ build_portion_of_line n num gifts) out [num, (num-1)..1]

build_portion_of_line n total gifts =
  let indexInGifts = n-1
      num_str = num_to_num_str n
      value = gifts !! indexInGifts
  in
    if n == 1
    then
        let shouldUseAnd = total > 1
            last_portion = "a "++ value ++"."
        in
          if shouldUseAnd
          then "and " ++ last_portion
          else last_portion
    else
        num_str ++ " " ++ value ++ ", "


num_str_to_day :: String -> String
num_str_to_day "one" = "first"
num_str_to_day "two" = "second"
num_str_to_day "three" = "third"
num_str_to_day "five" = "fifth"
num_str_to_day "eight" = "eighth"
num_str_to_day "nine" = "ninth"
num_str_to_day "twelve" = "twelfth"
num_str_to_day num_str = num_str ++ "th"


num_to_num_str :: Int -> String
num_to_num_str num =
  let bases = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve"] in
    bases !! (num - 1)


-- On the first day of Christmas my true love gave to me, a Partridge in a Pear Tree.
--
-- On the second day of Christmas my true love gave to me, two Turtle Doves, and a Partridge in a Pear Tree.
--
-- On the third day of Christmas my true love gave to me, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.
--
-- On the fourth day of Christmas my true love gave to me, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.
--
-- On the fifth day of Christmas my true love gave to me, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.
--
-- On the sixth day of Christmas my true love gave to me, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.
--
-- On the seventh day of Christmas my true love gave to me, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.
--
-- On the eighth day of Christmas my true love gave to me, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.
--
-- On the ninth day of Christmas my true love gave to me, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.
--
-- On the tenth day of Christmas my true love gave to me, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.
--
-- On the eleventh day of Christmas my true love gave to me, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.
--
-- On the twelfth day of Christmas my true love gave to me, twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.
