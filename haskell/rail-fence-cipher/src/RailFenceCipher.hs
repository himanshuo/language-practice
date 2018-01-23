module RailFenceCipher (encode, decode) where


-- Helper Functions
get_skip_counts :: Int -> Int -> [Int]
get_skip_counts key current_row =
    let edge = (key - 1) + (key -2)
        potential_left = edge - 2 * current_row
        potential_right = edge - 2 * ((key - 1) - current_row)
        potential_skip_counts = [potential_left, potential_right]
    in
      filter (\p -> p > 0) potential_skip_counts

get_updated_skip_count_index :: [Int] -> Int -> Int
get_updated_skip_count_index skip_counts skip_count_index =
  if length skip_counts == 1
    then skip_count_index
    else
      if skip_count_index == 0
        then 1
        else 0

get_indexes_to_grab :: [Int] -> Int -> [Int] -> Int -> [Int]
get_indexes_to_grab current_indexes max_index skip_counts skip_count_index =
    let current_end = last current_indexes
        potential_new = current_end + (skip_counts !! skip_count_index) + 1
    in
      if potential_new > max_index
        then current_indexes
        else
          let updated_indexes = current_indexes ++ [potential_new]
              updated_skip_count_index = get_updated_skip_count_index skip_counts skip_count_index
          in
            get_indexes_to_grab updated_indexes max_index skip_counts updated_skip_count_index

get_encoded_str :: Int -> String -> Int -> String
get_encoded_str key str current_row =
    let rest = if (key - 1) > current_row
                then get_encoded_str key str (current_row + 1)
                else ""
        skip_counts = get_skip_counts key current_row
        starting_indexes_to_grab = [current_row]
        max_index = (length str) - 1
        indexes_to_grab = get_indexes_to_grab starting_indexes_to_grab max_index skip_counts 0
        str_for_current_row = map (\i -> str !! i) indexes_to_grab
    in
        str_for_current_row ++ rest

-- End Helper Functions

encode :: Int -> String -> String
encode key str = get_encoded_str key str 0

decode :: Int -> String -> String
decode = error "You need to implement this function!"
