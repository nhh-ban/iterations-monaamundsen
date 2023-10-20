#TASK 2 Function to make the stations_metadata into a df
transform_metadata_to_df <- function(stations_metadata) {
  df <- stations_metadata[[1]] |> 
    map(as_tibble) |> 
    list_rbind() |> 
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) |> 
    mutate(latestData = as_datetime(latestData, tz = "UTC"))  |> 
    unnest_wider(location) |> 
    unnest_wider(latLon)
  
  return(df)
}

# TASK 4a Function to return the date-time variable in iso86012
to_iso8601 <- function(datetime, offset) {
  new_datetime <- datetime + days(offset)
  
  iso8601_string <- as_datetime(new_datetime, tz = "UTC")
  
  iso8601_string_formated <- format(iso8601_string, 
                                    format= "%Y-%m-%dT%H:%M:%SZ")
  
  return(iso8601_string_formated)
}

#TASK 5 function to make sample into a data frame
transform_volumes <- function(traffic_data) {
  #indexing to the trafficData list 
  df <- traffic_data$trafficData[[1]] |>
    #making it into a tibble 
    map(as_tibble) |>
    list_rbind() |>
    #First unnesting to get to the "node" part 
    unnest_wider(edges) |>
    #unnesting again to get "to", "from", and "total"
    unnest_wider(node) |>
    #making from and to into dttm 
    mutate(from = map_chr(from, 1, .default = NA_character_), 
           to = map_chr(to, 1, .default = NA_character_)) |> 
    mutate(from = as_datetime(from, tz = "UTC"), 
           to = as_datetime(to, tz = "UTC")) |>
    #Every row is a "named list", so using map to unlist every row 
    mutate(total = map(total, unlist)) |>
    #using map_dbl to extract the volume for each row in total and making it 
    #into a double value
    mutate(
      volume = map_dbl(total, "volumeNumbers.volume")
    ) |>
    #removing total column as i dont need it 
    select(-total)
  
  return(df)
  
}


