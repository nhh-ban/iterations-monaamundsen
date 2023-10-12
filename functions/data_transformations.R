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
to_iso8601 <- function(datetime_variable, offset_days) {
  # Add the offset in days
  new_datetime <- datetime_variable + days(offset_days)
  
  # Convert the datetime to ISO8601 format with "Z" for UTC
  iso8601_string <- format(new_datetime, "%Y-%m-%dT%H:%M:%SZ")
  
  return(iso8601_string)
}

#TASK 5 function
transform_volumes <- function() {
  
}

