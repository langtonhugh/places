# Load packages.
library(leaflet)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(googleway)
library(sf)
library(ggplot2)

# Useful function(s).
seq_fun = function(a, b) {
  seq(from = a, to = b, by = 1) 
}


# Step 1 ==============================================================================
# Load in a study region and create spatial data for query.

# Load in a geographic study region.
my_region_sf <- st_read("Data/my_region.shp")

# Make a square grid over the study region. A 50 meter circular buffer is used
# for the query, so 70 meters square grid is recommended to capture everything.
squ_region_sf <- my_region_sf %>% 
  st_make_grid(cellsize = 70) 

# Obtain the centroids of the grids.
centroids_sf <- squ_region_sf %>% 
  st_centroid() %>% 
  st_as_sf()

# Handle the coordinates so that they work in the query. Note
# edits or additions according to CRS.
centroids_rev_sf <- centroids_sf %>%
  st_transform(crs = 4326) %>% 
  mutate(long = unlist(map(geometry, 1)),
         lat  = unlist(map(geometry, 2))) %>% 
  select(long, lat) %>% 
  as_tibble() %>% 
  st_as_sf(coords = c(x = "lat", y = "long"), crs = 4326) %>% 
  pluck("geometry")

# Step 2 =======================================================================
# Specify the places types that we want and make the first query.

# Set the key. This needs to be generated.
gkey <- "my_api_key_code_here" 

# Create vector with place(s) types of interest for testing. In this example
# we use cafes only.
vplace_types <- c("cafe")

# Make empty list to store the results.
final_result <- list()

# Loop through the list of places for a query.

for (p in seq_along(centroids_rev_sf)){  
  
  result_nested <- list()

for (i in seq_along(vplace_types)){
  
  search_amsloop <- google_places(location   = centroids_rev_sf[[p]],
                                  place_type = vplace_types[[i]],
                                  radius     = 50, 
                                  key        = gkey)

queryams_df <- tibble(
    id      = search_amsloop$result$place_id,
    name    = search_amsloop$result$name,
    results = search_amsloop$result
    )

result_nested[[i]] <- queryams_df

}

final_result[[p]] <- result_nested
  
}

# Each element in the list is buffer, and within each element, are the places,
# depending on what we used above.
class(final_result)
class(final_result[[1]])
class(final_result[[1]][[1]])

# Remove elements with no results.
final_result_nm <- lapply(final_result, function(x){
  x[sapply(x, nrow)>0]
})

# Loop through the list of lists, binding the different places together,
# for each buffer.
final_result_unnest <- lapply(final_result_nm, function(x){
  x %>% 
    bind_rows() %>% 
    as_tibble() 
}
)

# Function to unnest the results.
unnest_fun <- function(x){
  tibble(
    id    = x$id,
    name  = x$name,
    lat   = x$results$geometry$location$lat,
    lon   = x$results$geometry$location$lng,
    types = x$results$types
  )
}

# Run that function through the list.
list_place_result <- lapply(final_result_unnest, unnest_fun)

# Bind the results together.
list_place_df <- bind_rows(list_place_result)

# Identify duplicates.
places_dedup_df <- list_place_df %>% 
  distinct(id, .keep_all = TRUE)

# Make spatial. Note CRS edits.
places_dedup_sf <- places_dedup_df %>% 
  st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326) 

# Step 3 =======================================================================
# Use the ids obtained to run another query for the opening times.

# First create a vector of the ids.
results_id_vec <- places_dedup_sf$id

# Then make an empty list to store the results.
id_openings <- list()

# Run the query and do some data handling on the results. Assign to the list.
for (i in results_id_vec){
  
  query_i <- google_place_details(place_id = i, simplify = TRUE, key = gkey)
  
  query_df <- tibble(
    id = query_i$result$place_id,
    opening_times   = query_i$result$opening_hours$weekday_text 
  ) 

  id_openings[[i]] <- query_df

}

# Remove tables from the list that are empty. Note that this is missing data.
id_openings_nm <- id_openings %>%
  discard(~ nrow(.x) <= 1)

# Clean those that are remaining.
id_openings_clean <- lapply(id_openings_nm, function(x){
  x %>%
    separate(col = opening_times, into = c("day_of_week", "opening_hours"), sep = ": ") %>%
    separate(col = opening_hours, into = c("opening"    , "closing"), sep =  " – ")
    
})

# Bind the results together.
id_openings_clean_df <- bind_rows(id_openings_clean)

# For many cafes do we have opening times?
length(unique(id_openings_clean_df$id))

# What percentage of the scraped cafes do we have opening times?
length(unique(id_openings_clean_df$id)) / nrow(places_dedup_sf)

# For which ids do we not have opening hours?
places_dedup_my_region_sf <- places_dedup_my_region_sf %>% 
  mutate(opening_times_info = if_else(condition = id %in% id_openings_clean_df$id,
         true  = "yes",
         false = "no")) 

# Step 4 =======================================================================
# Handle the hourly data.

# Remove non-numeric these so we can handle the opening times first.
# We also remove extra info on closing times (double times).
id_openings_open_only_df <- id_openings_clean_df %>% 
  filter(opening != "Closed" & opening != "Open 24 hours") %>% 
  separate(col = closing, into = c("closing", "extra_closing"), sep = "," ) %>% 
  select(-extra_closing)

# Handle over-night closing hours.
weekly_hour_open_df <- id_openings_open_only_df %>% 
  rename(day_opening = day_of_week) %>% 
  mutate(opening_lub      = parse_date_time(opening, '%I:%M %p'),
         opening_lub_hour = hour(opening_lub),
         closing_lub      = parse_date_time(closing, '%I:%M %p'),
         closing_lub_hour = hour(closing_lub),
         next_day_closing = if_else(closing_lub_hour < opening_lub_hour,
                                    "next day",
                                    "same day"),
         day_num_opening = recode(day_opening,
                                  Monday    = 1,
                                  Tuesday   = 2,
                                  Wednesday = 3,
                                  Thursday  = 4,
                                  Friday    = 5,
                                  Saturday  = 6,
                                  Sunday    = 7),
         day_num_closing = if_else(next_day_closing == "next day",
                                   day_num_opening + 1,
                                   day_num_opening),
         day_num_closing  = if_else(day_num_closing == 8, 1, day_num_closing),
         opening_hour_168 = opening_lub_hour + 24*(day_num_opening-1),
         closing_hour_168 = closing_lub_hour + 24*(day_num_closing-1),
         closing_hour_trunc_168 = if_else(day_opening == "Sunday" & closing_hour_168 < 144,
                                          167,
                                          closing_hour_168),
         open_span_list_1 = mapply(seq_fun, a = opening_hour_168, b = closing_hour_trunc_168),
         open_span_list_2 = ifelse(closing_hour_trunc_168 == 167 & closing_hour_168 !=0,
                                   mapply(seq_fun, a = 0, b = closing_hour_168), NA),
         open_span_char_1 = str_remove_all(as.character(open_span_list_1),"c\\(|\\)"),
         open_span_char_2 = str_remove_all(as.character(open_span_list_2),"c\\(|\\)"),
         open_span_char_12 = paste(open_span_char_1, open_span_char_2, sep = ","),
         open_span_char_12 = str_remove_all(open_span_char_12, " ")) %>% 
  select(id, open_span_char_12) %>% 
  separate_rows(open_span_char_12, sep = ",") %>% 
  filter(open_span_char_12 != "NA") %>% 
  mutate(weekly_opening_hours = as.numeric(open_span_char_12)) %>% 
  select(-open_span_char_12)

# Open 24 hours places.
id_openings_ao_df <- id_openings_clean_df %>% 
  filter(opening == "Open 24 hours") %>% 
  distinct(id) %>%
  mutate(weekly_opening_hours = paste(0:167, collapse=",")) %>%
  separate_rows(weekly_opening_hours, sep = ",") %>% 
  mutate(weekly_opening_hours = as.numeric(weekly_opening_hours))

# Bind with the normal ones.
weekly_hour_open_complete_df <- weekly_hour_open_df %>% 
  bind_rows(id_openings_ao_df) %>% 
  mutate(open_flag = 1)

# Create the hypothetical 'complete' df for which all our open 24/7.
hypothetical_df <- tibble(
  id = unique(weekly_hour_open_complete_df$id),
  weekly_opening_hours = paste(0:167, collapse=",")
) %>% 
  separate_rows(weekly_opening_hours, sep = ",") %>% 
  mutate(weekly_opening_hours = as.numeric(weekly_opening_hours))

# Create a mirrored data frame but with the *closed* hours.
weekly_hour_closed_complete_df <- hypothetical_df %>% 
  anti_join(weekly_hour_open_complete_df) %>% 
  mutate(open_flag = 0)

# Bind together.
weekly_hour_complete_df <- bind_rows(weekly_hour_closed_complete_df, 
                                     weekly_hour_open_complete_df) %>% 
  arrange(id, weekly_opening_hours)

# Deal with the closed days.
id_openings_cad_df <- id_openings_clean_df %>% 
  filter(opening == "Closed") %>% 
  mutate(closed_hours = ifelse(day_of_week == "Monday"   , paste( 0      :(24*1), collapse = ",")  , NA),
         closed_hours = ifelse(day_of_week == "Tuesday"  , paste((24*1+1):(24*2), collapse = ",")  , closed_hours),
         closed_hours = ifelse(day_of_week == "Wednesday", paste((24*2+1):(24*3), collapse = ",")  , closed_hours),
         closed_hours = ifelse(day_of_week == "Thursday" , paste((24*3+1):(24*4), collapse = ",")  , closed_hours),
         closed_hours = ifelse(day_of_week == "Friday"   , paste((24*4+1):(24*5), collapse = ",")  , closed_hours),
         closed_hours = ifelse(day_of_week == "Saturday" , paste((24*5+1):(24*6), collapse = ",")  , closed_hours),
         closed_hours = ifelse(day_of_week == "Sunday"   , paste((24*6+1):(24*7)-1, collapse = ","), closed_hours)) %>% 
  separate_rows(closed_hours, sep = ",") %>% 
  mutate(closed_hours = as.numeric(closed_hours),
         open_flag = 0) %>% 
  rename(weekly_opening_hours = closed_hours) %>% 
  select(id, weekly_opening_hours, open_flag) %>% 
  arrange(id, weekly_opening_hours)

# Bind back.
weekly_hour_complete_closed_df <- weekly_hour_complete_df %>% 
  bind_rows(id_openings_cad_df)

# We have some duplicate info now. This gives us the final thing.
cafes_weekly_hours_df <- weekly_hour_complete_closed_df %>% 
  distinct()

# Check the number of duplicates that were removed.
nrow(weekly_hour_complete_closed_df) - nrow(cafes_weekly_hours_df)

# Check the hours.
unique(cafes_weekly_hours_df$weekly_opening_hours)

# Step 5 =======================================================================
# Join the opening times data back with the spatial points.

# Long to wide for the join.
cafes_weekly_hours_wide_df <- cafes_weekly_hours_df %>% 
  distinct(id, weekly_opening_hours, .keep_all = TRUE) %>% # !!! 
  pivot_wider(id_cols = id,
              names_from = weekly_opening_hours, names_prefix = "week_hour_",
              values_from = open_flag) 

# Join back with the spatial data.
places_opening_sf <- places_dedup_my_region_sf %>% 
  left_join(cafes_weekly_hours_wide_df) 

# Check missings.
sum(is.na(places_opening_sf$week_hour_167)) / nrow(places_opening_sf)

# Load in roads network data.
roads_sf <- st_read("Data/road_networks.shp")

# snap point to closest street segment 
line_cafes_df <- places_opening_sf %>% 
  st_join(roads_sf, join = st_nearest_feature) %>% 
  select(id: WVK_ID, name) %>%
  as_tibble() 

# Join the cafes back with the street spatial data.
cafes_weg_df <- roads_sf %>% 
  as_tibble() %>% 
  select(WVK_ID, geom) %>%
  left_join(line_cafes_df) %>% 
  drop_na(id) %>% 
  select(WVK_ID, id, week_hour_0:week_hour_167)

# Remove those cafes which are open 24 hours (if wanted).
cafes_weg_df <- cafes_weg_df %>% 
  filter(id %nin% unique(id_openings_ao_df$id))

# Wide to long for more usable format. Complete with zeros.
cafes_weg_long_df <- cafes_weg_df %>% 
  pivot_longer(cols = week_hour_0:week_hour_167,
               names_to = "week_hour", values_to = "open", names_prefix = "week_hour_") %>% 
  mutate(week_hour = as.numeric(week_hour)) %>% 
  group_by(WVK_ID, week_hour) %>% 
  summarise(count_open = sum(open)) %>% 
  ungroup() %>% 
  complete(WVK_ID, week_hour,fill = list(count_open = 0)) %>% 
  arrange(WVK_ID, week_hour)

# Back to wide for join.
cafes_weg_wide_df <- cafes_weg_long_df %>% 
  pivot_wider(id_cols = WVK_ID, names_from = week_hour, values_from = count_open, names_prefix = "week_hour_")

# Join back with spatial streets.
cafes_weg_sf <- roads_sf %>% 
  select(WVK_ID) %>% 
  left_join(cafes_weg_wide_df) %>% 
  replace(is.na(.), 0)

# End