address_to_lonlat <- function( df_covid_test ) {

  read_csv( 'data/geocoded_addresses_to_check.csv')

}

address_to_lonlat_raw <- function( df_covid_test ) {
  my_geocode = memoise::memoise(geocode)

  df_covid_test %>%
    mutate(full_address = glue::glue('{site_name} {address} {suburb} {state} {postcode}', .na='')) %>%
    { . } -> rv

  rv %>%
    pluck('full_address') %>%
    my_geocode( output='all') %>%
    { . } -> all_geocodes

  tibble(
         g_address=map( all_geocodes, pluck, 'results', 1, 'formatted_address') ,
         covid_lat=map( all_geocodes, pluck, 'results', 1, 'geometry', 'location','lat') ,
         covid_lon=map(all_geocodes, pluck, 'results', 1, 'geometry', 'location','lng'),
         g_types= map( all_geocodes, pluck, 'results', 1, 'types') %>% map( paste, collapse=' ' )
         ) %>%
  unnest(everything()) %>%
  { . } -> geocoded_address


rv %>%
  bind_cols(  geocoded_address) %>%
  { . } -> rv_out

rv_out %>% write_csv('data/geocoded_addresses_to_check.csv')

rv_out


}
