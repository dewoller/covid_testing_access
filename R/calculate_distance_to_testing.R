calculate_distance_to_testing <- function( df_driving_time, df_crow_distances) {

  # what is the closest testing station for each meshblock
  # that has a driving duration (cause not all do!)
  df_crow_distances %>%
    select(-starts_with('mc')) %>%
    inner_join( df_driving_time, by=c("MB_CODE16","id")) %>%
    drop_na( duration) %>%
    group_by(MB_CODE16) %>%
    filter( dist == min(dist, na.rm=TRUE )) %>%
    select(id, MB_CODE16, mc_lon, mc_lat, duration ) %>%
    ungroup() %>%
    { . } -> df_closest_testing_dist

  # what is the distance to the closest testing station for each meshblock
  df_crow_distances %>%
    group_by(MB_CODE16) %>%
    filter( dist == min(dist, na.rm=TRUE )) %>%
    arrange( dist) %>%
    ungroup() %>%
    { . } -> df_closest_testing

  # how long to travel to the closest testing station
#  df_closest_testing %>%
#    select(-starts_with('mc')) %>%
#    inner_join( df_driving_time, by=c("MB_CODE16","id")) %>%
#    select(id, MB_CODE16, mc_lon, mc_lat, duration ) %>%
#    { . } -> df_closest_testing_dist

  # find the places where we had NO data
  df_closest_testing %>%
    anti_join( df_driving_time, by=c("MB_CODE16","id")) %>%
    { . } -> df_closest_testing_no_dist

  # try to get the driving time for these stations
  calculate_driving_time( df_closest_testing_no_dist) %>%
    bind_rows( df_closest_testing_dist ) %>%
    { . } -> df_rv


  df_rv

}
