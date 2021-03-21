calculate_distance_to_testing <- function( df_driving_time, df_crow_distances) {


  df_crow_distances %>%
    group_by(MB_CODE16) %>%
    filter( dist == min(dist) ) %>%
    #arrange( desc(dist)) %>%
    { . } -> df_closest_testing

  df_closest_testing %>%
    select(-starts_with('mc')) %>%
    inner_join( df_driving_time, by=c("MB_CODE16","id")) %>%
    select(id, MB_CODE16, mc_lon, mc_lat, duration ) %>%
    { . } -> df_closest_testing_dist

  df_closest_testing %>%
    anti_join( df_driving_time, by=c("MB_CODE16","id")) %>%
    { . } -> df_closest_testing_no_dist

  calculate_driving_time( df_closest_testing_no_dist) %>%
    bind_rows( df_closest_testing_dist ) %>%
    { . } -> df_rv



}
