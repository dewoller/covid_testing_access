calculate_distance_to_testing <- function( df_driving_time, df_crow_distances) {

  # what is the closest testing station for each meshblock

    df_driving_time %>%
    drop_na( duration) %>%
    group_by(MB_CODE16) %>%
    filter( distance == min(distance, na.rm=TRUE )) %>%
    select(id, MB_CODE16, mc_lon, mc_lat, duration, distance  ) %>%
    ungroup() 


}
