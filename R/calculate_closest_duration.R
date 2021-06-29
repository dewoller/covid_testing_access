calculate_closest_duration <- function( df_driving_time) {


    df_driving_time %>%
    drop_na( duration) %>%
    group_by(MB_CODE16) %>%
    filter( duration == min(duration, na.rm=TRUE )) %>%
    select(id, MB_CODE16, mc_lon, mc_lat, duration, distance  ) %>%
    ungroup()


}
