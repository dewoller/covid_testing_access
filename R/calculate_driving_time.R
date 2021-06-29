calculate_driving_time <- function( df_mesh_in_reach ) {
# calculate for each id
  options(osrm.server = "http://127.0.0.1:5000/")
  getOption("osrm.server")

  tictoc::tic('finished overall driving time calculation')
  #
  df_mesh_in_reach %>%
    filter( id<3) %>%
    group_by(id) %>%
    select( id, lon = covid_lon, lat=covid_lat,  MB_CODE16, mc_lon, mc_lat ) %>%
    nest( data=c(MB_CODE16, mc_lon, mc_lat )) %>%
    do( duration = calculate_driving_time_chunk(.$id, .$lon, .$lat, data.frame(.$data) )) %>%
    unnest(duration) %>%
    { . } -> df_driving_time

  tictoc::toc()

  df_driving_time
}


###################################################################################################
###################################################################################################
calculate_driving_time_chunk<- function( id, lon, lat, df_mesh_location ) {

  tictoc::tic(paste('Finished calculating time to drive to ', id))

  n_row_per_query = 10000
  nlocation = df_mesh_location %>% nrow()
  npass = floor(nlocation / n_row_per_query) + 1

  df_covid_location = tribble( ~id, ~lon, ~lat, id, lon, lat  )

  rv_durations = c()
  rv_distances = c()
  # break the osrm queries to 10K at a time
  for (i in 1:npass) {
    start_index = ((i-1)*n_row_per_query)+1
    end_index = pmin( start_index + n_row_per_query-1, nlocation)

    slice_out = osrmTable(src = df_mesh_location[start_index:end_index,],
                      dst = df_covid_location,
                      measure=c('duration', 'distance'))

    slice_durations = pluck(slice_out, 'durations')
    slice_distances = pluck(slice_out, 'distances')

  rv_durations = c(rv_durations, slice_durations)
  rv_distances = c(rv_distances, slice_distances)

  }
  tictoc::toc()

  df_mesh_location %>%
    mutate(
           duration = rv_durations,
           distance = rv_distances)
}
