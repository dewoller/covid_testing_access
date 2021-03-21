calculate_driving_time <- function( df_mesh_in_reach ) {

  options(osrm.server = "http://192.168.1.97:5000/")
  getOption("osrm.server")

  # parallel::detectCores() %>%
  #   new_cluster() %>%
  #   cluster_library(c("tidyverse",'osrm')) %>%
  #   cluster_copy( "calculate_driving_time_chunk") %>%
  #   { . } -> cluster
  #
   df_mesh_in_reach %>%
    drop_na() %>%
    { . } -> df_mesh_rv

  df_mesh_rv %>%
    group_by(id) %>%
    select( id, lon = covid_lon, lat=covid_lat,  MB_CODE16, mc_lon, mc_lat ) %>%
    nest( data=c(MB_CODE16, mc_lon, mc_lat )) %>%
    do( duration = calculate_driving_time_chunk(.$id, .$lon, .$lat, data.frame(.$data) )) %>%
    unnest(duration)


}

calculate_driving_time_test <- function(  ) {

  source('_drake.R')


  readd(df_mesh_in_reach) %>%
    filter(id==min(id)) %>%
    head(100) %>%
    calculate_driving_time()

  readd(df_mesh_in_reach) %>%
    head(100000) %>%
    count(id) %>%
    filter(n==2118)

  readd(df_mesh_in_reach) %>%
    head(100000) %>%
    #filter(id==68) %>%
    group_by(id) %>%
    select( id, lon = covid_lon, lat=covid_lat,  MB_CODE16, mc_lon, mc_lat ) %>%
    nest( data=c(MB_CODE16, mc_lon, mc_lat )) %>%
#    partition(cluster=cluster) %>%
    do( duration = calculate_driving_time_chunk(.$id, .$lon, .$lat, data.frame(.$data) )) %>%
#   collect()

    print()

  options(error=recover)
  options(error=traceback)
  options(error=stop)

}

###################################################################################################
###################################################################################################
calculate_driving_time_chunk<- function( id, lon, lat, df_mesh_location ) {

#  browser()
#  df_mesh_rv %>%
#    select( id=MB_CODE16, lon = mc_lon, lat=mc_lat) %>%
#    { . } -> df_mesh_location
  #
#  df_mesh_rv %>%
#    select( id, lon = covid_lon, lat=covid_lat) %>%
#    filter(row_number()==1) %>%
#    { . } -> df_covid_location

  n_row_per_query = 10000
  nlocation = df_mesh_location %>% nrow()
  npass = floor(nlocation / n_row_per_query) + 1

df_covid_location = tribble( ~id, ~lon, ~lat, id, lon, lat  )

  rv = c()
  #t0 = Sys.time()
  for (i in 1:npass) {
  #  elapsed = Sys.time() - t0
   # eta = (t0 + (elapsed / i ) * npass - Sys.time()) / 3600
   # print(glue::glue('{i}/{npass} , {elapsed} seconds, eta {eta} hours'))
    start_index = ((i-1)*n_row_per_query)+1
    end_index = pmin( start_index + n_row_per_query-1, nlocation)

    slice = osrmTable(src = df_mesh_location[start_index:end_index,],
                      dst = df_covid_location,
                      measure='duration') %>%
    pluck('durations')


    rv = c(rv, slice)

  }
  df_mesh_location %>%
    mutate( duration = rv)
}
