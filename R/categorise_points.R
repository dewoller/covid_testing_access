
categorise_points = function( df = readd(df_hospitals), long_txt = 'long', lat_txt = 'lat',
                             map_data = read_sf("data/sa1/SA1_2016_AUST.shp"),
                             column = 'SA1_MAIN16')  {

  #  browser()

  df %>%
    select( !!!long_txt, !!!lat_txt) %>%
    { . } -> save_ll

  replace_list = list()
  replace_list[[long_txt]]=0
  replace_list[[lat_txt]]=0


  df %>%
    replace_na( replace_list ) %>%
    st_as_sf(coords = c( long_txt,lat_txt ), crs = st_crs(map_data)) %>%
    mutate( intersection = as.integer(st_intersects(geometry, map_data)) ) %>%
    mutate( !!column := if_else(is.na(intersection),
                                '',
                                pluck(map_data, column ) %>%
                                pluck_multiple(intersection) )) %>%
    select( -intersection ) %>%
    st_drop_geometry() %>%
    bind_cols(save_ll)


}

#categorise_points()

pluck_multiple <- function(x, ...) {
  `[`(x, ...)
}

