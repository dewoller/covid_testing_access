
categorise_points = function( df_points = readd(df_hospitals), long_txt = 'long', lat_txt = 'lat',
                             df_map_category = read_sf("data/sa1/SA1_2016_AUST.shp"),
                             output_column = 'SA1_MAIN16')  {

  df_points %>%
    select( !!!long_txt, !!!lat_txt) %>%
    { . } -> save_ll

  replace_list = list()
  replace_list[[long_txt]]=0
  replace_list[[lat_txt]]=0

  # becasue st_intersect returns rownumber...
  df_map_category %>%
    st_drop_geometry() %>%
    mutate( intersection_row=row_number() ) %>%
    select( intersection_row, !!output_column ) %>%
    { . } -> df_map


  df_points %>%
    replace_na( replace_list ) %>%
    st_as_sf(coords = c( long_txt,lat_txt ), crs = st_crs(df_map_category)) %>%
    mutate( intersection_row = as.integer(st_intersects(geometry, df_map_category)) ) %>%
    left_join( df_map, by = 'intersection_row') %>%
    select( -intersection_row ) %>%
    st_drop_geometry() %>%
    bind_cols(save_ll)

}



