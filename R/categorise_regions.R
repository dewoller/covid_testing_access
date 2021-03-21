if (FALSE) {
  map_data1 = readd(map_mesh)
  map_data2 = readd(map_sa1)
  map_data3 = readd(map_sa2)

}
categorise_regions = function(
                              map_data1, id_column1,
                              map_data2, id_column2)  {

  #  browser()
  a= st_contains(map_data1, map_data2)

  map(a,length) %>%
    unlist() %>%
    enframe() %>%
    count(value)

  b= st_intersects(map_data1, map_data3)

  map(b,length) %>%
    unlist() %>%
    enframe() %>%
    count(value)


  c=st_intersects( st_transform(map_data1, 3577), st_transform( map_data2, 3577))

  map(c,length) %>%
    unlist() %>%
    enframe() %>%
    count(value)




  map_data1 %>%
    mutate( intersection = st_intersects(map_data1, map_data2))  %>%
    { . } -> a


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

