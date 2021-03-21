calculate_intermesh_distances <- function( df_mesh_centroids, df_mesh_lockdown, df_mesh_detail, map_mesh, max_dist=5000) {


  df_mesh_centroids %>%
    inner_join(df_mesh_detail %>% select(MB_CODE16,
                                         MB_CATEGORY_NAME_2016), by='MB_CODE16') %>%
  mutate( gh = gh_encode(mc_lat, mc_lon, 5)) %>%
  mutate(ghn = gh_neighbours(gh))%>%
  do.call(data.frame, .) %>%
  as_tibble()  %>%
  rename(lon=mc_lon, lat=mc_lat) %>%
  { . } -> df_gh


# we only want the park neighbours that are within melbourne
df_gh  %>%
  inner_join( df_mesh_lockdown, by='MB_CODE16') %>%
  select( starts_with('gh')  ) %>%
  pivot_longer(starts_with('gh'), names_to=NULL, values_to ='gh_group') %>%
  distinct() %>%
  { . } -> df_from_mc_neighbours


map_mesh %>%
  select( MB_CODE16, geometry) %>%
  inner_join(  df_gh  %>%
             filter( MB_CATEGORY_NAME_2016=='Parkland') , by='MB_CODE16' ) %>%
  pivot_longer(starts_with('gh'), names_to=NULL, values_to ='gh_group') %>%
  select( gh_group, geometry  ) %>%
  inner_join( df_from_mc_neighbours, by='gh_group') %>%
  st_as_sf() %>%
  group_by(gh_group) %>%
  summarise(geometry = st_combine(geometry), .groups='drop') %>%
  as_Spatial() %>%
  clgeo_Clean() %>%
  st_as_sf() %>%
  st_transform( 3577) %>%
  #      data.frame() %>%
  #        as_tibble() %>%
  { . } -> df_to_parks

df_gh  %>%
  inner_join( df_mesh_lockdown, by='MB_CODE16') %>%
  select( -starts_with('mc'), -starts_with('ghn')  ) %>%
  rename(gh_group = gh ) %>%
  rename( from_lat = lat, from_lon=lon) %>%
  st_as_sf( coords = c("from_lon", "from_lat"), crs = 4283) %>%
  st_transform(3577) %>%
  mutate( circle = st_buffer( geometry, dist = 5000) ) %>%
  { . } -> df_from_mc

df_from_mc %>%
  st_drop_geometry() %>%
  select(-lga_name, -MB_CATEGORY_NAME_2016) %>%
  #      head(100) %>%
  group_by( gh_group ) %>%
  nest( data=c(MB_CODE16, circle )) %>%
  ungroup() %>%
  { . } -> df_pre_intersect


df_pre_intersect %>%
  inner_join( df_to_parks, by='gh_group' ) %>%
#  head(2) %>%
#  filter(gh_group=='r1px2' )  %>%
  rowwise()  %>%
  mutate(areas = calc_intersection( data, geometry, gh_group))  %>%
  ungroup() %>%
  { . } -> df_final

df_final

}



calc_intersection = function( circle, geometry, gh_group) {

  warning(gh_group)
  print(gh_group)
  browser()

  st_geometry(geometry) %>%
    st_transform( 3577)  %>%
    as_Spatial() %>%
    gSimplify( tol = 0.00001) %>%
    gBuffer( byid=TRUE, width=0) %>%
    st_as_sf() %>%
    st_transform( 3577) %>%
    { . } -> a

  circle %>%
    st_as_sf() %>%
    st_intersection( a ) %>%
    mutate( area = st_area(circle)) %>%
    st_drop_geometry() %>%
    list()

}

