source('_drake.R')



read_excel("data/meshblock-correspondence-file-asgs-2016.xlsx")

loadd(df_segments_sa_remote)
loadd(melbourne_net)

net %>%
  as_tibble() %>%
  count(highway, sort=TRUE)

df_segments_sa_remote %>%
  head(1) %>%
  { . } -> a


 route_dodgr (net = melbourne_net, from = c(a$lon, a$lat), to = c(a$target_lon, a$target_lat))

  from <- c(-0.12, 51.5)
  to <- c(-0.14, 51.5)
  r1 <- route_graphhopper(from = from, to = to, vehicle='car' , silent = FALSE)

  r1 <- route_dodgr(from = from, to = to,  net=melbourne_net)

  plot(r1)




    trip <- route_osrm(from = c(-1, 53), to = c(-1.1, 53))

  distA <- osrmTable(loc = apotheke.df[1:5, c("id","lon","lat")])

  trips <- osrmTrip(loc = apotheke.sf[1:2,])

  data("berlin")
  library(stplanr)

 library(sf)


 library(osrm)

 help(osrm)



 df_covid_test_location  %>%
   filter( id == 117 )

  mutate( centroid = sfc_as_cols( centroid) ) %>%
  unnest( centroid)

map_mesh  %>%
  st_transform( 3577) %>%
  st_centroid() %>%
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  # since you want the centroids in a second geometry col:
  st_geometry() %>%
  mutate( centroid = sfc_as_cols( centroid) ) %>%

  enframe(name=NULL, value='centroid') %>%
  mutate( MB_CODE16 = map_mesh %>% pluck( 'MB_CODE16' )),


map_mesh$centroid = df_mesh_centroids

map_mesh

map_mesh %>%
  st_set_geometry(NULL)


  pluck('centroid')
