
## Load your packages, e.g. library(drake).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

options(scipen = 999)


max_distance = 20000
list(

     # get  maps
     tar_target( map_mesh, read_sf( 'data/meshblocks/MB_2016_VIC.shp')),
     tar_target( map_sa1, read_sf( 'data/sa1/SA1_2016_AUST.shp') ),
     tar_target( map_sa2, read_sf( 'data/sa2/SA2_2016_AUST.shp') ),
     tar_target( map_remoteness, read_sf("data/remoteness/RA_2016_AUST.shp")),

     # load abs meshblock correspondences and details
     tar_target( df_mb2sa1,
                read_csv('data/MB_2016_VIC.csv') %>%
                  mutate( across( ends_with('_2016'), as.character )) %>%
                  rename( MB_CODE16 = MB_CODE_2016, SA1_MAIN16 = SA1_MAINCODE_2016)),

     # mesh details - population, size, etc
     tar_target( df_mesh_detail,
                read_excel("data/2016 census mesh block counts.xls", sheet="Table 2", skip=5,
                           na='Cells in this table have been randomly adjusted to avoid the release of confidential data.') %>%
                bind_rows( read_excel("data/2016 census mesh block counts.xls", sheet="Table 2.1", skip=5,
                                      na='Cells in this table have been randomly adjusted to avoid the release of confidential data.')) %>%
                drop_na(1) %>%
                mutate( MB_CODE_2016 = as.character( as.numeric( MB_CODE_2016))) %>%
                rename( MB_CODE16 = MB_CODE_2016) %>%
                mutate( MB_CODE16 = as.character( MB_CODE16) )),

     # seifa
     tar_target( df_SA1_seifa,
                read_excel('data/seifa_2033055001 - sa1 indexes.xls', skip=5, sheet='Table 2') %>%
                  select( SA1_MAIN16=2,
                         seifa=Score,
                         state=State ) %>%
                filter( state=='VIC')),

     # calculate meshblock centroids
     tar_target( df_mesh_centroids,
                map_mesh %>%
                  st_transform( 3577) %>%
                  st_centroid() %>%
                  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>% # we want the centroids in a second geometry col
                  st_coordinates() %>%
                    as_tibble() %>%
                    set_names('mc_lon','mc_lat') %>%
                    mutate( MB_CODE16 = map_mesh %>% pluck( 'MB_CODE16' ))),



     # get all thc covid testing location data
     tar_target( df_covid_test,
                read_csv( 'data/COVID19-Victorian-Testing-Clinics-Public-Spreadsheet - vic-covid-testing.csv') %>%
                  janitor::clean_names() %>%
                  mutate( id = row_number())),

     tar_target( df_covid_test_location, address_to_lonlat( df_covid_test )),


     # what is the remoteness of the test location
     tar_target( df_covid_remoteness,
                categorise_points( df_points = df_covid_test_location,
                                  long_txt='covid_lon',
                                  lat_txt='covid_lat',
                                  df_map_category=map_remoteness,
                                  output_column="RA_CODE16") %>%
                select( id, RA_CODE16 )),

     # what is the seifa of the test location. First, find SA1, then join up seifa SA1 (where exists!)
     tar_target( df_covid_seifa,
                categorise_points( df_points = df_covid_test_location,
                                  long_txt='covid_lon',
                                  lat_txt='covid_lat',
                                  df_map_category=map_sa1,
                                  output_column="SA1_MAIN16") %>%
                mutate( SA1_MAIN16 = as.numeric( SA1_MAIN16)) %>%
                left_join( df_SA1_seifa, by="SA1_MAIN16") %>%
                select( id, seifa )
              ),


     # distance calculations - first, quickly calculate crow fly distances, and exclude too distant pairs
     tar_target( df_crow_distances, calculate_crow_distances( df_covid_test_location, df_mesh_centroids)),
     tar_target( df_mesh_in_reach, exclude_distant_meshes( df_crow_distances, max_distance)),

     # calculate driving time, and distance to all pairs within reach
     tar_target( df_driving_time, calculate_driving_time( df_mesh_in_reach)),

     # calculate closest driving time, and distance to all pairs within reach
     tar_target( df_closest_duration_testing, calculate_closest_duration( df_driving_time)),

     # what is the seifa of the catchment of closest duration meshblocks
     # for all meshblocks, find their seifa ( if existing), and summarise according to their closest sites
     tar_target( df_covid_catchment_seifa,
                # find the seifa
                df_mesh_detail %>%
                  inner_join( df_mb2sa1, by='MB_CODE16') %>%
                  mutate( SA1_MAIN16 = as.numeric( SA1_MAIN16 )) %>%
                  inner_join( df_SA1_seifa, by='SA1_MAIN16' ) %>%
                  inner_join( df_closest_duration_testing, by='MB_CODE16') %>%
                  group_by( id) %>%
                  summarise( seifa_population = sum( Person ),
                            mean_catchment_seifa = mean( seifa ),
                            population_weighted_seifa = sum( seifa * Person )/seifa_population,
                            .groups='drop')
                  ),

     tar_target( df_site_summary,
                df_closest_duration_testing %>%
                  select(-starts_with('mc'), -starts_with('covid')) %>%
                  inner_join(df_mesh_detail, by = "MB_CODE16") %>%
                  group_by(id) %>%
                  summarise( area = sum(AREA_ALBERS_SQKM ),
                            dwelling = sum(Dwelling),
                            population = sum(Person),
                            mean_distance = mean(distance, na.rm=TRUE),
                            mean_duration = mean(duration, na.rm=TRUE),
                            n_mesh_blocks=n(),
                            .groups='drop'
                            ) %>%
                  inner_join( df_covid_remoteness,  by='id') %>%
                  inner_join( df_covid_seifa,  by='id') %>%
                  inner_join( df_covid_catchment_seifa,  by='id')
                  ),


     tar_target(write_mesh_block_summary,
                df_closest_duration_testing %>%
                  select(-starts_with('mc'), -starts_with('covid')) %>%
                  inner_join(df_mesh_detail, by = "MB_CODE16") %>%
                  write_csv('output/mesh_block_summary.csv')),

     #

     tar_target(write_summary_testing_site, df_covid_test_location %>% inner_join( df_site_summary, by='id') %>%
                write_csv('output/testing_site_summary.csv')),



     tar_target( dummy,TRUE)
)

