source('_drake.R')

df_distance_to_testing %>%
  anti_join( df_mesh_centroids, by='MB_CODE16')


read_csv('output/mesh_block_summary.csv') %>%
  mutate( MB_CODE16 = as.character(MB_CODE16)) %>%
  anti_join(df_distance_to_testing, by='MB_CODE16')


read_csv('output/mesh_block_summary.csv') %>%
  mutate( MB_CODE16 = as.character(MB_CODE16)) %>%
  { . } -> mbs

mbs %>%
  anti_join(df_distance_to_testing, by='MB_CODE16')

df_distance_to_testing %>%
  anti_join(mbs, by='MB_CODE16')

84,067

map_sa1 %>%
  st_drop_geometry() %>%
  filter( STE_CODE16 == 2) %>%
  distinct( SA1_MAIN16, STE_CODE16, STE_NAME16 ) %>%
  anti_join( df_mesh_sa1, by = 'SA1_MAIN16') %>%


map_sa2 %>%
  st_drop_geometry() %>%
  filter( STE_CODE16 == 2) %>%
  distinct( SA2_MAIN16, STE_CODE16, STE_NAME16 ) %>%
  anti_join( df_mesh_sa2, by = 'SA2_MAIN16')

df_mesh_sa1 %>%
  distinct( SA1_MAIN16 ) %>%


df_mesh_sa2

df_mesh_detail %>%
  anti_join( df_mesh_detail, by = "MB_CODE16")  %>%

df_closest_site %>%
  anti_join( df_mesh_detail, by = "MB_CODE16")  %>%


df_closest_site %>%
  select(-starts_with('mc'), -starts_with('covid'))  %>%
  inner_join(df_mesh_detail, by = "MB_CODE16")  %>%
  inner_join(df_distance_to_testing, by = c("MB_CODE16", 'id') ) %>%
  group_by(id)  %>%
  summarise( area = sum(AREA_ALBERS_SQKM ),
           dwelling = sum(Dwelling),
            population = sum(Person),
            mean_dist = mean(dist),
            mean_duration = mean(duration),
            n_mesh_blocks=n(),
            .groups='drop'
            ) %>%
  inner_join( df_covid_remoteness, by='id') %>%
  { . } -> df_site_summary
#
#
df_site_summary %>%
  write_csv('output/summary_testing_site.csv')



             df_mesh_detail = 

             bind_rows( read_excel("data/2016 census mesh block counts.xls", sheet="Table 2.1", skip=5 ))  %>%

options(scipen = 999) 

             read_csv("/mnt/temp/try.csv" )  %>%
             mutate( MB_CODE_2016 = as.character( as.numeric( MB_CODE_2016))) %>%
             rename( MB_CODE16 = MB_CODE_2016) %>%
             mutate( x=as.numeric( MB_CODE16 ) - 20586000000 ) %>%
             filter(  str_detect( MB_CODE16, '20586000000')) %>%
             filter( MB_CODE16 == '2058600000')

             read_excel("/mnt/temp/try.xlsx" )  %>%
             rename( MB_CODE16 = MB_CODE_2016) %>%
             filter(  str_detect( MB_CODE16, '^205860000')) %>%
             filter( MB_CODE16 == '2058600000')

             df_mesh_detail %>%
               drop_na(1) %>%
             rename( MB_CODE16 = MB_CODE_2016) %>%
             mutate( MB_CODE16 = as.character( MB_CODE16) ) %>% 
             { . } -> df_mesh_detail

           df_mesh_detail %>%
             filter( str_detect( MB_CODE16, '^20586000000'))




df_closest_site %>%
  select(-starts_with('mc'), -starts_with('covid'))  %>%
  inner_join(df_mesh_detail, by = "MB_CODE16")  %>%
  inner_join(df_distance_to_testing, by = c("MB_CODE16" ) ) %>%
  filter( id.x != id.y ) %>%
  write_csv('output/mesh_block_summary.csv')




