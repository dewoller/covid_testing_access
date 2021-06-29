source('_drake.R')
loadd(df_mesh_detail)

df_mesh_detail %>%
  filter( AREA_ALBERS_SQKM >0 ) %>%
  summarise(
            summary( AREA_ALBERS_SQKM ),
            summary( Person)
            )


  read_csv( 'output/mesh_block_summary.csv') -> csv

map_sa1= read_sf( '/mnt/temp/tmp/MB_2016_VIC.shp')

map_sa1 %>%
    st_drop_geometry() %>%
    filter( str_detect( GCC_CODE16, '^2')) %>%
count( ) %>%


map_sa1 %>%
distinct( GCC_CODE16 ) %>%

write_df_distance_to_testing %>%

weighted_seifa = df_mesh_detail %>%
inner_join( df_mesh_sa1, by='MB_CODE16') %>%
drop_na( SA1_MAIN16 ) %>%
mutate( SA1_MAIN16 = as.numeric( SA1_MAIN16 )) %>%
inner_join( df_seifa, by='SA1_MAIN16' ) %>%
inner_join(  write_df_distance_to_testing, by='MB_CODE16') %>%
group_by( id) %>%
  summarise( pop = sum( Person ),
            mean_seifa = mean( score  ),
            weighted_seifa = sum( score * Person )/pop,
            .groups='drop')


df_seifa %>%
count( SA1_MAIN16, sort=TRUE)


write_df_distance_to_testing %>%
  inner_join( df_covid_remoteness, by='id') %>%
  inner_join( df_mesh_detail, by='MB_CODE16') %>%
  group_by( id, site_name, RA_CODE16 ) %>%
  summarise( pop = sum( Person ),
            tt = mean( duration  ),
            wtt = sum( duration * Person )/pop,
            .groups='drop')%>%
  ggplot( aes( pop, tt, label=site_name, color=RA_CODE16  ) ) +
  geom_point()


write_df_distance_to_testing %>%
  inner_join( df_covid_remoteness, by='id') %>%
  inner_join( df_mesh_detail, by='MB_CODE16') %>%
  group_by( id, site_name, RA_CODE16 ) %>%
  summarise( pop = sum( Person ),
            tt = mean( duration  ),
            wtt = sum( duration * Person )/pop,
            .groups='drop')%>%
  ggplot( aes( pop, tt, label=site_name, color=RA_CODE16  ) ) +
  geom_point()

df_mesh_detail


A4 half page

write_df_distance_to_testing %>%
  anti_join( df_mb2sa1 )

df_mb2sa1  %>%
  anti_join(write_df_distance_to_testing )


df_driving_time %>%
  arrange(duration)

  arrange(distance)

df_mesh_in_reach %>%
  count( id ) %>%

read_csv('/mnt/temp/summary_testing_site.csv') %>%
  summarise( across( where( is.numeric), median ))
read_csv('../hospital_pt_accessability/output/summary_testing_site.csv') %>%
  summarise( across( where( is.numeric), median ))


read_csv('/mnt/temp/summary_testing_site.csv') %>%
  summarise( across( where( is.numeric), mean, na.rm=TRUE ))
read_csv('../hospital_pt_accessability/output/summary_testing_site.csv') %>%
  summarise( across( where( is.numeric), mean, na.rm=TRUE ))



