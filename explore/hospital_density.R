library(tidyverse)
library(readxl)
library(sf)
library(RPostgreSQL)
source('keys.R')


haven::read_sav('data/SPSSDatasetRevisedFull.sav') %>%
  janitor::clean_names() %>%
  #  mutate(across(4:8, as.ordered)) %>%
  select(person_id, hospitalid, long, lat) %>%
  { . } -> df

df %>% 
  distinct( hospitalid, lat, long) %>% 
  { . } -> df_hospital

#source('_drake.R')

drv <- dbDriver('PostgreSQL');
con <- dbConnect(drv,host='alf',port=5432,dbname='postgis_db',
                 user='dewoller',pass=db_password);

df_hospital  %>%
  janitor::clean_names() %>%
  st_as_sf(coords=c(2,3), crs=4283) %>%
  rename( id = hospitalid) %>%
  dbWriteTable(con, 'hospital_addresses', ., row.names=FALSE, overwrite=TRUE)

query = 'create index on hospital_addresses using gist( geometry )'
dbGetQuery(con,query) 
query = 'create index on hospital_addresses using gist( st_transform( geometry, 3577) )'
dbGetQuery(con,query) 


get_data(5000, 'hospital_addresses') %>% 
{ . } -> t

t %>%
  group_by(id) %>%
  summarise( size = sum(intersection_area), 
            population_covered = sum(population_covered), 
            population_intersected = sum(population_intersected), 
            dwelling_covered = sum(dwelling_covered), 
            dwelling_intersected = sum(dwelling_intersected), 
            density_covered =  population_covered / (size / 1000000), 
            density_intersected =  population_intersected / (size / 1000000), 
  .groups='drop')  %>%
  rename( hospitalid=id) %>%
inner_join( df, by='hospitalid') %>%
write_csv('data/hospital_density.csv')

get_data = function(distance, points_table_name) {


  query= glue::glue("
                    with points as (
                                    select id, st_buffer( st_transform( geometry, 3577), {distance})  as buffer, sc.geometry as point
                                    from {points_table_name} sc
                    )
                    select id,
                    to_mb.mb_code16,
                    mb_category_name_2016,
                    s2.area as intersection_area,
                    s3.proportion as proportion_covered, 
                    to_md.person * s3.proportion as population_covered, 
                    to_md.person  as population_intersected, 
                    to_md.dwelling * s3.proportion as dwelling_covered,
                    to_md.dwelling as dwelling_intersected
                    from points point     -- geography of the source point
                    , meshblock to_mb 				  -- geography of the target meshblock
                    , meshblock_detail to_md
                    , lateral( select st_intersection( point.buffer, st_transform( to_mb.geom, 3577))) as s1(intersection)
                    , lateral (values(st_area( s1.intersection))) as s2(area)
                    , lateral (values(round(cast(s2.area / (to_md.area_albers_sqkm*1000000) as numeric), 2))) as s3(proportion)
                    where to_mb.mb_code16 = to_md.mb_code16 
                    and st_dwithin( st_transform( point.point, 3577)::geometry, st_transform( to_mb.geom, 3577)::geometry, {distance});
                    "
  )
  print(query) 
  dbGetQuery(con,query) %>%
    as_tibble() 
}


test %>%
    group_by( id, mb_category_name_2016) %>%
    summarise( area = sum(intersection_area), .groups='drop') %>% 
    group_by( id) %>%
    mutate( proportion = area / sum(area) ) %>%
    { . } -> df_stats


  df_stats %>%
    select(-proportion) %>%
    pivot_wider( names_from=mb_category_name_2016, values_from=area, values_fill=0 ) %>%
    janitor::clean_names() %>% 
    write_csv(glue::glue('data/area_{distance}'))

  df_stats %>%
    select(-area) %>%
    pivot_wider( names_from=mb_category_name_2016, values_from=proportion, values_fill=0 ) %>%
    janitor::clean_names() %>% 
    write_csv(glue::glue('data/proportion_{distance}'))







