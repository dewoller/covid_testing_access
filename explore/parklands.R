source('_drake.R')

loadd(df_mesh_detail)

loadd(df_mesh_lga)


map_lga %>%
  inner_join( df_lga_lockdown by=c('municipality'='lga_name'))

df_mesh_lga %>%
  filter( lga_name != '') %>%
  inner_join( df_mesh_detail, by='MB_CODE16')

clean( df_mesh_lockdown_summary)

df_mesh_lockdown_summary  %>% 
  arrange( area)

loadd(df_mesh_lockdown)


df_mesh_lockdown %>%
  anti_join(df_mesh_lockdown_summary, by='MB_CODE16' ) %>%
  inner_join( df_mesh_detail, by='MB_CODE16' )  %>%
  count( MB_CATEGORY_NAME_2016, sort=TRUE)

