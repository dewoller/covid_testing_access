summarise_intermesh_distances <- function( df_mesh_lockdown_distance,
                                          df_mesh_lockdown,
                                          df_mesh_detail,
                                          df_mesh_sa1,
                                          df_mesh_sa2) {

  df_mesh_lockdown_distance %>%
    select(areas) %>%
    unnest(areas) %>%
    right_join( df_mesh_lockdown, by='MB_CODE16') %>%
    select(-starts_with('mc')) %>%
    replace_na( list( area=0) ) %>%
    inner_join( df_mesh_detail, by='MB_CODE16') %>%
    inner_join( df_mesh_sa1, by='MB_CODE16') %>%
    select(-starts_with('mc')) %>%
    inner_join( df_mesh_sa2, by='MB_CODE16') %>%
    select(-starts_with('mc')) 

}
