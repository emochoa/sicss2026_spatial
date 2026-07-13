#' Aggregate crimes by areal unit.


#' Turn off S2 processing for negative buffer.
sf::sf_use_s2(FALSE)




# SOURCE FILES -----------------------------------------------------------------

source('functions/f00--general.R')




# LIBRARIES --------------------------------------------------------------------


library(mapview)



# FUNCTIONS --------------------------------------------------------------------


f.filter_to_city <- function(geo_crime, geo_tracts){
  #' Drop observations at O'Hare, Midway, and in District 31 (not in Chicago).
  
  #' Identify events at MDW.
  geo_mdw <- 
    geo_crime %>% 
    select(id_crime, pol_beat) %>% 
    filter(pol_beat == 813) %>% 
    st_join(geo_tracts %>% 
              select(GEOID) %>% 
              filter(GEOID == GEOID.MDW) %>% 
              st_buffer(-1/111320),
            join = st_covered_by) %>% 
    drop_na() %>% 
    collect()
  
  #' Drop unwanted events.
  geo_crime %>% 
    filter(!(pol_district %in% 16 & str_detect(pol_beat, '165[0-9]')),
           !(id_crime %in% geo_mdw$id_crime),
           pol_district != 31)
}



#' f.geo_chi_no_census <- function(l_geo_chi_admin_pol){
#'   #' For a list of administrative and police geometries (polygons) with Census
#'   #' data (multi-year), keep only the geometry.
#'   
#'   #' For results.
#'   l_geo <- list()
#'   
#'   #' Loop through list, keeping only the geometry and identifiers.
#'   for(nm_main in names(l_geo_chi_admin_pol)){
#'     geo_main <- l_geo_chi_admin_pol[[nm_main]]
#'     
#'     for(nm in names(geo_main)){
#'       l_geo[[nm_main]][[nm]] <- 
#'         l_geo_chi_admin_pol[[nm_main]][[nm]][[1]] %>% 
#'         select(any_of(matches('id_|community')))
#'     }
#'   }
#'   
#'   l_geo
#' }



f.drop_mdw_events <- function(geo_crime, geo_tracts_census){
  #' Drop MDW events according to tract geometry (with negative buffer applied),
  #' not beat ID (813).
  geo_crime %>% 
    st_join(geo_tracts_census %>% 
              filter(GEOID == GEOID.MDW) %>% 
              select(GEOID, Name) %>% 
              st_buffer(-1/111320),
            st_covered_by,
            duplicate_edges = FALSE,
            left            = FALSE) %>% 
    st_drop_geometry() %>% 
    select(id_crime) %>% 
    anti_join(geo_crime, ., by = 'id_crime')
}












f.agg_crime_areal_id <- function(geo_crime_ident, geo_admin, v_vars_group_join){
  #' Aggregate crime counts by areal unit. `v_vars_group_join` accepts a vector
  #' of multiple variable names for multiple-variable primary keys.
  
  message('Aggregating crimes by areal unit...')
  
  #' Group identified crimes by specified variable(s). Count events per areal
  #' unit, then merge geometry back in. Fill in zeros for units with no crimes.
  geo_crime_ident %>% 
    st_drop_geometry() %>% 
    group_by(!!!syms(v_vars_group_join)) %>% 
    count(name = 'n_crimes') %>% 
    ungroup() %>% 
    full_join(geo_admin, ., by = v_vars_group_join) %>% 
    mutate(n_crimes = replace_na(n_crimes, 0))
}