#' Interpolate Census data to administrative and police geometries.




# SOURCE FILES -----------------------------------------------------------------


source('functions/f02a--interpolation--general.R')
source('functions/f03a--data--chicago.R')




# LIBRARIES --------------------------------------------------------------------


library(mapview)




# FUNCTIONS --------------------------------------------------------------------



f.trim_ohare <- function(l_geo_chi_pol, save = FALSE, n_threads = 10){
  #' Trim O'Hare airport from all geometries, both city and police.
  
  #' Turn off S2 processing for smoother buffers.
  sf::sf_use_s2(FALSE)
  
  message("Trimming O'Hare (airport) areas from administrative and police ",
          "geometries...")
  
  f_difference <- function(geo_base, geo_excl, var_id){
    #' Local function to subtract O'Hare area from local geometric layer. Split
    #' up multipolygons with `st_cast()`; group by the specified variable and
    #' keep the largest (i.e., drop the scraps). Drop `area`, then set the class
    #' to remove tibble (otherwise `print` fails in RMarkdown; this is a bug in
    #' `sf`, `dplyr`, or `RMarkdown`).
    geo_base %>% 
      st_difference(geo_excl) %>% 
      st_cast('POLYGON') %>% 
      mutate(area = st_area(geometry)) %>% 
      group_by(!!sym(var_id)) %>% 
      slice_max(area) %>% 
      ungroup() %>% 
      select(-area) %>% 
      f.assign_class(c('sf', 'data.frame')) %>%
      suppressMessages() %>% 
      suppressWarnings()
  }
  
  #' For results.
  l_res <- list()
  
  #' Area (O'Hare International Airport, ORD) to drop from each layer. Use a
  #' small buffer (0.5m; divide to approximate decimal degrees as we have turned
  #' off S2 processing) to clean up the edges; drop ID variable.
  geo_d16_ord <- 
    l_geo_chi_pol$police$beat %>%
    filter(id_district == 16, id_sector == 5) %>%
    group_by(id_district) %>%
    summarize(geometry = st_union(geometry)) %>% 
    st_buffer(0.5/111320) %>% 
    select(-id_district) %>% 
    suppressMessages() %>% 
    suppressWarnings()
  
  #' Nested, named list of base geometries to trim.
  l_geo_base <- f.names_geo_chi_admin_police(l_geo_chi_pol)
  
  #' Loop through all geometries and trim.
  for(top_level in names(l_geo_base)){
    message(top_level)
    #' Extract top-level list (administrative or police).
    geo_top_level <- l_geo_base[[top_level]]
    
    for(level in names(geo_top_level)){
      message('\t', level)
      #' Extract local objects.
      geo_local_level <- geo_top_level[[level]]
      
      #' Drop O'Hare areas from the map and assign results to list.
      l_res[[top_level]][[level]] <- 
        f_difference(geo_local_level[[1]],
                     geo_d16_ord,
                     names(geo_local_level[1]))
    }
  }
  
  if(save){
    glue('geo--chicago-admin-police--no-ORD') %>% 
      f.write_qs2(l_res, f_path = 'data', f_name = ., n_threads = n_threads)
  }
  
  l_res
}



f.names_geo_chi_admin_police <- function(l_geo_chi_pol){
  #' Nested, named list of base geometries to trim.
  list(
    admin  = 
      list(city      = list(id_chicago   = l_geo_chi_pol$admin$city),
           comm      = list(id_comm_area = l_geo_chi_pol$admin$comm)),
    police = 
      list(beat      = list(id_beat      = l_geo_chi_pol$police$beat %>% 
                                             filter(!(id_district == 16 &
                                                        id_sector == 5),
                                                    id_district != 31)),
           district  = list(id_district  = l_geo_chi_pol$police$district %>%
                                             filter(id_district != 31))))
}



f.interpolate_census_chi_pol_all <- function(l_geo_census_tract,
                                             l_geo_chi_pol,
                                             survey    = names(SURVEY),
                                             prefix    = NULL,
                                             suffix    = NULL,
                                             save      = FALSE,
                                             n_threads = 10){
  #' For every year of Census data in the list (tract geometry), interpolate
  #' tract data to all the Chicago administrative and police geometries.
  
  message('Interpolating Census tract data to Chicago administrative and ',
          'police geometries...')
  
  #' Named, nested list of all the geometries to be interpolated, by type, with
  #' necessary parameters.
  l_params <- 
    list(
      admin  =
        list(city      = list(geo_admin       = l_geo_chi_pol$admin$city,
                              id_admin_cutter = 'id_chicago',
                              level           = 'City'),
             comm      = list(geo_admin       = l_geo_chi_pol$admin$comm,
                              id_admin_cutter = 'id_comm_area',
                              level           = 'Community-Area')),
      police = 
        list(beat      = list(geo_admin       = l_geo_chi_pol$police$beat,
                              id_admin_cutter = 'id_beat',
                              level           = 'Police-Beat'),
             district  = list(geo_admin       = l_geo_chi_pol$police$district,
                              id_admin_cutter = 'id_district',
                              level           = 'Police-District')))
  
  #' Initialize for results.
  l_geo_interpolated <- list()
  
  #' Loop through each new geometry.
  for(top_level in names(l_params)){
    message(top_level)
    
    #' Extract parameters for top-level geometry types.
    l_params_top <- l_params[[top_level]]
    
    for(level in names(l_params_top)){
      message('\t', level)
      
      #' Extract parameters for specific geometry type.
      l_params_local <- l_params_top[[level]]
      
      #' Interpolate Census tract-level data to new geometric level; assign to
      #' results object.
      l_geo_interpolated[[str_to_title(top_level)]][[l_params_local$level]] <- 
        f.interpolate_census_chi_pol(
          l_geo_census_tract,
          l_geo_chi_pol[[top_level]][[level]],
          id_census_dough = 'GEOID',
          id_admin_cutter = l_params_local$id_admin_cutter,
          save            = FALSE,
          n_threads       = n_threads)
    }
  }
  
  if(save){
    f.save_census_chicago(
      l_census  = l_geo_interpolated,
      v_years   = names(l_geo_census_tract),
      level     = 'All-Administrative-Police',
      geometry  = TRUE,
      survey    = survey,
      place     = 'Chicago',
      prefix    = prefix,
      suffix    = suffix,
      n_threads = n_threads)
  }
  
  l_geo_interpolated
}



f.interpolate_census_chi_pol <- function(l_geo_census,
                                         geo_admin,
                                         id_census_dough,
                                         id_admin_cutter,
                                         level     = NULL,
                                         survey    = names(SURVEY),
                                         place     = 'Chicago',
                                         prefix    = NULL,
                                         suffix    = NULL,
                                         save      = FALSE,
                                         n_threads = 10){
  #' Interpolate longitudinal Census data to specified administrative level.
  
  #' Extract variable names to interpolate.
  v_variables <-
    l_geo_census[[1]] %>% 
    st_drop_geometry() %>% 
    select(-c(GEOID, Name)) %>%
                    # select(-any_of(c('GEOID', 'Name', id_admin_cutter))) %>% 
    names()
  
  #' Extract years.
  v_years <- names(l_geo_census)
  
  #' Extensive-variables interpolation for each year.
  l_geo_interpolated <- 
    parallel::mclapply(
      v_years,
      function(year){
        #' Interpolate extensive variables with `weight = 'total'`. Merge any
        #' additional admin data back in, then assign a name for each dataframe.
        f.interpolate(
          geo_dough  = l_geo_census[[year]],
          geo_cutter = geo_admin,
          v_vars_ext = v_variables,
          sid_dough  = id_census_dough,
          tid_cutter = id_admin_cutter,
          weight_ext = 'total',
          reshape    = FALSE)},
    mc.cores = n_threads) %>% 
    set_names(v_years)
  
  if(save){
    f.save_census_chicago(
      l_census  = l_geo_interpolated,
      v_years   = names(l_geo_census),
      level     = level,
      geometry  = TRUE,
      survey    = survey,
      place     = place,
      prefix    = prefix,
      suffix    = suffix,
      n_threads = n_threads)
  }
  
  l_geo_interpolated
}