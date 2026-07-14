#' Download Census data.


# LIBRARIES --------------------------------------------------------------------


library(mapview)




# SETTINGS ---------------------------------------------------------------------



#' Turn off S2 processing for negative buffer: stackoverflow.com/a/79437935
#' Also increases processing speed for geometric operations. Because we are
#' focusing on a small area of the globe, this is acceptable.
sf::sf_use_s2(FALSE)



# SOURCE FILES -----------------------------------------------------------------


source('functions/f00--general.R')
source('functions/f02a--interpolation--general.R')
source('functions/f03a--data--chicago.R')




# FUNCTIONS --------------------------------------------------------------------


f.acs5_longitudinal <- function(v_years,
                                l_vars    = f.census_variables_main(),
                                survey    = SURVEY,
                                level     = LEVEL,
                                state     = STATE,
                                county    = COUNTY,
                                geo       = TRUE,
                                prefix    = NULL,
                                suffix    = NULL,
                                n_threads = 10,
                                save      = FALSE){
  #' List of dataframes for given years: ACS-5 estimates for all variables.
  #' I've elected not to parallelize this function so as to avoid potentially
  #' being flagged for accessing the API too quickly.
  
  message('Downloading all Census variables...')
  
  #' String for the main API call.
  call_base <- f.census_api_base_call()
  
  #' Initialize list to hold yearly results.
  l_census <- list()
  
  #' Loop through years.
  for(yr in v_years){
    message('  ', yr)
    
    #' Paste in API key and details, make the request, and extract content.
    l_content <- glue(call_base) %>% httr::GET() %>% httr::content()
    
    #' Convert API results to dataframe and add to running list of yearly data:
    #'   Unlist with `recursive = FALSE` to keep `NULL` values.
    #'   Convert `NULL` to `NA`; convert to vector: stackoverflow.com/a/77459006
    #'   Convert vector to rectangular: stackoverflow.com/a/4227273
    #'   --Use `+ 4` for 4 fixed columns (`NAME`, `state`, `county`, & `tract`).
    #'   Set column names to values of first row. Drop first row. Build `GEOID`
    #'   and rename `NAME` column. Drop unneeded columns. Rename select columns
    #'   to readable names. Merge in race and compute composite variables.
    #'   Reorder variables.
    l_census <-
      list_assign(l_census,
                  !!sym(as.char(yr)) :=
                    l_content %>%
                    unlist(recursive = FALSE) %>%
                    map_vec(~ ifelse(is.null(.), NA_character_, .)) %>%
                    matrix(ncol  = length(l_vars) + 4,
                           byrow = TRUE) %>%
                    as_tibble(.name_repair = 'universal_quiet') %>%
                    set_names(slice(., 1)) %>%
                    slice(-1) %>%
                    mutate(GEOID = str_c(state, county, tract),
                           Name  = NAME) %>%
                    select(-c(NAME, state, county, tract)) %>%
                    set_names(c(names(l_vars),
                                names(.) %>%
                                  keep(!str_detect(., '_')))) %>%
                    mutate(across(names(l_vars), as.integer)) %>%
                    f.census_composite_vars() %>%
                    select(GEOID, Name, everything()))
    
    #' If so specified, download and add geometry.
    if(geo){
      #' Read geometry for that year; drop unneeded variables, if present.
      geo_yr <- 
        f.read_geo_tracts(yr, state, county) %>% 
        select(-any_of(c('State', 'County', 'Name')))
      
      #' Add geometry to list element. Use full join to catch any mismatch
      #' between variable data and geometry. Drop unneeded variables.
      l_census[[as.char(yr)]] <- 
        full_join(geo_yr, l_census[[as.char(yr)]], by = 'GEOID')
    }
  }
  
  if(save){
    f.save_census_chicago(l_census, v_years, level, geometry = geo,
                          names(SURVEY), glue('State-{state}--County-{county}'),
                          prefix, suffix, n_threads)
  }
  
  l_census
}



f.census_variables_main <- function(){
  #' Named list of all Census variables to be downloaded from the Census API.
  list(pop.t        = 'B01003_001E', # Total residential population
       pop_1_plus.t = 'B07001_001E', # Total residents, ages ≥ 1
       pop_25_64.t  = 'B23006_001E', # Total residents, ages 25-64
       famhh.t      = 'B11005_003E', # Total family households w/ children < 18
       hh.t         = 'B19057_001E', # Total households
       pop_black    = 'B02009_001E', # Black residential population
       pop_latinx   = 'B03003_003E', # Latinx residential population
       males_15_17y = 'B01001_006E', # Male residents, ages 15-17
       males_18_19y = 'B01001_007E', # Male residents, ages 18-19
       males_20y    = 'B01001_008E', # Male residents, age 20
       males_21y    = 'B01001_009E', # Male residents, age 21
       males_22_24y = 'B01001_010E', # Male residents, ages 22-24
       males_25_29y = 'B01001_011E', # Male residents, ages 25-29
       same_home_1y = 'B07001_017E', # Residents living in same home 1 yr ago
       no_hsd_25_64 = 'B23006_002E', # Residents (25-64) w/o high school diploma
       fem_hd_famhh = 'B11005_007E', # Female-headed family households with kids
       pai_hh       = 'B19057_002E') # Households with public assistance income
}



f.census_api_base_call <- function(){
  #' Base string for Census API call. The level, state, county, and key will be
  #' spliced in with `glue` before calling the API.
  str_c('https://api.census.gov/data/{yr}/acs/{survey}?get=NAME,',
        '{str_c(l_vars, collapse = ",")}&for={level}:*&in=state:{state}%20',
        'county:{str_c(county, collapse = ",")}&',
        'key={jsonlite::read_json(PTH.CENSUS_API_KEY)$key}')
}



f.census_composite_vars <- function(df){
  #' Create composite variables.
  df %>% f.recent_movers() %>% f.young_males()
}



f.recent_movers <- function(df){
  #' Subtract the non-movers from total population ≥ 1y to get recent movers.
  df %>% 
    mutate(recent_movers = pop_1_plus.t - same_home_1y) %>% 
    select(-same_home_1y)
}



f.young_males <- function(df){
  #' Sum 'young males' variables per areal unit and assign to a single variable.
  
  #' Regular expression to match 'young males' variables.
  rgx_young_males <- '^males[0-9_]+y$'
  
  #' Sum 'young males' variables for total count per areal unit; drop variables.
  df %>% 
    group_by(across(-matches(rgx_young_males))) %>% 
    mutate(young_males = sum(across(matches(rgx_young_males)))) %>%
    ungroup() %>% 
    select(-matches(rgx_young_males))
}



f.read_geo_tracts <- function(year, state = STATE, county = COUNTY){
  #' Census tract geometry via `tigris`. Rename and drop variables, set CRS.
  message(glue('Downloading TIGRIS geometry for {year}: {state}, {county}'))
  (tigris::tracts(state  = state,
                  county = county,
                  year   = year) %>% 
    select(-any_of('GEOIDFQ')) %>%
    select(GEOID  = starts_with('GEOID'),
           State  = STATEFP,
           County = COUNTYFP) %>% 
    st_set_crs(CRS) %>%
    st_transform(CRS)) %>%
    suppressWarnings()
}



f.geo_census_chi <- function(l_geo_census,
                             geo_chi,
                             id_chi         = 'id_chicago',
                             level          = LEVEL,
                             survey         = SURVEY,
                             place          = 'Chicago',
                             reshape_mdw_yr = YR.MDW_TEMPLATE,
                             prefix         = NULL,
                             suffix         = NULL,
                             save           = FALSE,
                             n_threads      = 10){
  #' For every geometric Census dataframe in the list, get only Chicago tracts.
  
  message('Getting Chicago Census tracts...')
  
  #' Extract years.
  v_years <- names(l_geo_census)
  
  #' For every year of Census data, keep only Chicago tracts.
  l_geo_census_chi <- 
    parallel::mclapply(
      v_years,
      function(year){
        f.geo_census_chi_yr(l_geo_census[[year]],
                            geo_chi,
                            l_geo_census[[reshape_mdw_yr]],
                            year,
                            id_chi)},
      mc.cores = n_threads) %>% 
      set_names(v_years)
  
  if(save){
    f.save_census_chicago(l_geo_census_chi, v_years, level, geometry = TRUE,
                          names(survey), place, prefix, suffix, n_threads)
  }
  
  l_geo_census_chi
}



f.geo_census_chi_yr <- function(geo_census,
                                geo_chi,
                                geo_mdw_template,
                                year,
                                id_chi = NULL){
  #' All Chicago Census tracts aligning with the City boundaries.
  
  #' Get most tracts and add shoreline tracts; drop three O'Hare tracts and
  #' duplicates (for duplicate tract IDs, keep the one with the largest area).
  #' Trim shoreline tracts. Fix Midway tracts and geometry collections.
  bind_rows(f.chi_census_most(geo_census, geo_chi, id_chi),
            f.chi_census_shoreline_plus(geo_census, geo_chi, id_chi)) %>% 
    filter(!(GEOID %in% V.TRACTS_ORD)) %>% 
    # mutate(area = st_area(geometry)) %>% 
    # group_by(GEOID) %>% 
    # slice_max(area, n = 1, with_ties = FALSE) %>% 
    # ungroup() %>% 
    # select(-area) %>% 
    distinct() %>% 
    f.trim_shoreline(geo_chi, id_chi) %>% 
    f.replace_geo_3301(year) %>%
    f.reshape_mdw(geo_mdw_template, year) %>%
    mutate(geometry = st_collection_extract(geometry)) %>% 
    st_as_sf() %>% 
    remove_rownames() %>% 
    suppressWarnings() %>% 
    suppressMessages()
}



f.chi_census_most <- function(geo_census, geo_chi, id_chi = NULL){
  #' Most of the tracts to keep, excluding shoreline tracts.
  
  #' Add a 200m buffer (divide by `111320` for approximate conversion to decimal
  #' degrees as we have set `sf::sf_use_s2() = FALSE`) around Chicago boundaries
  #' then get all Census tracts completely covered by the city boundaries
  #' (excludes most shoreline tracts).
  st_join(geo_census,
          st_buffer(geo_chi, 200/111320),
          left = FALSE,
          join = st_covered_by) %>% 
    select(-any_of(id_chi))
}



f.chi_census_shoreline_plus <- function(geo_census, geo_chi, id_chi = NULL){
  #' Get Chicago tracts along the shoreline and most other tracts.
  
  #' Apply a negative 425m buffer (divide by `111320` for approximate conversion
  #' to decimal degrees as we have set `sf::sf_use_s2() = FALSE`) to Chicago
  #' boundaries; get intersecting tracts.
  #' Negative buffer instructions: stackoverflow.com/a/79437935
  st_join(geo_census,
           st_buffer(geo_chi, -425/111320),
           left = FALSE,
           join = st_intersects) %>% 
    select(-any_of(id_chi))
}



f.trim_shoreline <- function(geo_census, geo_chi, id_chi = NULL){
  #' Trims the shoreline (and O'Hare area) of Census areal units to match the
  #' official Chicago Community Areas. Suppress messages and warnings.
  
  #' Trim the Census tracts to match Chicago boundaries.
  geo_shore <- st_intersection(geo_chi, geo_census)
  
  #' Trim little slivers from two tracts near Navy Pier: Filter to only the
  #' tracts in question. Dissolve each multipolygon into distinct polygons, then
  #' keep the one with the largest area for each `GEOID`. Add the remainder of
  #' the tracts back in. Drop the Chicago ID variable, if present. Reset class
  #' of object to remove `tibble` classes (`tbl_df` and `tbl`), as these break
  #' then printing in RMarkdown.
  geo_shore %>% 
    filter(GEOID %in% V.TRACTS_NVY) %>% 
    st_cast('POLYGON') %>% 
    mutate(area = st_area(geometry)) %>% 
    group_by(GEOID) %>% 
    slice_max(area, n = 1) %>% 
    ungroup() %>% 
    select(-area) %>% 
    bind_rows(geo_shore %>% 
                filter(!(GEOID %in% V.TRACTS_NVY))) %>% 
    select(-any_of(id_chi)) %>% 
    f.assign_class(c('sf', 'data.frame'))
}



f.replace_geo_3301 <- function(geo, year){
  #' Replace the '3301' Census tract(s) geometry with manually edited borders.
  #' The problem is that in earlier years (through 2019), '3301' was a single
  #' tract; in 2020, it was split into three. The eastern-most tract largely
  #' covers non-residential areas (Museum Campus, LSD); when standardizing the
  #' geometry across years, this would skew the interpolation and result in too-
  #' great a share of each variable being assigned to that tract in the earlier
  #' years. Trimming the tracts for both time periods should yield more accurate
  #' interpolation results.
  
  #' Read in the file appropriate for the current year.
  if(as.int(year) <= 2019) geo_3301 <- st_read(FNM.GEO_3301_10_19, quiet = TRUE)
  else geo_3301 <- st_read(FNM.GEO_3301_20_24, quiet = TRUE)
  
  #' Drop '3301' tract row(s) and merge in the updated geometry with values.
  geo %>% 
    filter(!str_detect(Name, '3301')) %>% 
    bind_rows(geo_3301 %>% 
                left_join(geo %>% 
                            st_drop_geometry(),
                          by = 'GEOID'))
}



f.reshape_mdw <- function(geo_current, geo_template, year_current){
  #' Reshape the tract for Midway Airport and an adjacent tract in `geo_current`
  #' to match that in `geo_template`. This is needed because there was a slight
  #' change in the shape of these tracts in 2012: When reshaping an older map by
  #' interpolating from a newer geometry, the Midway tract erroneously gains
  #' residents; it should have `0` for all Census variables. Reshaping the old
  #' polygons for these tracts to match the new geometry prevents this problem.
  
  #' If current year is the year of template geometry, return current dataset.
  if(year_current == YR.MDW_TEMPLATE) return(geo_current)
  
  #' Extract template geometry (drop other variables).
  geo_tracts_template <- 
    geo_template %>% 
    select(GEOID, geometry) %>% 
    filter(GEOID %in% V.TRACTS_MDW)
  
  #' Update MDW geometry from current to template (keep current variables).
  geo_current %>% 
    filter(GEOID %in% V.TRACTS_MDW) %>% 
    st_drop_geometry() %>% 
    right_join(geo_tracts_template, by = 'GEOID') %>% 
    bind_rows(geo_current %>% 
                filter(!(GEOID %in% V.TRACTS_MDW)))
}



f.polygon_reshape <- 
  function(
    l_geo,
    year_templ,
    geo_3301.01_orig,
    geo_chi,
    id_chi    = NULL,
    level     = LEVEL,
    survey    = SURVEY,
    place     = 'Chicago',
    prefix    = NULL,
    suffix    = glue('Reshaped-to-{YR.RESHAPE_TEMPLATE}--Interpolated'),
    save      = FALSE,
    n_threads = 10){
  #' Reshape the polygons in every dataframe in `l_geo` to match those in
  #' `geo_template` while interpolating all the Census values to align with
  #' their new polygons.
  
  message('Reshaping polygons and interpolating values for each year...')
  
  #' Variable names to interpolate.
  v_variables <- 
    l_geo[[1]] %>% 
    st_drop_geometry() %>% 
    select(-c(GEOID, Name)) %>% 
    names()
  
  #' Extract years.
  v_years <- names(l_geo)
  
  #' Get template geometry.
  geo_template <- l_geo[[year_templ]] %>% transmute(GEOID, Name)
  
  #' For each year, reshape variables in the older datasets to match those of
  #' the newest dataset. Merge the tract `Name` variable back in. Restore the
  #' newest '3301.01' tract geometry (i.e., from the newest Census year, minus
  #' manual edits, plus shoreline trimming).
  l_geo_rshp <- 
    parallel::mclapply(
      v_years,
      mc.silent = FALSE,
      function(year){
        message(year)
        
        f.interpolate(geo_dough  = l_geo[[year]],
                      geo_cutter = geo_template %>% 
                                     transmute(ID = GEOID),
                      v_vars_ext = v_variables,
                      sid_dough  = 'GEOID',
                      tid_cutter = 'ID',
                      weight_ext = 'total',
                      reshape    = TRUE) %>% 
        left_join(geo_template %>%
                    st_drop_geometry(),
                  by = 'GEOID') %>%
          f.restore_geo_3301.01(geo_3301.01_orig, geo_chi, id_chi)},
      mc.cores = n_threads) %>% 
      set_names(v_years)
  
  if(save){
    f.save_census_chicago(l_geo_rshp, v_years, level, geometry = TRUE,
                          names(survey), place, prefix, suffix, n_threads)
  }
  
  l_geo_rshp
}



f.restore_geo_3301.01 <- function(geo_census_chi,
                                  geo_census_orig,
                                  geo_chi,
                                  id_chi = NULL){
  #' Restore the manually-trimmed '3301' tracts to the later-year geometry with
  #' trimmed shoreline.
  geo_census_orig %>% 
    filter(str_detect(Name, '3301.01')) %>% 
    f.trim_shoreline(geo_chi, id_chi) %>% 
    bind_rows(geo_census_chi %>% 
                filter(!str_detect(Name, '3301.01')))
}