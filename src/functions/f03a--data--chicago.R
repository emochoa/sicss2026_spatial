#' Download and clean Chicago administrative boundaries geometry.




# SOURCE FILES -----------------------------------------------------------------


source('functions/f00--general.R')




# FUNCTIONS --------------------------------------------------------------------


f.read_geo_chi <- function(level){
  #' Select administrative regions, Chicago Data Portal: data.cityofchicago.org
  #' chicityclerk.com/legislation-records/journals-and-reports
  
  if(level == 'city'){
    #' Chicago city borders. data.cityofchicago.org/d/ewy2-6yfk
    sf::st_read('https://data.cityofchicago.org/resource/qqq8-j68g.geojson',
                quiet = TRUE) %>% 
      transmute(id_chicago = 'city_chicago')
  }
  else if(level == 'comm'){
    #' Chicago community areas. data.cityofchicago.org/d/cauq-8yn6
    sf::st_read('https://data.cityofchicago.org/resource/igwz-8jzy.geojson',
                quiet = TRUE) %>%
      transmute(community,
                id_comm_area = as.integer(area_num_1))
  }
  else stop('Use one of: `city`, `comm`')
}



f.read_geo_police <- function(level){
  #' Chicago Police Department geometry, current as of 20260630. Note: District
  #' 31 is technically not within city limits and District 16 is largely O'Hare.
  #' data.cityofchicago.org
  
  if(level == 'beat'){
    #' Current police beats: data.cityofchicago.org/d/aerh-rz74
    #' Effective 20121219
    sf::st_read('https://data.cityofchicago.org/resource/n9it-hstw.geojson',
                quiet = TRUE) %>% 
      select(-beat) %>% 
      rename(id_beat     = beat_num,
             id_district = district,
             id_sector   = sector) %>% 
      mutate(across(-geometry, as.integer))
  }
  else if(level == 'district'){
    #' Current police districts: data.cityofchicago.org/d/fthy-xz3r
    #' Effective 20121219
    sf::st_read('https://data.cityofchicago.org/resource/24zt-jpfn.geojson',
                quiet = TRUE) %>% 
      transmute(id_district = as.integer(dist_num))
  }
  else stop('Use one of: `beat`, `district`')
}



f.read_geo_chi_admin_pol <- function(){
  #' All Chicago administrative and police geometries in a single named list.
  
  message('Downloading Chicago administrative and police geometries...')
  
  #' Read administrative geometries.
  l_geo_admin <- 
    sapply(c('city', 'comm'),
           f.read_geo_chi,
           simplify  = FALSE,
           USE.NAMES = TRUE)
  
  #' Read police geometries.
  l_geo_pol <- 
    sapply(c('beat', 'district'),
           f.read_geo_police,
           simplify  = FALSE,
           USE.NAMES = TRUE)
  
  list(admin = l_geo_admin, police = l_geo_pol)
}



f.read_chi_local_remote <- function(download){
  #' Either download and save Chicago boundaries geometry, or read locally.
  if(download){
    #' Download Chicago city borders from Chicago Data Portal.
    (geo_chi <- 
      f.read_geo_chi('city')) %>% 
      f.write_qs2('data', 'geo--chicago-city')
  }
  else{
    #' Read the data locally, assuming the pre-specified file name was used, and
    #' assuming there is only one such file in the directory.
    geo_chi <- 
      dir('data', 'geo--chicago-city', full.names = TRUE) %>% 
      qs2::qs_read()
  }
  
  geo_chi
}



f.read_geo_chi_admin_pol_local_remote <- function(download){
  #' Either download and save all Chicago boundaries geometry, or read locally.
  if(download){
    #' Download all Chicago administrative and police boundaries.
    (l_geo_admin_pol <- 
      f.read_geo_chi_admin_pol()) %>% 
      f.write_qs2('data', 'geo--chicago-admin-police')
  }
  else{
    #' Read the data locally, assuming the pre-specified file name was used, and
    #' assuming there is only one such file in the directory.
    l_geo_admin_pol <- 
      dir('data', 'geo--chicago-admin-police--saved-', full.names = TRUE) %>% 
      qs2::qs_read()
  }
  
  l_geo_admin_pol
}