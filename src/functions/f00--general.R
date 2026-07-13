#' General libraries and functions needed throughout the project.




# SETTINGS ---------------------------------------------------------------------


#' Always display full errors every time.
options('lifecycle_verbosity' = 'error')




# LIBRARIES --------------------------------------------------------------------


library(sf)
library(glue)
library(rlang)
suppressPackageStartupMessages(library(tidyverse))




# SOURCE FILES -----------------------------------------------------------------


source('functions/f01--constants.R')




## Geometry --------------------------------------------------------------------


CRS <- 4326
CRS.AREAL <- 26915 # For areal interpolation



# FUNCTIONS --------------------------------------------------------------------


#' Shortcuts.
as.char <- as.character
as.int <- as.integer


f.now <- function(s = TRUE){
  # Returns the current date and time (seconds optional) formatted for printing.
  if(s) format.Date(Sys.time(), '%Y%m%d-%H%M%S')
  else format.Date(Sys.time(), '%Y%m%d-%H%M')
}



f.write_qs2 <- function(obj, f_path, f_name, compression = 22, n_threads = 2){
  #' Write in `qs2` format.
  glue::glue('{file.path(f_path, f_name)}--saved-{f.now()}.qs2') %>% 
    qs2::qs_save(obj, ., compress_level = compression, nthreads = n_threads)
}



f.save_census_chicago <- function(l_census,
                                  v_years,
                                  level,
                                  geometry,
                                  survey,
                                  place     = NULL,
                                  prefix    = NULL,
                                  suffix    = NULL,
                                  n_threads = 10){
  message('Saving longitudinal Census dataset...')
  
  #' If specified, set a prefix for the file name.
  pfx <- ifelse(is.null(prefix), '', str_c(prefix, '--'))
  
  #' If specified, paste in the place (e.g., city, state & county, etc.).
  pl <- ifelse(is.null(place),  '', str_c(place, '--'))
  
  #' String to capture the year(s) present in the list.
  yr <- ifelse(length(v_years) > 1,
               str_c(v_years[1], '-', v_years[length(v_years)]),
               v_years)
  
  #' If `suffix` is specified, include it; otherwise, get the empty string.
  sfx <- ifelse(is.null(suffix), '', str_c('--', suffix))
  
  #' Save with maximum compression.
  glue('{pfx}Census-Variables--{pl}{survey}--Geometry-{geometry}',
       '--Level-{level}--Year-{yr}{sfx}') %>%
    f.write_qs2(l_census, f_path = 'data', f_name = ., n_threads = n_threads)
}



f.map_all <- function(l_geo, zcol){
  #' Maps of a given level across the years, colored by `zcol`.
  for(geo in l_geo){
  mapview::mapview(geo, zcol = zcol) %>% print()
  }
}



f.assign_class <- function(obj, v_class){
  #' Assign an object's class as specified and return (with updated class).
  class(obj) <- v_class; obj
}