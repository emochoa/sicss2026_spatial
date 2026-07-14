#' General interpolation functions.


# LIBRARIES --------------------------------------------------------------------


library(rlang)




# SOURCE FILES -----------------------------------------------------------------


source('functions/f00--general.R')
source('functions/f02b--interpolation--areal-library-changes.R')




# FUNCTIONS --------------------------------------------------------------------


f.interpolate <- function(geo_dough,
                          geo_cutter,
                          v_vars_ext = NULL,
                          v_vars_int = NULL,
                          sid_dough  = 'GEOID',
                          tid_cutter = 'ID',
                          weight_ext = NULL,
                          reshape    = TRUE){
  #' Areal interpolation of Census demographics.
  #' The 'dough' is the layer that will be trimmed into distinct shapes; the
  #' 'cutter' is the die that defines the shapes of the new areas.
  #' ***************************************************************************
  #' **NOTE:** When computing an areal interpolation with the goal of reshaping
  #' or as a cookie-cutter, the argument to `weight` **MUST ALWAYS** be `total`.
  
  #' Ensure that `weight` is assigned consistently with variables passed.
  if(!is.null(v_vars_ext) &
     (is.null(weight_ext) || !(weight_ext %in% c('sum', 'total')))){
    stop("Use either `weight_ext = 'sum'` or `weight_ext = 'total'`")
  }
  else if(!is.null(v_vars_ext)) weight <- weight_ext
  else if( is.null(v_vars_ext)) weight <- 'sum'
  
  #' Reset CRS for interpolation only.
  geo_cutter_crs <- st_transform(geo_cutter, CRS.AREAL)
  geo_dough_crs  <- st_transform(geo_dough, CRS.AREAL)
  
  #' Validate interpolation before attempting; will halt if validation fails.
  f.validate(geo_dough_crs,
             geo_cutter_crs,
             c(v_vars_ext, v_vars_int),
             sid_dough,
             tid_cutter)
  
  #' Compute interpolation. Round interpolated values to integers and reset the
  #' CRS. If reshaping, rename the `tid` back to the original `sid`.
  f.areal.aw_interpolate(
    geo_dough,
    tid_cutter,
    geo_cutter,
    sid_dough,
    weight    = weight,
    output    = 'sf',
    extensive = v_vars_ext,
    intensive = v_vars_int) %>% 
    mutate(across(all_of(v_vars_ext), ~ round(.) %>% as.int())) %>% 
    st_transform(CRS) %>% 
    {function(geo_interpolated){
      if(reshape){
        geo_interpolated %>% rename(!!sid_dough := !!sym(tid_cutter))
      }
      else geo_interpolated
    }}() %>% 
    relocate(geometry, .after = everything())
}



f.validate <- function(geo_dough,
                       geo_cutter,
                       v_variables,
                       sid_dough,
                       tid_cutter){
  # If the interpolation validation step fails, halt execution.
  
  # Formal validation test.
  r <- areal::ar_validate(source  = geo_dough,
                          target  = geo_cutter,
                          varList = v_variables,
                          method  = 'aw',
                          verbose = TRUE)
  
  # Check for errors; if so, print test results and halt; otherwise, continue.
  if(FALSE %in% r$result) stop(print(r))
  else message('  Validation successful')
}