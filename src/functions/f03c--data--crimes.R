#' Download and clean Chicago deprivation-of-property crimes, 2010-2025. Convert
#' to spatial points and save.




# SOURCE FILES -----------------------------------------------------------------


source('functions/f00--general.R')




# LIBRARIES --------------------------------------------------------------------


library(mapview)
library(tidyverse)




# CONSTANTS --------------------------------------------------------------------


#' Variables to select from the 'Crimes 2001 to present' dataset.
VARS.CRIME <- 
  str_c('id AS id_crime',
        'case_number',
        # 'date',
        'date_trunc_ymd(date) AS date',
        'primary_type AS type',
        'description',
        'arrest',
        'domestic',
        'district AS pol_district',
        'beat AS pol_beat',
        'community_area AS community',
        'ward',
        'longitude',
        'latitude',
        sep = ', ')


#' Chicago Data Portal's 'Crimes - 2001 to Present' dataset (mins last 7 days):
#' data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2
#' Dataset created 20110930 (per the site).
URL.CRIME <- 'https://data.cityofchicago.org/resource/crimes.csv'


#' Query for deprivation-of-property crimes from the 'Crimes - 2001 to Present'
#' dataset, date-limited to 2010-2024
#' '%25' is used to represent a literal '%', which itself represents a wildcard.
Q.CRIME <- 
  glue(
    "{URL.CRIME}?",
    "$select={VARS.CRIME}&",
    "$where=date BETWEEN '{DATE.MIN}' AND '{DATE.MAX}' AND ",
           "date IS NOT NULL AND ",
           "(primary_type in('BURGLARY', 'MOTOR VEHICLE THEFT', 'ROBBERY') OR ",
                 "(primary_type='DECEPTIVE PRACTICE' AND ",
                   "(description like '%25LESSEE%25' OR ",
                    "description like '%25MISLAID%25')) OR ",
                 "(primary_type='THEFT' AND ",
                    "description NOT like '%25FINANCIAL ID%25')) AND ",
               "latitude IS NOT NULL AND ",
               "longitude IS NOT NULL")




# FUNCTIONS --------------------------------------------------------------------


f.crime_raw <- function(download,
                        query       = NULL,
                        compression = 22,
                        n_threads   = 10){
  #' Read (remotely with `download = TRUE` and by passing a query to `query`)
  #' deprivation-of-property crimes; if downloading, also save the results.
  if(download){
    #' Download; convert `date` and logical variables to correct types. Save.
    (df_crime <- 
      RSocrata::read.socrata(query) %>% 
      as_tibble() %>% 
      mutate(date = as_date(date),
             across(c(arrest, domestic), ~ . == 'true'))) %>% 
      f.write_qs2('data',
                  'df--crime--dep-prop--raw',
                  compression = compression,
                  n_threads   = n_threads)
  }
  else{
    df_crime <- 
      dir('data', 'crimes--dep-prop--raw', full.names = TRUE) %>% 
      qs2::qs_read(nthreads = n_threads)
  }
  
  df_crime
}



f.clean_crime <- function(df_crime, geo_chi){
  #' Read and clean crime dataset.
  
  #' Bounding box of Chicago city geometry.
  l_chi_bounds <- st_bbox(geo.chi)
  
  #' Drop observations outside Chicago (data-entry errors in source data).
  #' Extract parts of date. Rename variables as needed. Recode `description`.
  df_crime %>% 
    filter(latitude  >= l_chi_bounds$ymin & latitude  <= l_chi_bounds$ymax,
           longitude >= l_chi_bounds$xmin & longitude <= l_chi_bounds$xmax) %>% 
    mutate(year    = year(date) %>% as.int(),
           quarter = quarter(date),
           month   = month(date) %>% as.int(),
           weekday = wday(date) %>% as.int()) %>%
    f.recode_description()
}



f.recode_description <- function(df_crime){
  #' Recode `description` variable in crime dataset.
  mutate(
    df_crime,
    description =
      case_when(
        type == 'BURGLARY' ~
          case_when(
            str_detect(description, 'MOTOR')   ~ 'MV',
            str_detect(description, 'ATTEMPT') ~ 'ATTEMPT',
            TRUE                               ~ 'BURGLARY'),
        type == 'DECEPTIVE PRACTICE' ~
          case_when(
            str_detect(description, 'LESSEE,( )?NON-')  ~ 'THEFT LESSEE',
            str_detect(description, 'LESSEE,( )?MOTOR') ~ 'THEFT LESSEE MV',
            str_detect(description, 'MISLAID')          ~ 'THEFT MISLAID',
            TRUE                                        ~ NA),
        type == 'MOTOR VEHICLE THEFT' ~
          case_when(
            str_detect(description, 'ATT.+AUTO')  ~ 'ATTEMPT AUTOMOBILE',
            str_detect(description, 'AUTOMOBILE') ~ 'AUTOMOBILE',
            str_detect(description, 'ATT.+TRUCK') ~ 'ATTEMPT TRUCK BUS MHOME',
            str_detect(description, 'TRUCK')      ~ 'TRUCK BUS MHOME',
            str_detect(description, 'ATT.+CYCLE, SCOOTER, BIKE')
            ~ 'ATTEMPT CYCLE SCOOTER BIKE',
            str_detect(description, 'CYCLE, SCOOTER, BIKE')
            ~ 'CYCLE SCOOTER BIKE',
            TRUE                                  ~ NA),
        type == 'ROBBERY' ~
          case_when(
            str_detect(description, 'ATTEMPT.+AGGRAVATED')  ~ 'AGG ATTEMPT',
            str_detect(description, 'AGGRAVATED VEHICULAR') ~ 'AGG HIJACK',
            str_detect(description, 'AGGRAVATED')           ~ 'AGG',
            str_detect(description, 'ATTEMPT.+ARMED')       ~ 'ARMED ATTEMPT',
            str_detect(description, 'ATTEMPT.+NO WEAPON')   ~ 'UNARMED ATTEMPT',
            str_detect(description, 'NO WEAPON')            ~ 'UNARMED',
            str_detect(description, 'ARMED')                ~ 'ARMED',
            str_detect(description, '^VEHICULAR')           ~ 'UNARMED HIJACK',
            TRUE                                            ~ NA),
        type == 'THEFT' ~
          case_when(
            str_detect(description, 'ATTEMPT')       ~ 'ATTEMPT',
            str_detect(description, 'MOTOR VEHICLE') ~ 'MOTOR VEHICLE',
            str_detect(description, 'POCKET|PURSE')  ~ 'POCKET PURSE',
            str_detect(description, 'RETAIL')        ~ 'RETAIL',
            TRUE                                     ~ 'THEFT'),
        TRUE ~ NA))
}



f.geo_crime <- function(df_crime){
  #' Crime events geometry, dropping observations without coordinates. Set class
  #' to overcome `print` bug for geometric tibble objects.
  df_crime %>% 
    sf::st_as_sf(coords = c('longitude', 'latitude')) %>% 
    sf::st_set_crs(CRS) %>% 
    f.assign_class(c('sf', 'data.frame'))
}