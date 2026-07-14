#' Constants needed throughout project.



# CONSTANTS --------------------------------------------------------------------


#' Date limits for crime download.
DATE.MIN_1 <- ymd('2010-01-01')
DATE.MAX_1 <- ymd('2010-12-31')
DATE.MIN_2 <- ymd('2024-01-01')
DATE.MAX_2 <- ymd('2024-12-31')


#' Path to JSON file with Census API key.
PTH.CENSUS_API_KEY <- '~/census_api_key.json'


#' Census geography level.
#' More info: census.gov/programs-surveys/geography/about/glossary.html
LEVEL <- 'tract'


#' Geographic codes: census.gov/library/reference/code-lists/ansi.html
COUNTY <- c(Cook = '031') #' Cook County
STATE <- c(IL = '17') #' Illinois


#' Census survey to use.
SURVEY <- c('ACS-5' = 'acs5')


#' Tracts to be adjusted.
V.TRACTS_MDW <- c('17031640100', '17031980100') #' Midway
V.TRACTS_NVY <- c('17031081202', '17031081403') #' Navy Pier
V.TRACTS_ORD <- c('17031770700', '17031811701', '17031980000') #' O'Hare


#' MDW proper tract.
GEOID.MDW <- '17031980100'


#' Years for which to download Census data. Most recent data is from 2024.
V.YEARS_CENSUS <- 2010:2024


#' Year for canonical Midway tract geometry.
YR.MDW_TEMPLATE <- '2024'


#' Year of target geometry (for standardizing polygons).
YR.RESHAPE_TEMPLATE <- '2024'


#' Year of geometry for restoring '3301' tracts.
YR.RESTORE_3301.01 <- '2024'


#' Qualitative palette, colorblind-accessible.
PAL.CB <- palette.colors(palette = 'Okabe-Ito')


#' Date new Police geometry took effect.
DATE.POL_NEW <- ymd('20121219')


#' Paths to files with edited geometry for the '3301' Census tracts.
FNM.GEO_3301_10_19 <- 
  str_c('data/Census--2010-2019--3301-tracts--manual-edits--',
        'saved-20260414-191600.geojson')
FNM.GEO_3301_20_24 <- 
  str_c('data/Census--2020-2024--3301-tracts--manual-edits--',
        'saved-20260414-191500.geojson')