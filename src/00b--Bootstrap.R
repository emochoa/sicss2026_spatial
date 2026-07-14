#' Users for whom `00-Activate.R` was not successful: Make a new RProject in a
#' fresh directory, then run this script from there.


#' Install `pacman`, which allows for bulk library installation.
install.packages('pacman')


#' Install most libraries needed for the `sicss2026_spatial` tutorial (should
#' also install dependencies), if they are not already installed.
pacman::p_install('doParallel',
                  'glue',
                  'htmltools',
                  'httr',
                  'janitor',
                  'leaflet',
                  'mapview',
                  'multidplyr',
                  'parallel',
                  'purrr',
                  'rgeoda',
                  'rlang',
                  'rmapshaper',
                  'sf',
                  'tictoc',
                  'tidyverse',
                  'tigris',
                  'viridisLite')


#' Install RSocrata from the City of Chicago's GitHub repository.
pacman::p_install_gh('Chicago/RSocrata')