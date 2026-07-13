f.read_chi_admin <- function(){
  #' Read in administrative geometry dataset and compute proportion variables.
  dir('data', 'ACS-5.+--Level-All-Administrative-Police', full.names = TRUE) %>%
    qs2::qs_read(nthreads = 10) %>% 
    lapply(function(l_geo_admin){
      lapply(l_geo_admin, function(l_geo_yrs){
        lapply(l_geo_yrs, function(geo){
          f.proportions(geo)
        }
        )
      }
      )
    }
    )
}



f.proportions <- function(geo_units){
  #' Compute demographic proportion variables.
  geo_units %>% 
    mutate(prop.femhh  = fem_hd_famhh / famhh.t,
           prop.no_hsd = no_hsd_25_64 / pop_25_64.t,
           prop.pai_hh = pai_hh / hh.t,
           prop.black  = pop_black / pop.t,
           prop.latinx = pop_latinx / pop.t,
           prop.movers = recent_movers / pop_1_plus.t,
           prop.ymales = young_males / pop.t)
}
