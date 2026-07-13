#' Adapt a number of functions in the `areal` library for more general use.




# FUNCTIONS --------------------------------------------------------------------


f.areal.aw_interpolate <- function(geo_dough,
                                   tid_cutter,
                                   geo_cutter,
                                   sid_dough,
                                   weight,
                                   output,
                                   extensive,
                                   intensive){
  #' Adapted from `areal::aw_interpolate()`: github.com/chris-prener/areal
  #' 
  #' The `areal::aw_interpolate()` function as published does not allow for the
  #' source and target IDs to be passed as a string assigned to a variable; for
  #' example, the following would fail:
  #' `target_id <- 'area_id'`
  #' `areal::aw_interpolate(..., tid = target_id, ...)`
  #' 
  #' To allow for this, I changed seven functions in the `areal` library:
  #' --`areal::aw_interpolate()`
  #' --`areal:::aw_interpolate_single()`
  #' --`areal:::aw_interpolate_multiple()`
  #' --`areal:::strip_df()`
  #' --`areal:::aw_interpolater()`
  #' --`areal:::aw_aggregate()`
  #' --`areal:::aw_total()`
  #' 
  #' Additionally, `dplyr` was throwing a warning about `select` syntax, so I
  #' updated `areal:::aw_strip_df()` to use `all_of()`, as instructed.
  #' 
  #' The following types of changes have been made:
  #' --Some code blocks have been completely commented out
  #' --Some code has been changed to use different functions or variables
  #' --Some code has been added
  #' --Code style has been adjusted to conform with this project's style
  #' --The `areal` library is referenced explicitly for all calls to its
  #'   unchanged functions
  
  #' Rename input objects as expected by the rest of the function.
  .data  <- sf::st_transform(geo_cutter, CRS.AREAL)
  source <- sf::st_transform(geo_dough, CRS.AREAL)
  tid    <- tid_cutter
  sid    <- sid_dough
  
  paramList <- as.list(match.call())
  
  if(missing(.data)){
    stop("A sf object containing target data must be specified for the '.data'",
         " argument.")
  }
  
  if(missing(tid)){
    stop("A variable name must be specified for the 'tid' argument.")
  }
  
  if(missing(source)){
    stop("A sf object must be specified for the 'source' argument.")
  }
  
  if(missing(sid)){
    stop("A variable name must be specified for the 'sid' argument.")
  }
  
  #' The following line has been changed to check for `NULL` values instead of
  #' whether the objects exist. This is necessary to accommodate all calls to
  #' this function either explicitly or implicitly passing both arguments.
  #' The original code (reformatted for style) has been commented out.
  # if(missing(extensive) & missing(intensive)){
  if(is.null(extensive) & is.null(intensive)){
    stop("Either 'extensive' or 'intenstive' must be specified with an ",
         "accompanying list of variables to interpolate.")
  }
  
  #' These three lines have been changed to check for `NULL` values instead of
  #' whether the objects exist, as in the previous comment. The original code
  #' (reformatted for style) has been commented out.
  # if(missing(intensive) & !missing(extensive))       type <- "extensive"
  # else if(!missing(intensive) & missing(extensive))  type <- "intensive"
  # else if(!missing(intensive) & !missing(extensive)) type <- "mixed"
  if(      is.null(intensive) & !is.null(extensive)) type <- "extensive"
  else if(!is.null(intensive) &  is.null(extensive)) type <- "intensive"
  else if(!is.null(intensive) & !is.null(extensive)) type <- "mixed"
  
  
  #' Updated to allow for `weight = NULL`; original code is commented out.
  # if(weight %in% c("sum", "total") == FALSE){
  if(is.null(weight) || weight %in% c("sum", "total") == FALSE){
    stop(glue::glue("The given weight type '{var}' is not valid. 'weight' must",
                    " be either 'sum' or 'total'.",
                    #' Added provisions for `NA` and `NULL` values.
                    # var = weight))
                    var = weight, .na = '`NA`', .null = '`NULL`'))
  }
  
  
  if(type == "intensive" & weight == "total"){
    stop("Spatially intensive interpolations should be caclulated using ",
         "'sum' for 'weight'.")
  }
  
  if(output %in% c("sf", "tibble") == FALSE){
    stop(glue::glue("The given output type '{var}' is not valid. 'output' ",
                    "must be either 'sf' or 'tibble'.",
                    var = output))
  }
  
  if(!is.character(paramList$sid)){
    #' Updated in accordance to `rlang` instructions.
    # sidQ <- rlang::enquo(sid)
    sidQ <- rlang::enquo(sid) %>% rlang::quo_get_expr()
  }
  else if(is.character(paramList$sid)){
    #' Updated in accordance to `rlang` instructions.
    # sidQ <- rlang::quo(!!rlang::sym(sid))
    sidQ <- rlang::quo(!!rlang::sym(sid)) %>% rlang::quo_get_expr()
  }
  
  sidQN <- rlang::quo_name(rlang::enquo(sid))
  
  if(!is.character(paramList$tid)){
    #' Updated in accordance to `rlang` instructions.
    # tidQ <- rlang::enquo(tid)
    tidQ <- rlang::enquo(tid) %>% rlang::quo_get_expr()
  }
  else if(is.character(paramList$tid)){
    #' Updated in accordance to `rlang` instructions.
    # tidQ <- rlang::quo(!!rlang::sym(tid))
    tidQ <- rlang::quo(!!rlang::sym(tid)) %>% rlang::quo_get_expr()
  }
  
  tidQN <- rlang::quo_name(rlang::enquo(tid))
  
  if(!!sidQN %in% colnames(source) == FALSE){
    stop(glue::glue("Variable '{var}', given for the source ID ('sid'), ",
                    "cannot be found in the given source object.",
                    var = sidQN))
  }
  
  if(!!tidQN %in% colnames(.data) == FALSE){
    stop(glue::glue("Variable '{var}', given for the target ID ('tid'), ",
                    "cannot be found in the given target object.",
                    var = tidQN))
  }
  
  if(tidQN == sidQN){
    nameConflict <- TRUE
    tidOrig      <- tidQN
    .data        <- dplyr::rename(.data, ...tid = !!tidQN)
    tidQN        <- "...tid"
    tidQ         <- rlang::quo(!!rlang::sym(tidQN))
  }
  else nameConflict <- FALSE
  
  if(type == "extensive")      vars <- extensive
  else if(type == "intensive") vars <- intensive
  else if(type == "mixed")     vars <- c(extensive, intensive)
  
  if(areal::ar_validate(
    source  = source,
    target  = .data,
    varList = vars, 
    method  = "aw") == FALSE){
    stop("Data validation failed. Use ar_validate with verbose = TRUE to ",
         "identify concerns.")
  }
  
  if((type == "extensive" | type == "intensive") &
       length(vars) == 1){
    valueQ <- rlang::quo(!!rlang::sym(vars))
    
    #' One line of code has been changed to call a different function. The
    #' original line is commented out below.
    data <- 
      # areal:::aw_interpolate_single(
      f.areal.aw_interpolate_single(
        source = source,
        sid    = !!sidQ,
        value  = !!valueQ,
        target = .data,
        tid    = !!tidQ,
        type   = type,
        weight = weight)
  }
  else if((type == "extensive" | type == "intensive") &
            length(vars) > 1){
    #' Three lines of code have been changed to call a different function and
    #' pass different arguments. The original lines are commented out below.
    data <-
      # areal:::aw_interpolate_multiple(
      f.areal.aw_interpolate_multiple(
        source = source,
        sid    = !!sid,
        # sid    = sidQ,
        values = vars,
        target = .data,
        tid    = !!tid,
        # tid    = tidQ,
        type   = type,
        weight = weight)
  }
  else if(type == "mixed"){
    if(length(extensive) == 1){
      valueQ <- rlang::quo(!!rlang::sym(extensive))
      
      exresults <- 
        # areal:::aw_interpolate_single(
        f.areal.aw_interpolate_single(
          source = source, 
          sid    = !!sidQ,
          value  = !!valueQ,
          target = .data, 
          tid    = !!tidQ,
          type   = "extensive",
          weight = weight)
    }
    else if(length(extensive) > 1){
      exresults <- 
        # areal:::aw_interpolate_multiple(
        f.areal.aw_interpolate_multiple(
          source = source, 
          sid    = !!sidQ,
          values = extensive,
          target = .data, 
          tid    = !!tidQ,
          type   = "extensive",
          weight = weight)
    }
    
    if(length(intensive) == 1){
      valueQ <- rlang::quo(!!rlang::sym(intensive))
      
      inresults <- 
        # areal:::aw_interpolate_single(
        f.areal.aw_interpolate_single(
          source = source, 
          sid    = !!sidQ,
          value  = !!valueQ,
          target = .data, 
          tid    = !!tidQ,
          type   = "intensive",
          weight = "sum")
    }
    else if(length(intensive) > 1){
      inresults <- 
        # areal:::aw_interpolate_multiple(
        f.areal.aw_interpolate_multiple(
          source = source, 
          sid    = !!sidQ,
          values = intensive,
          target = .data, 
          tid    = !!tidQ,
          type   = "intensive",
          weight = "sum")
    }
    
    data <- dplyr::left_join(exresults, inresults, by = tidQN)
  }
  
  if(output == "sf"){
    out <- dplyr::left_join(.data, data, by = tidQN)
  }
  else if(output == "tibble"){
    data <- dplyr::left_join(.data, data, by = tidQN)
    
    sf::st_geometry(data) <- NULL
    
    out <- dplyr::as_tibble(data)
  }
  
  if(nameConflict == TRUE) out <- dplyr::rename(out, `:=`(!!tidOrig, !!tidQN))

  return(out)
}



f.areal.aw_interpolate_single <- 
  function(source, sid, value, target, tid, type, weight){
  #' Adapted from `areal:::aw_interpolate_single()`:
  #'   github.com/chris-prener/areal
  #' Three lines of code have been changed; these now call different functions.
  #' Some code has been changed to conform to this project's style.
  
  paramList <- as.list(match.call())
  sidQ      <- rlang::enquo(sid)
  sidQN     <- rlang::quo_name(rlang::enquo(sidQ))
  valueQ    <- rlang::enquo(value)
  valueQN   <- rlang::quo_name(rlang::enquo(value))
  tidQ      <- rlang::enquo(tid)
  tidQN     <- rlang::quo_name(rlang::enquo(tidQ))
  
  #' The following two lines have been changed to call `f.areal.aw_strip_df()`
  #' instead of `areal:::aw_strip_df()`. The original code is commented out.
  # sourceS <- areal:::aw_strip_df(source, id = sidQN, value = valueQN)
  # targetS <- areal:::aw_strip_df(target, id = tidQN)
  sourceS <- f.areal.aw_strip_df(source, id = sidQN, value = valueQN)
  targetS <- f.areal.aw_strip_df(target, id = tidQN)
  
  
  out <- 
    #' The following line has been changed to call `f.areal.aw_interpolater()`
    #' instead of `areal:::aw_interpolater()`. The original code has been
    #' commented out.
    # areal:::aw_interpolater(
    f.areal.aw_interpolater(
      source = sourceS,
      sid    = !!sidQ,
      value  = !!valueQ, 
      target = targetS,
      tid    = !!tidQ,
      type   = type,
      weight = weight)
  
  return(out)
}



f.areal.aw_interpolate_multiple <- function(source,
                                            sid,
                                            values,
                                            target,
                                            tid,
                                            type,
                                            weight){
  #' Adapted from `areal:::aw_interpolate_multiple()`:
  #'   github.com/chris-prener/areal
  #' Three lines of code have been changed to call different functions, all of
  #' which have also been adapted from the `areal` library. Some code has been
  #' adjusted or reformatted for style. The `areal` library is referenced
  #' explicitly for all calls to its unchanged functions.

    paramList <- as.list(match.call())
    sidQ      <- rlang::enquo(sid) %>% rlang::quo_get_expr()
    sidQN     <- rlang::quo_name(rlang::enquo(sidQ))
    tidQ      <- rlang::enquo(tid) %>% rlang::quo_get_expr()
    tidQN     <- rlang::quo_name(rlang::enquo(tidQ))
    colNames  <- c(tidQN, values)
    
    #' The next line, commented out, has been updated (in the line following it)
    #' from calling `areal:::aw_strip_df()` to calling `f.areal.aw_strip_df()`.
    #' targetS <- areal:::aw_strip_df(target, id = tidQN)
    targetS   <- f.areal.aw_strip_df(target, id = tidQN)
    
    
    #' Two lines of the original code in this block have been changed: the call
    #' to `areal:::aw_strip_df()` is now to `f.areal.aw_strip_df`, and the call
    #' to `areal:::aw_interpolater()` is now to `f.areal.aw_interpolater()`. The
    #' original lines have been commented out.
    out <- 
      values %>% 
      split(values) %>% 
      purrr::map(
        #' ~ areal:::aw_strip_df(source, id = !!sidQ, value = .x)) %>% 
        ~ f.areal.aw_strip_df(source, id = !!sidQ, value = .x)) %>% 
      purrr::imap(
        # ~ areal:::aw_interpolater(
        ~ f.areal.aw_interpolater(
            source   = .x,
            sid      = !!sidQ,
            value    = (!!rlang::quo(!!rlang::sym(.y))), 
            target   = targetS,
            tid      = !!tidQ,
            type     = type,
            weight   = weight, 
            multiple = TRUE)) %>% 
      purrr::reduce(.f = dplyr::bind_cols)
    
    
    sf::st_geometry(targetS) <- NULL
    
    out <- dplyr::bind_cols(targetS, out)
    
    return(out)
}



f.areal.aw_strip_df <- function(.data, id, value){
  #' Adapted from `areal:::aw_strip_df()`: github.com/chris-prener/areal
  #' `dplyr` was throwing an error, with instructions to use `all_of()` when
  #' selecting variables by quosures. Two lines of code have been changed to do
  #' this. The original lines of code are commented out. Style has been changed
  #' to conform with this project's style.

  paramList <- as.list(match.call())
  idQ       <- rlang::enquo(id)
  
  if(missing(value)){
    #' The following line of code has been changed: `all_of` is now applied to
    #' `!!idQ` when selecting. The original line of code has been commented out.
    # out <- dplyr::select(.data, !!idQ)
    out <- dplyr::select(.data, all_of(!!idQ))
  }
  else {
    valsQ <- rlang::enquo(value)
    
    #' The following line of code has been changed: `all_of` is now applied when
    #' selecting. The original line of code has been commented out.
    # out <- dplyr::select(.data, !!idQ, !!valsQ)
    out   <- dplyr::select(.data, all_of(c(!!idQ, !!valsQ)))
  }
  
  return(out)
}



f.areal.aw_interpolater <- function(source,
                                    sid,
                                    value,
                                    target,
                                    tid,
                                    type,
                                    weight,
                                    multiple = FALSE){
  #' Adapted from `areal:::aw_interpolater()`: github.com/chris-prener/areal
  #' Three lines of code have been changed to call different functions: the two
  #' calls to `areal:::aw_total()` now point to `f.areal.aw_total()`; the call
  #' to `areal:::aw_aggregate()` now points to `f.areal.aw_aggregate` instead.
  #' Some code has been adjusted or reformatted for style. The `areal` library
  #' is referenced explicitly for all calls to its unchanged functions.
  
  paramList <- as.list(match.call())
  sidQ      <- rlang::enquo(sid)
  valueQ    <- rlang::enquo(value)
  valueQN   <- rlang::quo_name(rlang::enquo(value))
  tidQ      <- rlang::enquo(tid)
  
  intersected <- 
    target %>% 
    areal:::aw_intersect(source = source, areaVar = "...area")
  
  
  #' Two lines in the code below have been changed: both the calls to the
  #' `areal:::aw_total()` function now point to `f.areal.aw_total()` instead.
  #' The original lines have been commented out.
  if(type == "extensive"){
      totaled <- 
        intersected %>% 
        # areal:::aw_total(
        f.areal.aw_total(
          source   = source, 
          id       = !!sidQ,
          areaVar  = "...area",
          totalVar = "...totalArea", 
          type     = "extensive",
          weight   = weight)
  }
  else if(type == "intensive"){
      totaled <- 
        intersected %>% 
        # areal:::aw_total(
        f.areal.aw_total(
          source   = source, 
          id       = !!tidQ,
          areaVar  = "...area",
          totalVar = "...totalArea", 
          weight   = weight,
          type     = "intensive")
  }
  
  
  #' One line of the original code in this block has been changed: the call to
  #' `areal:::aw_aggregate()` has been replaced by a call to an updated
  #' function, `f.areal.aw_aggregate()`. The original line is commented out.
  out <- 
    totaled %>% 
    areal:::aw_weight(
      areaVar    = "...area",
      totalVar   = "...totalArea", 
      areaWeight = "...areaWeight") %>% 
    areal:::aw_calculate(
      value      = !!valueQ, 
      areaWeight = "...areaWeight") %>% 
    # areal:::aw_aggregate(
    f.areal.aw_aggregate(
      target   = target, 
      tid      = !!tidQ,
      interVar = !!valueQ)
  
  sf::st_geometry(out) <- NULL
  
  if(multiple == TRUE) out <- dplyr::select(out, !!valueQ)
  
  return(out)
}



f.areal.aw_aggregate <- function(.data, target, tid, interVar, newVar){
  #' Adapted from `areal:::aw_aggregate()`: github.com/chris-prener/areal
  #' One line of code has been substantively changed to allow for the original
  #' function call to `aw_interpolate()` to have accepted `tid` as a string
  #' assigned to a variable. Some code has been adjusted or reformatted for
  #' style. The `areal` library is referenced explicitly for all calls to its
  #' unchanged functions.
  
    paramList <- as.list(match.call())
    
    if(missing(.data)){
      stop("A sf object containing intersected data must be specified for the",
           " '.data' argument.")
    }
    
    if(missing(target)){
      stop("A sf object must be specified for the 'target' argument.")
    }
    
    if(missing(tid)){
      stop("A variable name must be specified for the 'tid' argument.")
    }
    
    if(missing(interVar)){
      stop("A variable name must be specified for the 'interVar' argument.")
    }
    
    if(!is.character(paramList$tid))     tidQ <- rlang::enquo(tid)
    else if(is.character(paramList$tid)) tidQ <- rlang::quo(!!rlang::sym(tid))
    
    tidQN <- rlang::quo_name(rlang::enquo(tid))
    
    if(!is.character(paramList$interVar)){
      interVarQ <- rlang::enquo(interVar)
    }
    else if(is.character(paramList$interVar)){
        interVarQ <- rlang::quo(!!rlang::sym(interVar))
    }
    
    interVarQN <- rlang::quo_name(rlang::enquo(interVarQ))
    
    if(missing(newVar)){
      newVarQN <- interVarQN
    }
    else if(!missing(newVar)){
        if(!is.character(paramList$newVar)){
          newVarQ <- rlang::enquo(newVar)
        }
        else if(is.character(paramList$newVar)){
            newVarQ <- rlang::quo(!!rlang::sym(newVar))
        }
        
        newVarQN <- rlang::quo_name(rlang::enquo(newVarQ))
    }
    
    if(!!tidQN %in% colnames(target) == FALSE){
      stop(glue::glue("Variable '{var}', given for the target ID ('tid'),",
                      " cannot be found in the given target object.",
                      var = tidQN))
    }
    
    sf::st_geometry(.data) <- NULL
    
    
    #' One line of the original code in the block below has been changed: the
    #' call to `group_by()` now uses `!!rlang::sym(tidQN)` instead of `tidQ`
    #' (note the difference between `tidQN` and `tidQ`). The original line of
    #' code has been commented out.
    sum <-
      .data %>%
    #' dplyr::group_by(!!tidQ) %>%
      dplyr::group_by(!!rlang::sym(tidQN)) %>%
      dplyr::summarize(`:=`(!!newVarQN, base::sum(!!interVarQ)))
    
    
    out <- dplyr::left_join(target, sum, by = tidQN)
    
    return(out)
}



f.areal.aw_total <- 
  function(.data, source, id, areaVar, totalVar, type, weight){
  #' Adapted from `areal:::aw_total()`: github.com/chris-prener/areal
  #' Three lines of code have been substantively changed, two to allow for the
  #' original function call to `aw_interpolate()` to have accepted `tid` as a
  #' string assigned to a variable; one other has been updated for robustness
  #' against `weight` having the value `NA` or `NULL`, along with an additional
  #' line also used for that purpose. Some code has been adjusted or reformatted
  #' for style. The `areal` library is referenced  explicitly for all calls to
  #' its unchanged functions.
  
  paramList <- as.list(match.call())
  geometry  <- NULL
  
  if(missing(.data)){
    stop("A sf object containing intersected data must be specified for the ",
         "'.data' argument.")
  }
  
  if(missing(source)){
    stop("A sf object containing souce data must be specified for the 'source'",
         " argument.")
  }
  
  if(missing(id)){
    stop("A variable name must be specified for the 'id' argument.")
  }
  
  if(missing(areaVar)){
    stop("A variable name must be specified for the 'areaVar' argument.")
  }
  
  if(missing(totalVar)){
    stop("A variable name must be specified for the 'totalVar' argument.")
  }
  
  if(missing(type)){
    stop("An interpolation type (either 'extensive' or 'intensive') must be ",
         "specified for the 'type' argument.")
  }
  
  if(missing(weight)){
    stop("A weight type (either 'sum' or 'total') must be specified for the ",
         "'weight' argument.")
  }
  
  #' Updated to allow for `weight = NULL`; original code is commented out.
  # if(weight %in% c("sum", "total") == FALSE){
  if(is.null(weight) || weight %in% c("sum", "total") == FALSE){
    stop(glue::glue("The given weight type '{var}' is not valid. 'weight' must",
                    " be either 'sum' or 'total'.",
                    #' Added provisions for `NA` and `NULL` values.
                    # var = weight))
                    var = weight, .na = '`NA`', .null = '`NULL`'))
  }
  
  if(type == "intensive" & weight == "total"){
    stop("Spatially intensive interpolations should be caclulated using 'sum' ",
         "for 'weight'.")
  }
  
  if(!is.character(paramList$id)){
    idQ <- rlang::enquo(id)
  }
  else if(is.character(paramList$id)){
    idQ <- rlang::quo(!!rlang::sym(id))
  }
  
  idQN <- rlang::quo_name(rlang::enquo(id))
  
  if(!is.character(paramList$areaVar)){
    areaVarQ <- rlang::enquo(areaVar)
  }
  else if(is.character(paramList$areaVar)){
    areaVarQ <- rlang::quo(!!rlang::sym(areaVar))
  }
  
  areaVarQN <- rlang::quo_name(rlang::enquo(areaVar))
  
  totalVarQN <- rlang::quo_name(rlang::enquo(totalVar))
  
  if(!!idQN %in% colnames(.data) == FALSE){
    stop(glue::glue("Variable '{var}', given for the ID ('id'), cannot be ",
                    "found in the given intersected object.", 
                    var = idQN))
  }
  
  if(!!areaVarQN != "...area"){
    if(!!areaVarQN %in% colnames(.data) == FALSE){
      stop(glue::glue("Variable '{var}', given for the area, cannot be found ",
                      "in the given intersected object.", 
                      var = areaVarQN))
    }
  }
  
  if(type == "intensive" | 
      (type == "extensive" & weight == "sum")){
    
    df <- .data
    
    sf::st_geometry(df) <- NULL
    
    #' One line of code in the block below has been changed: the call to
    #' `group_by()` now uses `!!rlang::sym(idQN)` instead of `idQ` (note the
    #' difference between `idQN` and `idQ`). The original line of code has been
    #' commented out.
    sum <- 
      df %>% 
      # dplyr::group_by(!!idQ) %>% 
      dplyr::group_by(!!rlang::sym(idQN)) %>%
      dplyr::summarize(`:=`(!!totalVarQN, base::sum(!!areaVarQ)))
    
    out <- dplyr::left_join(.data, sum, by = idQN)
  }
  else if(type == "extensive" & weight == "total"){
    #' One line of code in the block below has been changed; `idQ` is now first
    #' passed to `all_of()` before `select()`.
    total <- 
      source %>% 
      # dplyr::select(!!idQ) %>% 
      dplyr::select(all_of(!!idQ)) %>% 
      dplyr::mutate(`:=`(!!totalVarQN, unclass(sf::st_area(geometry))))
    
    sf::st_geometry(total) <- NULL
    
    out <- dplyr::left_join(.data, total, by = idQN)
  }
  
  return(out)
}