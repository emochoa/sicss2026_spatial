#' This script contains instructions for preparing to run through the
#' `sicss2026_spatial` tutorial.




# CENSUS API KEY ---------------------------------------------------------------


#' Sign up for a Census API key: https://api.census.gov/data/key_signup.html
#' Using any text editor, store the key in a single-line JSON file with this
#' format: `{"key":"key_value","email":"email_address"}`
#' Be sure to substitute 'key_value' with the key and 'email_address' with the
#' email address you used when requesting the key.
#' Name the file 'census_api_key.json' and place it in your home directory.
#' --Do not place the file in this (or any) GitHub (or other version-control)
#'   repository. Do not share the key publicly or with anyone else.




# INSTALL GDAL SYSTEM LIBRARY --------------------------------------------------


#' If it is not already installed, install the GDAL system library:
#' https://gdal.org/en/stable/download.html




# INSTALL R LIBRARIES ----------------------------------------------------------


#' Install `renv` and activate environment; install `RSocrata` and `lwgeom`.


#' Install `renv`.
install.packages('renv')


#' Activate the project.
renv::activate()


#' Install RSocrata from the City of Chicago's GitHub repository.
renv::install('Chicago/RSocrata')


#' *ALL USERS:* Enter the *ABSOLUTE PATH* (starting from the root, '/') to this
#' repository (remove the brackets, as well).
#' 
#' Windows users: It may be necessary to use backslashes instead of forward-
#' slashes (and possibly escape those, as well). Adjust as needed (what does
#' your usual workflow use for path separators?).
#' 
#' Windows and Unix-like users: I do not know whether the `lwgeom` library will
#' work as expected. Should you encounter difficulties, it may not be possible
#' to complete certain geometric operations or mapping visualizations. In this
#' case, it may still be possible to install `lwgeom` in the usual manner, which
#' is by installing the `sf` library outside of the `sicss2026_spatial` project.
#' You may wish to create a different project without using `renv`, copy in this
#' repository's scripts and data, install `pacman`, then run `00b--Bootstrap.R`.
#' 
#' MacOS users: During the course of the tutorial, you may see a dialogue box
#' with something along the lines of: 'Apple could not verify `lwgeom` is free
#' of malware that may harm your Mac or compromise your privacy'. DO NOT CLICK
#' 'Send to trash'; choose the other option. Then open your MacOS `Settings`,
#' choose `Privacy & Security`, scroll to the bottom, then authorize the OS to
#' open `lwgeom`.
renv::install('[ABSOLUTE PATH TO THE sicss2026_spatial REPOSITORY]/renv/library/macOS/R-4.5/aarch64-apple-darwin20/lwgeom/')