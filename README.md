# Areal Interpolation and Event Aggregation
### Introduction to Areal Interpolation, Event Aggregation, and Descriptive Spatial Visualization
Workshop presentation, 2026 SICSS — Chicago State University, Chicago, Illinois, 2026 July 15

<br>

This tutorial offers an introduction to:

* Downloading Census data using the Census API
* Downloading crime records and administrative boundaries using the [City of Chicago's Data Portal](https://data.cityofchicago.org/)
* Areal interpolation across geographic levels using the [areal](https://chris-prener.github.io/areal/) library
* Aggregating crime events by areal unit
* Generating [choropleth maps](https://en.wikipedia.org/wiki/Choropleth_map) of crime rates and demographic characteristics

<br>

The goal of this tutorial is to create datasets of crime rates over time at different levels of administrative units (Census tracts, Chicago city administrative boundaries, and Chicago Police Department unit boundaries).

<br>

This tutorial is designed to run in a reproducible environment created with the [`renv`](https://rstudio.github.io/renv/articles/renv.html) R library. Begin by cloning this repository locally.

To work through the tutorial, you will need to install the [GDAL system library](https://gdal.org/en/stable/) (this may require the installation of additional system libraries).

After installing GDAL, run the `src/00a--Activate.R` installation script, which should install the necessary versions of the R libraries needed.  We will spend some time addressing any issues students may encounter before we dive in.

You'll also need a Census API key; sign up for one [here](https://api.census.gov/data/key_signup.html).  Then, using any text editor, store the key in your home directory on your computer in a single-line JSON file with this format:
`{"key":"key_value","email":"email_address"}`

* Be sure to substitute `key_value` with the key and `email_address` with the email address you used when requesting the key.
* Name the file `census_api_key.json` and place it in your home directory.
**Do not place the file in any publicly accessible location. This file should never be accessible by anyone but you.**


<hr>


This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.


This work is licensed under <a href="https://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International</a> <img src="https://mirrors.creativecommons.org/presskit/icons/cc.svg" alt="CC logo" width=25 height=25> <img src="https://mirrors.creativecommons.org/presskit/icons/by.svg" alt="CC logo" width=25 height=25>
