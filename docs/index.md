--- 
title: "Geographic sampling"
author: "Sebastian Nickel"
date: "2018-09-11"
site: bookdown::bookdown_site
output: 
  bookdown::tufte_html_book:
    split_by: chapter
documentclass: book
biblio-style: apalike
link-citations: yes
github-repo: senickel/geosampling_book
---
# Prerequisites {-}
This small contains the method's used for sampling in GLD's urbanization project.  

## Study Area
The study includes the following areas:  

* The border area between Kenya and Tanzania.  
* The border area between Malawi, Tanzania, and Zambia.  
* The three capitals Dar es Salaam, Lusaka, and Nairobi. 

## Data
The following data is necessary:  

* [WorldPop](https://www.worldpop.org.uk/) raster data for Kenya, Tanzania, and Zambia  
* Administrative spatial polygons from [GADM](https://gadm.org/)
* Lakes spatial polygons from [Natural Earth](https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-rivers-lake-centerlines/)

## geosampling package
The geosampling package was created to simplify the process and has the necessary functions needed.


```r
devtools::install_github("senickel/geosampling")
```
