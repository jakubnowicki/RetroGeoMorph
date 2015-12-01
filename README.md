# **RetroGeoMorph**

Package: RetroGeoMorph  
Type: Package  
Title: Generating minimal distance matrices from tectonic-deformed landmark data.  
Version: 0.02  
Date: 2015-12-01  
Author: Jakub Nowicki  
Maintainer: Jakub Nowicki <j.nowicki@student.uw.edu.pl>  
Description: Package is a tool for creating Procrustes distance matrices from landmark data. The main pourpose of use is the search for the "minimal distance" configuration of streched landmarks. Results can be then analysed using various methods, including non-metric multidimensional scaling (for example from the package vegan).  
License: GPL-2  
Imports: doParallel, shapes, abind, geomorph, dplyr  

#### **Instalation:**
library(devtools)  
install_github('jakubnowicki/RetroGeoMorph')
