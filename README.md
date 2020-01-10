# Vigie-Chiro_scripts

[![Build Status](https://travis-ci.org/cesco-lab/Vigie-Chiro_scripts.svg?branch=master)](https://travis-ci.org/cesco-lab/Vigie-Chiro_scripts)
[![Build status](https://ci.appveyor.com/api/projects/status/wfsc5yog9pxn9xr2/branch/master?svg=true)](https://ci.appveyor.com/project/statnmap/vigie-chiro-scripts/branch/master)



Website with {pkgdown} for documentation: https://cesco-lab.github.io/Vigie-Chiro_scripts/

## Summary

The workflows are summarized [here](https://drive.google.com/open?id=1LV-Li36kZvC18UaklBbJp0hjeumf1fCh) (slides 6 to 20)
R scripts are colored blue, php scripts in yellow, manually edited inputs in green, and script outputs in white.
Scripts and files that are consuming a lot of memory are shown in a red box.

This repository should be useful mostly for Vigie-Chiro team and participants but might also prove useful to other team interested in managing acoustic monitoring data. 
We'll try to update and improve documentation as much as we can, but do not hesitate to send an e-mail to yves.bas@mnhn.fr for any issue, question, bug report and so on.



## Index

Please find below a description of every objects (scripts, inputs and outputs) in alphabetical order

### ActTot.r
This script computes acoustic activity data (number of species occurence among files) of every species for every full night of stationary recordings.

### export.txt
This is the standardised complete export of "observation" (species occurrence) data from Vigie-Chiro

### exportXXX.csv
a subset of export.txt according to SelExportDonnees.r settings

### extr_PF_DataLP.r 
merging participation (time covariates), locality (spatial covariates) and observation (species occurence) data, for stationary recordings

### Recherche de données
This a php script extracting "observation" data (species occurrence) from the Vigie-Chiro database (MongoDB). As other php scripts, this is not available yet because of server security policy.
 
### SelExportDonnees.r
write a subset of export.txt, either by date (a prefix of participation field) or id checking (obs.espece and valid.espece)

 
