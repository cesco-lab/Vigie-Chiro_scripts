usethis::use_data_raw()
library(data.table)
library(tibble)
library(readr)
library(dplyr)

data=fread(file.choose())
glimpse(data)
datat=as.tibble(data)
glimpse(datat)


filter_count=function(x,my_species)
{
x %>%
  filter(tadarida_taxon %in% my_species) %>%
  count(tadarida_taxon_autre)
  
}
filter_count(datat,"Pippip")
