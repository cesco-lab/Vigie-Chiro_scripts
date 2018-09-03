library(data.table)

SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
write.csv(SiteLoc,"SL_pourQGIS.csv")
