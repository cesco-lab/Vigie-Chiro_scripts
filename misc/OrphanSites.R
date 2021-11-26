library(data.table)
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")

SiteU=unique(SiteLoc$site)

OrphanSites=subset(SiteLoc,!(SiteLoc$site %in% Particip$site))
OrphanSitesU=unique(OrphanSites,by="site")
OrphanSitesUPed=subset(OrphanSitesU,OrphanSitesU$protocole=="CARRE")
OrphanSitesUPed$site
table(substr(OrphanSitesUPed$site,23,24))
table(OrphanSitesUPed$observateur)
