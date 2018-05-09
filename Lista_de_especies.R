library("rgbif", lib.loc = "~/R/R-3.4.4/library")
library(readxl)

# fauna_2005 <- read_excel("2017.05.22 - Especies da fauna ameacadas.xlsx",
#   col_types = c(
#     "numeric", "numeric", "text",
#     "text", "text", "text", "text"
#   ),
#   skip = 1
# )
# 
# flora_2005 <- read_excel("2017.05.22 - Especies da flora ameacadas.xlsx",
#   col_types = c(
#     "numeric", "text", "text",
#     "text", "text", "text", "text", "text",
#     "text"
#   )
# )

mamiferos_especies_gibfkey <- read_csv("GitHub/Localidades/mamiferos_especies_gibfkey.csv")
# aves_especies_gibfkey <- read_csv("GitHub/Localidades/aves_especies_gibfkey.csv")
# peixes_especies_gibfkey <- read_csv("GitHub/Localidades/peixes_especies_gibfkey.csv")
# repteis_especies_gibfkey <- read_csv("GitHub/Localidades/repteis_especies_gibfkey.csv")
# anfibios_especies_gibfkey <- read_csv("GitHub/Localidades/anfibios_especies_gibfkey.csv")

#mamiferos_gibfkey_null<- subset(mamiferos_especies_gibfkey, (key == 0))
mamiferos_especies_gibfkey<- subset(mamiferos_especies_gibfkey, (key != 0))

library(raster)
BRA.map <- getData("GADM", country="BRA", level=1)
ES.map<- BRA.map[BRA.map$NAME_1 == "Espirito Santo",]
ES_ext<-extent(ES.map)
rm(BRA.map)

registros_gbifes_mam<-list(length(mamiferos_especies_gibfkey[[1]]))
registros_gbifes_mam[[1]]<-mamiferos_especies_gibfkey[,1]
registros_gbifes_mam[[2]]<-mamiferos_especies_gibfkey[,2]
registros_gbifes_mam[[3]]<-list()

for(i in c(1: nrow(mamiferos_especies_gibfkey))){
  print(paste0(i, " de ", nrow(mamiferos_especies_gibfkey), ": espécie = ", registros_gbifes_mam[[1]][[1]][[i]]))
  gbif_data <- occ_search(taxonKey = registros_gbifes_mam[[2]][[1]][[i]], return = "data")
  gbif_data<-as.data.frame(gbif_data)
  row.names(gbif_data)<- gbif_data$key
  gbif_data_sub <- subset(gbif_data, !is.na(decimalLongitude) & !is.na(decimalLatitude))
  gbif_data_sub <- subset(gbif_data_sub, (decimalLongitude != 0) & (decimalLatitude != 0))
  coord<- gbif_data_sub[, c(4, 3)]
  colnames(coord) <- c("Longitude", "Latitude")
  coord_ES<- subset(coord, (Longitude >= ES_ext[1]) & (Longitude <= ES_ext[2]))
  registros_ES<- subset(coord_ES, (Latitude >= ES_ext[3]) & (Latitude <= ES_ext[4]))
  registros_gbifes_mam[[3]][[i]]<-list()
  if (!is.null(registros_ES)){
     registros_gbifes_mam[[3]][[i]]<- list(registros_ES)
   print(paste0(nrow(registros_ES), " registros encontrados"))
 }
}





