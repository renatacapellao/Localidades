library("rgbif")
library(readxl)
library(readr)

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
aves_especies_gibfkey <- read_csv("GitHub/Localidades/aves_especies_gibfkey.csv")
peixes_especies_gibfkey <- read_csv("GitHub/Localidades/peixes_especies_gibfkey.csv")
repteis_especies_gibfkey <- read_csv("GitHub/Localidades/repteis_especies_gibfkey.csv")
anfibios_especies_gibfkey <- read_csv("GitHub/Localidades/anfibios_especies_gibfkey.csv")

library(raster)
BRA.map <- getData("GADM", country="BRA", level=1)
ES.map<- BRA.map[BRA.map$NAME_1 == "Espirito Santo",]
ES_ext<-extent(ES.map)
rm(BRA.map)
rm(ES.map)

grupos<-c("mamiferos_especies_gibfkey", "aves_especies_gibfkey","anfibios_especies_gibfkey","repteis_especies_gibfkey", "peixes_especies_gibfkey")

selecionar <- function(type) {
  switch(type,
         "mamiferos_especies_gibfkey" = mamiferos_especies_gibfkey,
         "aves_especies_gibfkey" = aves_especies_gibfkey,
         "anfibios_especies_gibfkey" = anfibios_especies_gibfkey,
         "repteis_especies_gibfkey" = repteis_especies_gibfkey,
         "peixes_especies_gibfkey" = peixes_especies_gibfkey)
}

library("rgbif")
for(g in c(1:length(grupos))){
  
  print(paste0("#####################   DATASET: ", grupos[g], "   #####################"))
  dataset <- selecionar(grupos[g])
  dataset<- subset(dataset, (key != 0))
  registros_gbif_es <- array(dim=c(nrow(dataset), 1000,2))
  
  
  # registros_gbif_es<-list()
  # registros_gbif_es[[1]]<-dataset[,1]
  # registros_gbif_es[[2]]<-dataset[,2]
  # registros_gbif_es[[3]]<-list()
  
  for(i in c(1: nrow(dataset))){
    print(paste0(i, " de ", nrow(dataset), ": espécie = ", registros_gbif_es[[1]][[1]][[i]]))
    gbif_data <- occ_search(taxonKey = registros_gbif_es[[2]][[1]][[i]], return = "data")
    gbif_data<- as.data.frame(gbif_data)
    #row.names(gbif_data)<- gbif_data$key
    gbif_data_sub <- subset(gbif_data, !is.na(decimalLongitude) & !is.na(decimalLatitude))
    gbif_data_sub <- subset(gbif_data_sub, (decimalLongitude != 0) & (decimalLatitude != 0))
    coord<- gbif_data_sub[, c(4, 3)]
    
    colnames(coord) <- c("Longitude", "Latitude")
    coord_ES <- subset(coord, (Longitude >= ES_ext[1]) & (Longitude <= ES_ext[2]))
    registros_ES<- subset(coord_ES, (Latitude >= ES_ext[3]) & (Latitude <= ES_ext[4]))
    registros_gbif_es[[3]][[i]]<-list()
    if (!is.null(registros_ES)){
      registros_gbif_es[[3]][[i]]<- list(registros_ES)
      print(paste0(nrow(registros_ES)," de ", nrow(coord) ," registros encontrados"))
    }
  }
  if(g==1){
    registrosgbif_es_mamiferos <- registros_gbif_es
  }
  if(g==2){
    registrosgbif_es_aves <- registros_gbif_es
  }
  if(g==3){
    registrosgbif_es_anfibios <- registros_gbif_es
  }
  if(g==4){
    registrosgbif_es_repteis <- registros_gbif_es
  }
  if(g==5){
    registrosgbif_es_peixes <- registros_gbif_es
  }
  rm(registros_gbif_es)
}
