library("rgbif")
library(readxl)
library(readr)
library(raster)
library(scrubr)

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

mamiferos_especies_gibfkey <- read_csv("~/GitHub/Localidades/mamiferos_especies_gibfkey.csv")
aves_especies_gibfkey <- read_csv("~/GitHub/Localidades/aves_especies_gibfkey.csv")
peixes_especies_gibfkey <- read_csv("~/GitHub/Localidades/peixes_especies_gibfkey.csv")
repteis_especies_gibfkey <- read_csv("~/GitHub/Localidades/repteis_especies_gibfkey.csv")
anfibios_especies_gibfkey <- read_csv("~/GitHub/Localidades/anfibios_especies_gibfkey.csv")

BRA.map <- getData("GADM", country = "BRA", level = 1)
ES.map <- BRA.map[BRA.map$NAME_1 == "Espirito Santo", ]
ES_ext <- extent(ES.map)
rm(BRA.map)
rm(ES.map)

grupos <- c("mamiferos_especies_gibfkey", "aves_especies_gibfkey", "anfibios_especies_gibfkey", "repteis_especies_gibfkey", "peixes_especies_gibfkey")

selecionar <- function(type) {
  switch(type,
         "mamiferos_especies_gibfkey" = mamiferos_especies_gibfkey,
         "aves_especies_gibfkey" = aves_especies_gibfkey,
         "anfibios_especies_gibfkey" = anfibios_especies_gibfkey,
         "repteis_especies_gibfkey" = repteis_especies_gibfkey,
         "peixes_especies_gibfkey" = peixes_especies_gibfkey
  )
}

for (g in c(1:length(grupos))) {
  print(paste0("#####################   DATASET: ", grupos[g], "   #####################"))
  dataset <- selecionar(grupos[g])
  dataset <- subset(dataset, (key != 0))
  registros_gbif_es <- array(dim = c(nrow(dataset), 1000, 4))
  # sp key, lat, long
  
  for (i in c(1:nrow(dataset))) {
    print(paste0(i, " de ", nrow(dataset), ": espécie = ", dataset[i,1]))
    registros_gbif_es[i,k,1] <- dataset[i,1]
    registros_gbif_es[i,k,2] <- dataset[i,2]
    
    gbif_data <- occ_search(taxonKey = dataset[i,2], return = "data")
    gbif_data <- dframe(gbif_data)
    gbif_data_sub <- subset(gbif_data, !is.na(decimalLongitude) & !is.na(decimalLatitude))
    gbif_data_sub <- subset(gbif_data_sub, (decimalLongitude != 0) & (decimalLatitude != 0))
    coord <- gbif_data_sub[, c(4, 3)]
    
    colnames(coord) <- c("Longitude", "Latitude")
    coord_ES <- subset(coord, (Longitude >= ES_ext[1]) & (Longitude <= ES_ext[2]))
    registros_ES <- subset(coord_ES, (Latitude >= ES_ext[3]) & (Latitude <= ES_ext[4]))

    if (!is.null(registros_ES)) {
      print(paste0(nrow(registros_ES), " de ", nrow(coord), " registros encontrados"))
     
      for (k in c(1:nrow(registros_ES))){
        registros_gbif_es[i,k,3] <- registros_ES[k,1]
        registros_gbif_es[i,k,4] <- registros_ES[k,2]
      }
    }
  }
  write.csv(registros_gbif_es, file = paste0(grupos[g], "_registrosgbif.csv"), col.names = TRUE, row.names = FALSE)
  
  if (g == 1) {
    registrosgbif_es_mamiferos <- registros_gbif_es
  }
  if (g == 2) {
    registrosgbif_es_aves <- registros_gbif_es
  }
  if (g == 3) {
    registrosgbif_es_anfibios <- registros_gbif_es
    write.csv(registrosgbif_es_mamiferos, file = paste0(grupos[g], "_gibfkey.csv"), col.names = TRUE, row.names = FALSE)
  }
  if (g == 4) {
    registrosgbif_es_repteis <- registros_gbif_es
  }
  if (g == 5) {
    registrosgbif_es_peixes <- registros_gbif_es
  }
  rm(registros_gbif_es)
}
