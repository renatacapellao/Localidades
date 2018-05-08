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

anfibios <- read_excel("Anfibios.xlsx")
mamiferos <- read_excel("mamiferos.xlsx")
peixes <- read_excel("peixes.xlsx")
repteis <- read_excel("repteis.xlsx")
aves <- read_excel("aves.xlsx")

anfibios_especies <- as.data.frame(anfibios)
anfibios_especies <- anfibios_especies[, 2]
anfibios_especies <- levels(as.factor(anfibios_especies))
sp <- array(dim = c(length(anfibios_especies), 2))
sp [, 1] <- anfibios_especies
colnames(sp) <- c("especie", "key")
anfibios_especies <- sp
anfibios_especies <- as.data.frame(anfibios_especies)
anfibios_especies[, 1] <- as.character(anfibios_especies[, 1])
anfibios_especies[, 2] <- as.numeric(anfibios_especies[, 2])

mamiferos_especies <- as.data.frame(mamiferos)
mamiferos_especies <- mamiferos_especies[, 2]
mamiferos_especies <- levels(as.factor(mamiferos_especies))
sp <- array(dim = c(length(mamiferos_especies), 2))
sp [, 1] <- mamiferos_especies
colnames(sp) <- c("especie", "key")
mamiferos_especies <- sp
mamiferos_especies <- as.data.frame(mamiferos_especies)
mamiferos_especies[, 1] <- as.character(mamiferos_especies[, 1])
mamiferos_especies[, 2] <- as.numeric(mamiferos_especies[, 2])

peixes_especies <- as.data.frame(peixes)
peixes_especies <- peixes_especies[, 2]
peixes_especies <- levels(as.factor(peixes_especies))
sp <- array(dim = c(length(peixes_especies), 2))
sp [, 1] <- peixes_especies
colnames(sp) <- c("especie", "key")
peixes_especies <- sp
peixes_especies <- as.data.frame(peixes_especies)
peixes_especies[, 1] <- as.character(peixes_especies[, 1])
peixes_especies[, 2] <- as.numeric(peixes_especies[, 2])

repteis_especies <- as.data.frame(repteis)
repteis_especies <- repteis_especies[, 2]
repteis_especies <- levels(as.factor(repteis_especies))
sp <- array(dim = c(length(repteis_especies), 2))
sp [, 1] <- repteis_especies
colnames(sp) <- c("especie", "key")
repteis_especies <- sp
repteis_especies <- as.data.frame(repteis_especies)
repteis_especies[, 1] <- as.character(repteis_especies[, 1])
repteis_especies[, 2] <- as.numeric(repteis_especies[, 2])

aves_especies <- as.data.frame(aves)
aves_especies <- aves_especies[, 2]
aves_especies <- levels(as.factor(aves_especies))
sp <- array(dim = c(length(aves_especies), 2))
sp [, 1] <- aves_especies
colnames(sp) <- c("especie", "key")
aves_especies <- sp
aves_especies <- as.data.frame(aves_especies)
aves_especies[, 1] <- as.character(aves_especies[, 1])
aves_especies[, 2] <- as.numeric(aves_especies[, 2])


##################### GET GBIF DATA #####################

library(rgbif)
grupos<-c("mamiferos_especies", "aves_especies","anfibios_especies","repteis_especies", "peixes_especies")

selecionar <- function(type) {
  switch(type,
         "mamiferos_especies" = mamiferos_especies,
         "aves_especies" = aves_especies,
         "anfibios_especies" = anfibios_especies,
         "repteis_especies" = repteis_especies,
         "peixes_especies" = peixes_especies)
}

for(g in c(1:length(grupos))){
  id <- selecionar(grupos[g])
  for (i in c(1:length(id[, 1]))) {
    key <- name_backbone(name = id[i, 1])$speciesKey
    if (length(key) < 1) {
      id[i, 2] <- 0
    } else {
      id[i, 2] <- key
    }
    print(paste(i,id[i, 1], sep = ":"))
  }
  write.csv(id, file= paste0(grupos[g], "_gibfkey.csv"),col.names=TRUE, row.names = FALSE )
}



  # for (i in c(1:length(grupo[, 1]))) {
  #   key <- name_backbone(name = grupo[i, 1])$speciesKey
  #   if (length(key) < 1) {
  #     grupo[i, 2] <- 0
  #   } else {
  #     grupo[i, 2] <- key
  #   }
  #   print(paste(i, grupo[i, 1], sep = ":"))
  # }




#clean <- function(coord, abio) {
#   if (dim(coord)[2] == 2) {
#     if (exists("abio")) {
#       # selecionar os pontos únicos e sem NA
#       mask = abio[[1]]
#       # Selecionar pontos espacialmente únicos #
#       cell <- cellFromXY(mask, coord)  # get the cell number for each point
#       dup <- duplicated(cell)
#       pts1 <- coord[!dup, ]  # select the records that are not duplicated
#       pts1 <- pts1[!is.na(raster::extract(mask, pts1)), ]  #selecionando apenas pontos que tem valor de raster
#       cat(dim(coord)[1] - dim(pts1)[1], "points removed\n")
#       cat(dim(pts1)[1], "spatially unique points\n")
#       names(pts1) = c("Longitude", "Latitude")#
#       return(pts1)
#     } else (cat("Indicate the object with the predictive variables"))
#   } else (stop("Coordinate table has more than two columns.\nThis table should only have longitude and latitude in this order."))
# }
# 
# gbif_data <- occ_search(taxonKey = 2437892)
# for(i in c(1:length(gbif_data))){
#   gbif_data_sub <- subset(gbif_data, !is.na(decimalLongitude) & !is.na(decimalLatitude))
#   gbif_data_sub <- subset(gbif_data_sub, (decimalLongitude != 0) & (decimalLatitude != 0))
#   occur.data <- gbif_data_sub[, c(4, 3)]
#   colnames(occur.data) <- c("Longitude", "Latitude")
# }
# 
# 
# library(raster)
# # use state bounds from gadm website:
# # us = shapefile("USA_adm1.shp")
# BRA.map <- getData("GADM", country="BRA", level=1)
# ES.contorno<- BRA.map[BRA.map$NAME_1 == "Espírito Santo",]
# #plot(ES)
# ext<-extent(ES.contorno)
# 
# occur.data.coord <<- clean(occur.data.coord, ext)