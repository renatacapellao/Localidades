library("rgbif")
library("readxl")

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