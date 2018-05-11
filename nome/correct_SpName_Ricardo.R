library(flora)
library(scrubr)
library(stringr)
library(readxl)

# Onde gravar o arquivo de saída
# path <- "C:/Users/renata.capellao/Documents"
path <- getwd()

# Nome da família
familia <- "Acanthaceae"

original<- read_excel("Localidades/nome/Acanthaceae MBML.xlsx")
dataset <- original

dataset <- dframe(dataset)
dataset[, 73] <- NA
teste <- get.taxa(c("Miconia albicans"))
output <- data.frame(teste)
output[1:nrow(dataset),] <- NA

for (i in c(1:nrow(dataset))) {
  print(paste0(i))
  dataset[i, 73] <- paste(dataset[i, 7], dataset[i, 8], sep = " ")
  sp <- paste0(dataset[i, 73])
  flora_data <- get.taxa(sp)
  output[i,] <- flora_data[1,]
  dataset[i, 7] <- word(flora_data[1, 2], 1, sep = fixed(" "))
  dataset[i, 8] <- word(flora_data[1, 2], 2, sep = fixed(" "))
  dataset[i, 9] <- word(flora_data[1, 2], 3, -1)
}
dataset <- dataset[, 1:72]

write.table(
  output,
  file = paste0(path, "/florapkg_output", familia, ".csv"),
  sep = ";",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.table(
  dataset,
  file = paste0(path, "/nomes.editados_", familia, ".csv"),
  sep = ";",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

library(Taxonstand)
check_especie <- which(is.na(dataset[,8]))
check<-original[c(check_especie),]
dataset_tpl <- check
