# 
# input$btnapagar
# input$btneliminarduplicatas
# input$btnsearch_spdatacsv
# input$btnsearch_spdata
# 
# if (is.null(occur.data.coord)) {
#   n <- 0
# }
# 
# n <- nrow(occur.data.coord)
# 
# if (n > 0) {
#   if (exists("occur.data.coord")) {
    library(stringr)
HERB <- read_delim("GitHub/Localidades/HERB1 (Renata).csv", ";", escape_double = FALSE, trim_ws = TRUE)
    dataset<-data.frame(HERB)
    coord<- dataset[, c(1, 2, 3)]
    for(l in c(1:nrow(dataset))){
      Especie<- paste(dataset[l,7], dataset[l,8], sep=" ")
      print(paste0(Especie))
      Long <- paste0(dataset[l,25],"°",dataset[l,26],"'", dataset[l,27], '"', dataset[l,28])
      Lat <- paste0(dataset[l,21],"°",dataset[l,22],"'", dataset[l,23], '"', dataset[l,24])
      coord[l,1]<- as.character(Especie)
      coord[l,2]<- Long
      coord[l,3]<- Lat
    }
    names(coord)<-c("Especie", "Long", "Lat")
   coord<-as.data.frame(coord)
     sp_name<-list(coord[,1])
    sp_name<- as.factor(sp_name)
    especies<-levels(sp_name)
   
    
     
    
  duplicados.index <-  unique(coord)
  for(i in c(1:length(especies))){
    print(paste0(sp_name[i]))
   species <- sp_name[i]
    sp_data<- subset(dataset, dataset[,2]== species)
    coord<- data.frame(sp_data[,2],sp_data[,11],sp_data[,12])
    sp<- dataset[,2]
    teste<- data.frame(dataset[,2],dataset[,13],dataset[,14])
    
     duplicados <- sp_data[duplicated(sp_data), ]
    occur.data.coord <- unique(dataset)
  }      
    
   
