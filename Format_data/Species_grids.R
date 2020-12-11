## create grids in advance and save for each species cause the dggridR package
##  is not available anymore in the CRAN so it cannot be used to make a shiny app

library(dggridR)

TAB <- readRDS("data_sensi.RDS")
colnames(TAB)[1] <- "scien"
colnames(TAB)[17] <- "species"

slist <- sort(unique(TAB$species))

for(i in 1:length(slist)) {
  
    species <- slist[i]
    
    arr_f <- TAB[which(TAB$species == species),]

    #create hex grid
    hexgrid6 <- dggridR::dgconstruct(res = 6)
    cell_grid <- dggridR::dgcellstogrid(hexgrid6, arr_f$cell)
    cell_grid$cell <- as.numeric(cell_grid$cell)
    
    name <- paste('cell_grid',species,sep="_")
 
    assign(name,cell_grid)
    
    print(species)
       
}
