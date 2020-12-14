## create grids in advance and save for each species cause the dggridR package
##  is not available anymore in the CRAN so it cannot be used to make a shiny app

library(dggridR)

TAB <- readRDS("Data/data_sensi.RDS")

slist <- sort(unique(TAB$species))

#create objects for grid for each species
for(i in 1:length(slist)) {

    #i <- 1  
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

#master hex grid
hexgrid6 <- dggridR::dgconstruct(res = 6)
master_cell_grid <- dggridR::dgcellstogrid(hexgrid6, unique(TAB$cell))
master_cell_grid$cell <- as.numeric(master_cell_grid$cell)
saveRDS(master_cell_grid, 'Data/master_cell_grid.rds')


#save only species cell grid objects to .RData file
objs <- ls()
to_rm <- objs[-grep('cell_grid_', objs)]
rm(list = to_rm)
save.image('Data/species_Grid.RData')

