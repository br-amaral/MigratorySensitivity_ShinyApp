## create grids in advance and save for each species cause the dggridR package
##  is not available anymore in the CRAN so it cannot be used to make a shiny app

library(dggridR)

TAB <- readRDS("Data/data_sensi.RDS")
cellnumbs <- as.data.frame(cbind(sort(unique(TAB$cell)),
                                 seq(1,length(sort(unique(TAB$cell))),1)))
colnames(cellnumbs) <- c("cell","cell2")

TAB <- left_join(TAB,cellnumbs, by="cell")

slist <- sort(unique(TAB$species))

for(i in 1:length(slist)) {
  
    species <- slist[i]
    
    arr_f <- TAB[which(TAB$species == species),]

    #create hex grid
    hexgrid6 <- dggridR::dgconstruct(res = 6)
    cell_grid <- dggridR::dgcellstogrid(hexgrid6, arr_f$cell)
    cell_grid$cell <- as.numeric(cell_grid$cell)
    cell_grid <- left_join(cell_grid, cellnumbs, by = "cell") %>% 
      select(-cell) %>% 
      rename(cell = cell2)
    
    name <- paste('cell_grid',species,sep="_")
 
    assign(name,cell_grid)
    
    print(species)
       
}

#master hex grid
hexgrid6 <- dggridR::dgconstruct(res = 6)
master_cell_grid <- dggridR::dgcellstogrid(hexgrid6, unique(TAB$cell))
master_cell_grid$cell <- as.numeric(master_cell_grid$cell)
master_cell_grid <- left_join(master_cell_grid, cellnumbs, by = "cell") %>% 
  select(-cell) %>% 
  rename(cell = cell2)

saveRDS(master_cell_grid, 'Data/master_cell_grid.rds')


#save only species cell grid objects to .RData file
objs <- ls()
to_rm <- objs[-grep('cell_grid_', objs)]
rm(list = to_rm)
save.image('Data/species_Grid.RData')

