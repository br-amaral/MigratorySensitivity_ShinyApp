library(dplyr)
library(ggplot2)
library(dggridR)

arr_master <- readRDS("Data/Raw/pheno-data-2020-08-25.rds")

worldmap <- ggplot2::map_data("world")
pp <- ggplot(data = worldmap, aes(x = long, y = lat, 
                                  group = group)) +
  geom_polygon(fill = alpha('black', 0.1), color = NA) +
  coord_map("ortho", orientation = c(35, -80, 0),
            xlim = c(-110, -50), ylim = c(21, 66)) +
  #theme_bw() +
  theme(panel.grid.major = element_line(color = alpha('black', 0.2),
                                        size = 0.5),
        panel.ontop = TRUE,
        panel.background = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab('') +
  ylab('') +
  labs(fill = 'Estimated Arrival \n   (day of year) \n ') +
  geom_path(aes(x = long, y = lat, group = group),
            alpha = 0.4, color = 'black') + 
  theme(plot.margin=grid::unit(c(0,37,0,0), "mm"))

spslist <- (unique(as.character(arr_master$species)))
years <- sort(unique(as.character(arr_master$year)))
mods <- c("GAM","IAR")
rangs <- c("bre","mig","both")

## function that generates the map according to year, range, model and species 
## arguments so you can test the function when there is no data:
##     species <- arr_master$species[1] ; rang <- "both" ; mod <- "GAM" ; PP_YEAR <- 2002
doplot <- function(PP_YEAR, species,mod,rang) {
  PP_YEAR <- as.numeric(PP_YEAR)
  arr_f <- arr_master[which(arr_master$species == species),]
  arr_f <- arr_f[which(arr_f$per_ovr >= 0.05),]
  
  #min/max for plotting using output data
  MIN <- floor(min(c(arr_f$arr_GAM_mean, arr_f$arr_IAR_mean), na.rm = TRUE))
  MAX <- ceiling(max(c(arr_f$arr_GAM_mean, arr_f$arr_IAR_mean), na.rm = TRUE))
  
  #add missing years to df
  miss_yrs <- as.numeric(years[which(years %ni% unique(arr_f$year))])

  arr_f <- arr_f[which(arr_f$year == PP_YEAR),]
  
  if(rang == "bre"){ arr_f <- arr_f[which(arr_f$breed_cell==TRUE),]}
  if(rang == "mig"){ arr_f <- arr_f[which(arr_f$mig_cell==TRUE),]}

  name <- paste("images/",PP_YEAR,sep="") %>%
    paste(species,mod,rang,sep="_") %>%
    paste(".png",sep="")
  
  if(dim(arr_f)[1]==0) {  ## no data
    png(name,# quality = 100,
        width = 819, height = 664, type = "cairo",
                               res = 100)
    hexgrid6 <- dggridR::dgconstruct(res = 6)
    cell_grid <- dggridR::dgcellstogrid(hexgrid6, arr_master$cell)
    cell_grid$cell <- as.numeric(cell_grid$cell)
    
    #merge hex spatial data with HM data
    to_plt <- dplyr::inner_join(arr_master, cell_grid, by = 'cell')
    
    elp <- 
      pp +
      geom_polygon(data = to_plt, aes(x = long, 
                                      y = lat, group = group#, 
                                      #fill = arr_IAR_mean
                                      ),
                   alpha = 0.4) +
      geom_path(data = to_plt, aes(x = long, 
                                   y = lat, group = group), 
                alpha = 0.4, color = 'black') +
      theme(plot.margin=grid::unit(c(0,37,0,0), "mm")) +
      ## Trying to add legend to plot with only gray hexagons:
      scale_fill_gradientn(colors = c('red', 'blue'),
                           limits = c(MIN, MAX)) 
    
    ggsave(filename = name, plot = elp)
  } else {
    
    #create hex grid
    hexgrid6 <- dggridR::dgconstruct(res = 6)
    cell_grid <- dggridR::dgcellstogrid(hexgrid6, arr_f$cell)
    cell_grid$cell <- as.numeric(cell_grid$cell)
    
    #merge hex spatial data with HM data
    to_plt <- dplyr::inner_join(arr_f, cell_grid, by = 'cell')
    
    png(name, #quality = 100, 
        width = 819, height = 664, type = "cairo",
         res = 100)
    
    if(mod == "IAR") { elp <- 
      pp +
        geom_polygon(data = to_plt, aes(x = long, 
                                        y = lat, group = group, 
                                        fill = arr_IAR_mean), alpha = 0.4) +
        geom_path(data = to_plt, aes(x = long, 
                                     y = lat, group = group), 
                  alpha = 0.4, color = 'black') +
        scale_fill_gradientn(colors = c('red', 'blue'),
                             limits = c(MIN, MAX)) + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
    }else{ elp <- 
      pp +
        geom_polygon(data = to_plt, aes(x = long, 
                                        y = lat, group = group, 
                                        fill = arr_GAM_mean), alpha = 0.4) +
        geom_path(data = to_plt, aes(x = long, 
                                     y = lat, group = group), 
                  alpha = 0.4, color = 'black') +
        scale_fill_gradientn(colors = c('red', 'blue'),
                             limits = c(MIN, MAX)) + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
    }
    print(name)
    ggsave(filename = name, plot = elp#,
           #height = 2.865#, width = 3.896667,
           #scale = 0.7
           )
  }
  dev.off()
}

## 


## generate png maps
for(i in 1:length(years)){
  for(j in 1:length(spslist)){
    for(k in 1:length(mods)){
      for(l in 1:length(rangs)){
        #i <- 1
        #j <- 1
        #k <- 1
        #l <- 1
        doplot(years[i],spslist[j],mods[k],rangs[l])
      }
    }
  }
}



