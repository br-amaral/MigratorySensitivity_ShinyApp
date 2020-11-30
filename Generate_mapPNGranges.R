library(dplyr)
library(ggplot2)
library(dggridR)

#arr_master <- read.csv('arrival_master_subset_2020-06-08.csv')
setwd("~/Box/PhenoMismatch/App_pngMaps")

load("~/Box/PhenoMismatch/data_sps.RData")
arr_master <- data3
rm(data3)

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
        panel.background = element_rect(fill = NA)) +
  xlab('Longitude') +
  ylab('Latitude') +
  labs(fill = 'Estimated Arrival ') +
  geom_path(aes(x = long, y = lat, group = group),
            alpha = 0.4, color = 'black') + 
  theme(plot.margin=grid::unit(c(0,37,0,0), "mm"))


spslist <- (unique(as.character(arr_master$species)))
years <- sort(unique(as.character(arr_master$year)))
mods <- c("GAM","IAR")
rangs <- c("bre","mig","both")
## function that generates the map according to year and species input
doplot <- function(PP_YEAR, species,mod,rang) {
  PP_YEAR <- as.numeric(PP_YEAR)
  arr_f <- arr_master[which(arr_master$species == species),]
  arr_f <- arr_f[which(arr_f$year == PP_YEAR),]
  arr_f <- arr_f[which(arr_f$per_ovr > 0.01),]
  
  if(rang == "bre"){ arr_f <- arr_f[which(arr_f$breed_cell==TRUE),]}
  if(rang == "mig"){ arr_f <- arr_f[which(arr_f$mig_cell==TRUE),]}

  name <- paste("images/",PP_YEAR,sep="") %>%
    paste(species,mod,rang,sep="_") %>%
    paste(".png",sep="")
  
  if(dim(arr_f)[1]==0) {
    png(name,# quality = 100,
        width = 819, height = 664, type = "cairo",
                               res = 100)
    ggsave(filename = name, plot = last_plot())
  } else {
    #min/max for plotting using output data
    MIN <- floor(min(c(arr_f$arr_GAM_mean, arr_f$arr_IAR_mean), na.rm = TRUE))
    MAX <- ceiling(max(c(arr_f$arr_GAM_mean, arr_f$arr_IAR_mean), na.rm = TRUE))
    
    #create hex grid
    hexgrid6 <- dggridR::dgconstruct(res = 6)
    cell_grid <- dggridR::dgcellstogrid(hexgrid6, arr_f$cell)
    cell_grid$cell <- as.numeric(cell_grid$cell)
    
    #merge hex spatial data with HM data
    to_plt <- dplyr::inner_join(arr_f, cell_grid, by = 'cell')
    
    png(name, #quality = 100, 
        width = 819, height = 664, type = "cairo",
         res = 100)
    
    if(mod == "IAR") { #elp <- 
      pp +
        geom_polygon(data = to_plt, aes(x = long, 
                                        y = lat, group = group, 
                                        fill = arr_IAR_mean), alpha = 0.4) +
        geom_path(data = to_plt, aes(x = long, 
                                     y = lat, group = group), 
                  alpha = 0.4, color = 'black') +
        scale_fill_gradientn(colors = c('red', 'blue'),
                             limits = c(MIN, MAX)) + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
    }else{ #elp <- 
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
    ggsave(filename = name, plot = last_plot()#,
           #height = 2.865#, width = 3.896667,
           #scale = 0.7
           )
  }
  dev.off()
}

#lapply(PP_YEAR=years, species = spslist, mod = "GAM",doplot)
for(i in 1:length(years)){
  for(j in 1:length(spslist)){
    for(k in 1:length(mods)){
      for(l in 1:length(rangs)){
        doplot(years[i],spslist[j],mods[k],rangs[l])
      }
    }
  }
}


#[1] 2002
#[1] "Oreothlypis_ruficapilla"
#[1] 2002
#[1] "Setophaga_caerulescens"
