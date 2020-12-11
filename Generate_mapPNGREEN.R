library(dplyr)
library(ggplot2)
library(dggridR)
library(viridis)

arr_master <- readRDS("ShinyTabs/data_arr.RDS")

arr_master2 <- arr_master[complete.cases(arr_master$gr_mn),] 
arr_master <- arr_master2

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
  labs(fill = 'Green-up date \n') +
  geom_path(aes(x = long, y = lat, group = group),
            alpha = 0.4, color = 'black') + 
  theme(plot.margin=grid::unit(c(0,37,0,0), "mm"))

years <- sort(unique(as.character(arr_master$year)))
## function that generates the map according to year and species input
doplot <- function(PP_YEAR) {
  PP_YEAR <- as.numeric(PP_YEAR)
  arr_f <- arr_master[which(arr_master$year == PP_YEAR),]
 
  name <- paste(PP_YEAR,".png",sep="")
  
  #min/max for plotting using output data
  MIN <- 80#round(min(c(arr_f$arr_GAM_mean, arr_f$arr_IAR_mean), na.rm = TRUE), 1)
  MAX <- 200#round(max(c(arr_f$arr_GAM_mean, arr_f$arr_IAR_mean), na.rm = TRUE), 1)
  
  #create hex grid
  hexgrid6 <- dggridR::dgconstruct(res = 6)
  cell_grid <- dggridR::dgcellstogrid(hexgrid6, arr_f$cell)
  cell_grid$cell <- as.numeric(cell_grid$cell)
  
  #merge hex spatial data with HM data
  to_plt <- dplyr::inner_join(arr_f, cell_grid, by = 'cell')
  
  png(name, #quality = 100, 
      width = 819, height = 664, type = "cairo",
       res = 100)
   yrp <- pp +
      geom_polygon(data = to_plt, aes(x = long, 
                                      y = lat, group = group, 
                                      fill = gr_mn), alpha = 0.4) +
      scale_fill_viridis(option="viridis",limits = c(MIN, MAX)) + 
      geom_path(data = to_plt, aes(x = long, 
                                   y = lat, group = group
                                   ), 
                alpha = 0.4,
                color = 'black') +
      theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
    
  print(PP_YEAR)
  ggsave(filename = name, plot = yrp#,
         #height = 2.865#, width = 3.896667,
         #scale = 0.7
         )
  
  dev.off()
}

for(i in 1:length(years)){
      doplot(years[i])
}


#[1] 2002
#[1] "Oreothlypis_ruficapilla"
#[1] 2002
#[1] "Setophaga_caerulescens"
