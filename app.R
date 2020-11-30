## update abstract
## add link to the paper
## modify how to cite this data

library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(viridis)
library(maps)
library(mapproj)
library(robustHD)
library("tidyverse")
library(geosphere)

###################### Bird arrival date map - TAB 2 ###################### 
## load data
arr_master <- readRDS("data_arr.RDS")
arr_master3 <- arr_master2 <- arr_master
colnames(arr_master2)[7] <- c("year2")

## load the picture that corresponds to this combination of this arguments
picplot <- function(year, sps, mod, rang){
  name <- paste(year, sps, mod, rang, sep="_")
  return(name) 
}

###################### green-up map - TAB 3 ###################### 
## load the picture that corresponds to this combination of this arguments
picgreen <- function(year){
  name <- paste(year)
  return(name) 
}

##################  Sensitivuty analyses - TAB 4 ##################
## load maps and plot formating
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
        axis.title.y = element_text(size = rel(1.2), angle = 90),
        axis.title.x = element_text(size = rel(1.2), angle = 00),
        axis.text=element_text(size=12, colour = "black"),
        legend.title=element_text(size=13),
        legend.spacing.y = grid::unit(0.5, "cm"),
        legend.text=element_text(size=rel(1.2)),
        legend.key.height=grid::unit(0.9,"cm"),
        legend.title.align=0.5) +
  xlab('Longitude') +
  ylab('Latitude') +
  labs(fill = 'Sensitivity\n(Days/Day)') +
  geom_path(aes(x = long, y = lat, group = group),
            alpha = 0.4, color = 'black')

rm(worldmap)

TAB <- readRDS("data_sensi.RDS")
colnames(TAB)[1] <- "scien"
colnames(TAB)[17] <- "species"
load("species_Grid.RData")

## sensitivity map
doplot <- function(species) {
  arr_f <- TAB[which(TAB$species == species),]
  
  #min/max for plotting using output data
  MIN <- round((floor((min(arr_f$beta_mean, na.rm = TRUE))*10)/10), 1)
  MAX <- round((ceiling((max(arr_f$beta_mean, na.rm = TRUE))*10)/10), 1)
  
  # get hex grid for species
  cell_grid <- get(paste('cell_grid',species,sep="_"))
  
  #merge hex spatial data with HM data
  to_plt <- dplyr::inner_join(arr_f, cell_grid, by = 'cell')
  
  #pre-IAR
  pp +
    geom_polygon(data = to_plt, aes(x = long, 
                                    y = lat, group = group, 
                                    fill = beta_mean), alpha = 0.5) +
    geom_path(data = to_plt, aes(x = long, 
                                 y = lat, group = group), 
              #alpha = 0.4, 
              color = 'black') + 
    ## Casey's colors
    #scale_fill_gradient2(low = '#C7522B', mid = '#FBF2C4', high = '#3C5941',
    #                     limits = c(MIN, MAX), midpoint = 0)+ 
    scale_fill_viridis(option="magma",limits = c(MIN, MAX)) 
  
}

## Line plot

## all species
qq <- ggplot(data = TAB, aes(x = cell_lat, y = beta_mean, group = species)) + 
  geom_smooth( method = lm, se = FALSE, size=0.8, col="gray") +
  theme_classic() +
  xlab("Latitude (Degrees)") +
  ylab("Sensitivity (Days / Day)") +
  theme(axis.title.y = element_text(size = rel(0.9), angle = 90),
        axis.title.x = element_text(size = rel(0.9), angle = 00),
        axis.text=element_text(size=8, colour = "black")
        #axis.text.y = element_text(angle=90)
  ) 
## add species of interest on top 
doline <- function(species){
  arr_f <- TAB[which(TAB$species == species),]
  qq +  geom_smooth(data = arr_f, aes(x = cell_lat, y = beta_mean),
                    method = lm, se = FALSE, size=0.8, col="black") 
}

################ Interannual variation - TAB 5 ##################
## load data
tt <- readRDS('arr-gr-SVC-sens-data-2020-08-25-centroids.rds')

## file with the center of the species distribuition
tt <- as.data.frame(tt$f_mrg2[,c(1,11,12)])
colnames(tt)[1] <- c("sci_name")

arr_master3 <- left_join(arr_master3,tt, by="sci_name")
cellnumbs <- as.data.frame(cbind(sort(unique(TAB$cell)),
                   seq(1,length(sort(unique(TAB$cell))),1)))
colnames(cellnumbs) <- c("cell","cell2")
arr_master3 <- left_join(arr_master3,cellnumbs, by="cell")
  
f1a_green <- 'indianred'
f1a_bird <- '#2686A0'

## Range map

ran_sp <- arr_master3 

#create hex grid
cell_grid_tab5 <- readRDS("cell_grid_tab5.rds") ## load grid - package not on CRAN

#merge hex spatial data with HM data
ran_sp <- left_join(ran_sp, cell_grid_tab5, by = 'cell')%>%
  transmute(species,
            cell,cell2,
            cell_lat,cell_lng,
            lat,long)

ran_sp3 <- distinct(ran_sp)
ran_sp3$species <- NA
ran_sp3 <- distinct(ran_sp3)

rr <- pp +
  geom_polygon(data = ran_sp3, aes(x = long, y = lat),
  fill="white",
  inherit.aes = FALSE, alpha = 1) +
  #geom_path(data = ran_sp3, 
  #          aes(x = long,y = lat, group = cell), 
  #          inherit.aes = FALSE,
  #          color = 'black') + 
  annotate('text', x = ran_sp3$cell_lng, y = ran_sp3$cell_lat,
           label = ran_sp3$cell2, col = 'black', alpha = 0.9,
           size = 3)

ran_map <- function(species,cel){
  
  ran_sp2 <- ran_sp[which(ran_sp == species),]
  
  #ran_sp$species <- NA
  #ran_sp <- distinct(ran_sp)
  
  #ran_sp <- rbind(ran_sp,ran_sp2)
  
  centercoor <- ran_sp2[which(ran_sp2$cell2 == cel),4:5] 
  centercoor <-  centercoor[1,]
  
   rr +
    geom_path(data = ran_sp2,
              aes(x = long,y = lat, group = cell),
              color="#00BFC4",
              inherit.aes = FALSE, alpha = 0.4) +
    geom_point(data = centercoor, aes(x = cell_lng, y = cell_lat), size = 4, 
                 shape = 21, fill = "deepskyblue4",inherit.aes = FALSE )
}

## Line plot
plot4 <- function(species,cel){
  #   species <- 'Tree Swallow' ; cel <- 59
  sp1 <- arr_master3[which(arr_master3$species == species),]
  sp1 <- sp1[complete.cases(sp1$arr_GAM_mean),]  ## remove rows with GAM mean NA
  sp1_c <- sp1[which(sp1$cell2 == cel),]
  
  # center both arrival and green-up
  sp1_c$std_gr_mn <- scale(sp1_c$gr_mn, scale = FALSE)
  sp1_c$std_arr_IAR_mean <- scale(sp1_c$arr_IAR_mean, scale = FALSE)
  
  sp1_c <- 
  sp1_c %>% 
    transmute(year,     ## seelect this columns
              cell, cell_lng, cell_lat,
              sci_name,
              arrIAR_mean = std_arr_IAR_mean,
              grMn_mean = std_gr_mn,
              grMn_sd = NA,
              arrIAR_sd = arr_IAR_sd
              ) %>%
    pivot_longer(-c(year,cell,sci_name, cell_lng, cell_lat),         ## repeat those (doubles to have groups)
                 names_to=c("series", ".value"), names_sep="_") %>%  ##   for means and legends in ggplot
    mutate(low = mean - sd,
           high = mean + sd,
           series = fct_recode(series,
                               "Green Up" = "grMn",
                               "Bird Arrival" = "arrIAR"
           )) %>% as.data.frame()
    
    ggplot(aes(x=year, y=mean, group=series, color=series), data = sp1_c) +
    geom_line(size = 0.8) +
    geom_point(size = 1.8) +
    #geom_ribbon(aes(ymin=low, ymax=high), alpha=0.1, size=0, fill="red")
    geom_errorbar(aes(year, 
                      ymin = low, 
                      ymax = high), 
                  width = 0.3,# color = f1a_bird#, 
                  alpha = 0.7,
                  data = sp1_c
                  ) +
    scale_color_manual(values=c(f1a_bird, f1a_green)) +
    theme_bw() +
    xlab('Year') +
    ylab('Phenological anomaly') +
    scale_x_continuous(breaks=seq(2002, 2017,2)) +
    labs(x = "Year", 
         y = "Phenological anomaly (Days)",
         color = "Legend") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x= element_text(size=16, margin = margin (t = 14)),
          axis.title.y=element_text(size=16, margin = margin (r = 14)),
          axis.text=element_text(size=12),
          legend.title=element_blank(),
          legend.text=element_text(size=12))
}

## Trait plot
mrg2_xi_PC <- readRDS(file="mrg2_xi_PC.rds")
fit_df <- readRDS(file="fit_df.rds") 

tplo <- ggplot(mrg2_xi_PC, aes(PC1, xi_mean, group= species)) +
  geom_errorbar(aes(ymin = (xi_mean - xi_sd), 
                    ymax = (xi_mean + xi_sd)),
                width = 0, alpha = 0.6) +
  geom_point(alpha = 0.6, size = 2.5) +
  geom_ribbon(data = fit_df, aes(x = sim_PC1, 
                                 ymin = fit_LCI, 
                                 ymax = fit_UCI,
                                 group= NULL),
              alpha = 0.3,
              inherit.aes = FALSE) +
  geom_line(data = fit_df, aes(x = sim_PC1,
                               y = fit_mn,
                               group= NULL), 
            alpha = 0.6, size = 1.5) +
  xlab('PC1') +
  ylab('Sensitivity') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none') 

trait_plot <- function(species){
  
  spstab <- mrg2_xi_PC[which(mrg2_xi_PC$species == species),]
  
  tplo +
    geom_errorbar(spstab, 
                  mapping = aes(ymin = (xi_mean - xi_sd),
                                ymax = (xi_mean + xi_sd)),
                  width = 0, alpha = 0.6,
                  colour = "magenta3") +
    geom_point(spstab,
               mapping = aes(x = PC1, y = xi_mean),
               alpha = 0.9,
               size = 3,
               colour = "magenta3")
}

##### Download functions:

## bird arrival data download - TAB 2

arr_csv <- function(year, species,mod,rang){
  
  arr_master_ARR <- arr_master[which(arr_master$species == species),]
  arr_master_ARR  <- arr_master_ARR [which(arr_master_ARR$year == year),]
  arr_master_ARR  <- arr_master_ARR [which(arr_master_ARR$per_ovr > 0.01),]
  
  if(rang == "bre"){ arr_master_ARR  <- arr_master_ARR[which(arr_master_ARR$breed_cell==TRUE),]}
  if(rang == "mig"){ arr_master_ARR  <- arr_master_ARR[which(arr_master_ARR$mig_cell==TRUE),]}
  
  if(mod == "GAM"){ 
    arr_master_ARR2  <- arr_master_ARR %>%
      transmute(arr_GAM_mean,
                arr_GAM_sd,
                cell_lng,
                cell_lat)
    }
  
  if(mod == "IAR"){ 
    arr_master_ARR2  <- arr_master_ARR %>%
      transmute(arr_IAR_mean,
                arr_IAR_sd,
                cell_lng,
                cell_lat)
  }

  for(i in 1:nrow(arr_master_ARR2)){
    if(is.na(arr_master_ARR2[i,1])){
      arr_master_ARR2[i,2] <- NA
    }
  }
  
  colnames(arr_master_ARR2)[1:2] <- c("posterior_mean","posterior_sd")
   
  return(arr_master_ARR2)
  
}

## green up data download - TAB 3
green_csv <- function(year){
  
  arr_master_GRE <- arr_master[which(arr_master$year == year),] 
  
  arr_master_GRE2 <- arr_master_GRE %>%
    transmute(gr_mn,
              cell_lng,
              cell_lat
    ) 
  
  arr_master_GRE2 <- distinct(arr_master_GRE2)
  
  colnames(arr_master_GRE2)[1] <- c("green_up_mean")
  
  return(arr_master_GRE2)
  
}

## csv file for sensitivity - file is TAB 4
sensi_csv <- function(species){
  
  TAB2 <- TAB[which(TAB$species == species),] 
  
  TAB3 <- TAB2 %>%
    transmute(beta_mean,
              beta_sd,
              cell_lng,
              cell_lat
    ) 
  for(i in 1:nrow(TAB3)){
    if(is.na(TAB3[i,1])){
      TAB3[i,2] <- NA
    }
  }
  return(TAB3)
  
}

## csv file for interannual arrival variation - file is TAB 5
inter_csv <- function(species,cel){
  
  INT <- arr_master3[which(arr_master3$species == species),] 
  
  INT2 <- INT %>%
    transmute(year, 
              cell,
              cell2,
              cell_lng,
              cell_lat,
              arr_IAR_mean,
              gr_mn,
              arr_IAR_sd
    ) 
  
  INT2 <- INT2[which(INT2$cell2 == cel),]
  
  INT2$std_gr_mn <- scale(INT2$gr_mn, scale = FALSE)
  INT2$std_arr_IAR_mean <- scale(INT2$arr_IAR_mean, scale = FALSE)
  
  INT3 <- INT2 %>%
    transmute(year, 
              cell_lng,
              cell_lat,
              std_arr_IAR_mean,
              arr_IAR_sd,
              std_gr_mn
    ) 
  
  for(i in 1:nrow(INT3)){
    if(is.na(INT3[i,4])){
      INT3[i,5] <- NA
    }
  }
  
  colnames(INT3)[4:6] <- c("posterior_mean","posterior_sd","green_up_mean")
  
  return(INT3)
  
}

## csv file for trait plot

trait_csv <- function(species){
  spstab <- mrg2_xi_PC[which(mrg2_xi_PC$species == species),] %>%
    transmute(species,
              xi_mean,
              xi_sd,
              PC1)
  
}


####
## Build the app

shinyApp(
  ui = navbarPage("Migratory Sensitivity", theme = shinytheme("flatly"),
                  
#########################################
                  
                  tabPanel("Introduction",
                           h3("Migratory strategy drives bird sensitivity to spring green-up"),
                           br(),
                           h5("Globally,  animals and plants are shifting the timing of key life events in response to climate change, yet there exists dramatic"),
                           h5("variation in the magnitude of these shifts both within and among species. Such differences have the potential to desynchronize"),
                           h5("species interactions, with consequences for ecosystem functioning. However, despite recent documentation of escalating phenological"),
                           h5("change, scientists lack an understanding of how and why phenological responses vary across space and among species. Here, we use"),
                           h5("data from over 7 million community-contributed bird observations to derive species-specific, spatially-explicit estimates of annual"), 
                           h5("spring migration phenology for 56 species of birds across eastern North America. We show that changes in the spring arrival of"),
                           h5("migratory birds are broadly synchronized with fluctuations in vegetation green-up; however, the sensitivity of birds to plant phenology"),
                           h5("varied extensively. Bird arrival responded more synchronously with vegetation green-up at higher latitudes, where phenological shifts"), 
                           h5("are also greater. Species that migrate more slowly, arrive earlier, and overwinter further north also showed greater responsiveness to"),
                           h5("earlier springs. Identifying how and why phenological responses to global change vary across space and among species will be critical"),
                           h5("to improving predictions of climate impacts and strategizing conservation efforts. Efforts such as these are much needed for North"), 
                           h5("American migratory birds, which have undergone substantial declines over the last several decades."),
                           br(),
                           wellPanel(
                             helpText(
                               a("Click here to check the full the paper",
                                 href="https://doi.org/10.1098/rstb.2011.0059",
                                 target="_blank")
                             )
                           )
                    
                  ),

########################################                  
                  tabPanel("Bird Arrival", 
                           sidebarLayout(
                             sidebarPanel(#width = 4,
                               #fluidPage(theme = shinytheme("cerulean"),                                               ## size that this column will occupy
                               selectInput("sps",                               ## data that will be entered
                                           label = "Choose a species to display:",
                                           choices = sort(unique(arr_master$species)),     ## sps to choose from
                                           selected = arr_master$species[[3436]]),       ## which one will be selected when first launch the app
                               
                               downloadLink('downloadData1', 'Click here to download data',class = "butt"),
                               tags$head(tags$style(".butt{background-color:lightgray;} .butt{color: black;}")), # background color and font color
                               
                               br(),  ## blank rows between menus 
                               br(),
                               
                               radioButtons("mod",
                                            "Model:",
                                            choices = list("GAM model (local estimation)"="GAM",
                                                           "IAR model (spatially smooth)"="IAR"),
                                            selected = "GAM"),
                               
                               radioButtons("rang",
                                            "Species distribuition:",
                                            choices = list("Breeding range"="bre",
                                                           "Migration range"="mig",
                                                           "Breeding and migration range" = "both"),
                                            selected = "both"),
                               ## size that this column will occupy 
                               sliderInput("year",                                   ## data that will be entered
                                           "Year:",                                  ## title
                                           min = min(arr_master$year),               ## limits of the sliderbar
                                           max = max(arr_master$year),
                                           step = 1,                                 ## interval of sliderbar unit
                                           value = min(arr_master$year),             ## first value to be displayd when launch the app
                                           animate=T,                                ## add the animation
                                           sep = "")                                 ## remove comma from year number
                               #)
                               
                             ),
                             mainPanel(
                               imageOutput("mapapic")
                             )
                           )
                  ),
                  
##########################################
                  tabPanel("Green-up", 
                           
                           sidebarLayout(
                             sidebarPanel(width = 4,
                               #fluidPage(theme = shinytheme("cerulean"),                                               ## size that this column will occupy
                               
                               sliderInput("year2",                                   ## data that will be entered
                                           "Year:",                                  ## title
                                           min = min(arr_master2$year2),               ## limits of the sliderbar
                                           max = max(arr_master2$year2),
                                           step = 1,                                 ## interval of sliderbar unit
                                           value = min(arr_master2$year2),           ## first value to be displayd when launch the app
                                           animate=T,                                ## add the animation
                                           sep = ""),
                               
                               downloadLink('downloadData2', 'Click here to download data',class = "butt"),
                               tags$head(tags$style(".butt{background-color:lightgray;} .butt{color: black;}")), # background color and font color
                               
                               br(),  ## blank rows between menus 
                               br(),
                               
                               h6("Estimates of spring vegetation phenology ('green-up') obtained from the 
                                  Moderate Resolution Imaging Spectroradiometer (MODIS) Land Cover Dynamics 
                                  (MCD12Q2) Version 6 data product (https://lpdaac.usgs.gov/products/mcd12q2v006/). 
                                  Values shown are within-cell averages of 'midgreen-up' dates, for all pixels classified as 
                                  forest, according to the MODIS Land Cover Type (MCD12Q1) Version 6 data product
                                  (https://lpdaac.usgs.gov/products/mcd12q1v006/).",style = "font-size:12px;")
                               
                               #)
                             ),
                             mainPanel(
                               imageOutput("green") #,
                               #h2("ha!")
                             )
                           )
                  ),
                  
########################################
                  tabPanel("Interannual Variation", 
                           #sidebarLayout(
                           # sidebarPanel(
                           #fluidPage(theme = shinytheme("cerulean"),                                               ## size that this column will occupy
                           fluidRow(
                             column(3,
                                    selectInput("sps3",                               ## data that will be entered
                                                label = "Choose a species to display:",
                                                choices = sort(unique(arr_master3$species)),     ## sps to choose from
                                                selected = "Tree Swallow"),       ## which one will be selected when first launch the app
                             ),
                             column(3, 
                                    
                                    selectInput("cell", 
                                                label = "Choose a cell to display (range in blue)",
                                                choices = cellnumbs$cell2,
                                                selected = 59)
                             ),
                             
                             column(3, 
                                    br(),
                                    downloadLink('downloadData4', 'Click here to download data',class = "butt"),
                                    tags$head(tags$style(".butt{background-color:lightgray;} .butt{color: black;}")) # background color and font color
                             ),
                           ),
                           
                           mainPanel(
                             splitLayout(cellWidths = c("60%", "90%"),
                                         plotOutput("range1"),
                                         plotOutput("plot4"))
                           )
                           #)
                           
                  ),
############################################                  
                  tabPanel("Sensitivity across Latitude", 
                           #sidebarLayout(
                           #   sidebarPanel(
                           #fluidPage(theme = shinytheme("cerulean"),                                               ## size that this column will occupy
                           fluidRow(column(12,
                                            h5("Spatial variation in sensitivity of bird arrival to green-up, where sensitivity is 
                                                  defined as the magnitude of change in bird arrival (in days) for every one day change 
                                                  in green-up."),
                                            style="color:black",align = "left")
                           ),
                           
                           fluidRow(column(12,
                                           h5(" At left, the cell-specific posterior mean estimates of sensitivity. Cells 
                                                  are gridded over the portion of the species' migratory and breeding ranges that fell 
                                                  within the study area. At right, sensitivity as a function of latitude, with each line 
                                                  representing a different species. The bold line corresponds to the species presented 
                                                  at left."),
                                           style="color:black",align = "left")
                           ),
                           
                           br(),
                           
                           column(4, selectInput("sps2", 
                                                 label = "Choose a species to display",
                                                 choices = sort(unique(TAB$species)),
                                                 selected = TAB$species[[583]])#,
                                  #br(),
                           ),  
                           
                           column(7,   
                                  br(),
                                  br(),
                                  #downloadButton('downloadData4', "Download")
                                  downloadLink('downloadData3', 'Click  here  to download data',class = "butt"),
                                  tags$head(tags$style(".butt{background-color:lightgray;} .butt{color: black;}")) # background color and font color)
                                  
                           ), 
                           column(7,
                                  h6(em("* Put the cursor on top of the lines to find out which species they belong to")),
                                  style="color:gray",align = "right"),
                     
                          
                           fluidRow(
                             mainPanel(
                               splitLayout(cellWidths = c("90%", "50%"), 
                                           plotOutput("distPlot"),
                                           plotlyOutput("plot")))
                             # )
                             #)
                           )#,
                           
                           #fluidRow( br(),
                                     
                           #          column(6,
                           #                h6(em("."),style="color:white")
                            #         ),
                             #        column(6,
                              #              h6(em("* Put the cursor on top of the lines to find out which species they belong to")),
                               #             style="color:gray",align = "right")
                                #    )
                           

                  ),

######################################

                  tabPanel("Migratory traits",
                           fluidRow(column(12,
                                          h5("Species-level sensitivity as a function of migratory traits."),
                                            style="color:black",align = "left")
                           ),
                           
                           fluidRow(column(12,
                                           h5("Correlated traits were combined 
                                             into a single principal component axis, PC1, for which negative values are associated with 
                                             slower migration pace, earlier arrival dates, and more northerly overwinter latitudes. Each 
                                             point represents one species. Error bars represent one posterior standard deviation. The black
                                             line represents the mean relationship between species-level sensitivity and the migratory 
                                             traits of interest, while the gray ribbon denotes the 95% credible interval."),
                                           style="color:black",align = "left")
                           ),
                           
                           br(),
                           
                           sidebarLayout(
                             sidebarPanel(#width = 4,
                               selectInput("sps4",                               ## data that will be entered
                                           label = "Choose a species to display:",
                                           choices = sort(unique(mrg2_xi_PC$species)),     ## sps to choose from
                                           selected = mrg2_xi_PC$species[[34]]),       ## which one will be selected when first launch the app
                               
                               downloadLink('downloadData5', 'Click here to download data',class = "butt"),
                               tags$head(tags$style(".butt{background-color:lightgray;} .butt{color: black;}")),
                               
                               br(),
                               
                               h5(em("* Put the cursor on top of the points to find out which species they belong to"))
                            ),
                            
                           mainPanel(
                             plotlyOutput("plotTrait")
                           )
                          )
                          
                  ),
                  
                  ######################################
                  
                  tabPanel("How to cite these data",
                           br(),
                           h5("Globally,  animals and plants are shifting the timing of key life events in response to climate change, yet there exists dramatic"),
                           h5("variation in the magnitude of these shifts both within and among species. Such differences have the potential to desynchronize"),
                           br(),
                      
                  )

########################################  
                  
  ),
  
  server = function(input, output) { 
    
  ## arrival date TAB 2
    output$mapapic <- renderImage({
      # When input$n is 1, filename is ./images/image1.jpe
      name <- picplot(input$year, input$sps, input$mod, input$rang)
      filename <- normalizePath(file.path('./images',
                                          paste(name, '.png', sep='')))
      
      # Return a list containing the filename
      list(src = filename,
           height = 199*2.5,
           width = 244*2.5
      )
    }, deleteFile = FALSE)
    
      output$downloadData1 <- downloadHandler(
      filename = function() {
        name1 <- sub(" ","",input$sps)  
        if(input$mod == "GAM") {
          name_part <- "_local-estimator"} else {
            name_part <- "_spatially-smooth"
          }
        paste(input$year, name1,"_", input$rang, name_part, "_ArrivalDate.csv", sep="")
      },
      content = function(file) {
        arrtab <- arr_csv(input$year, input$sps, input$mod, input$rang)
        write.csv(arrtab, file)
      })
    
  ## green up TAB 3
    output$green <- renderImage({
      # When input$n is 1, filename is ./images/image1.jpeg
      name2 <- picgreen(input$year2)
      filename <- normalizePath(file.path('./images',
                                          paste(name2, '.png', sep='')))
      
      # Return a list containing the filename
      list(src = filename,
           height = 199*2.5,
           width = 244*2.5
      )
    }, deleteFile = FALSE)
    
    output$downloadData2 <- downloadHandler(
      filename = function() {
        paste(input$year2, "_greenup.csv", sep="")
      },
      content = function(file) {
        gretab <- green_csv(input$year2)
        write.csv(gretab, file)
      })
  
  ## sensitibity TAB 4
    output$distPlot <- renderPlot({
      doplot(input$sps2)
    })
    
  #  nms <- unique(TAB$species)
    
    output$plot <- renderPlotly({
      p <- doline(input$sps2)
      ggplotly(p, tooltip = c("species"))
      
    })
    
    output$downloadData3 <- downloadHandler(
      filename = function() {
        name3 <- sub(" ","",input$sps2)       ## remove blank space in sps name
        paste(name3, "_sensi.csv", sep="")
      },
      content = function(file) {
        sentab <- sensi_csv(input$sps2)
        write.csv(sentab, file)
      })
    
    ## interannual arrival var
    output$range1 <- renderPlot({
      ran_map(input$sps3,input$cell)
    })
    
    output$plot4 <- renderPlot({
      plot4(input$sps3,input$cell)
    })
    
    output$downloadData4 <- downloadHandler(
      filename = function() {
        name4 <- sub(" ","",input$sps3)       ## remove blank space in sps name
        paste(name4,input$cell, "_interan.csv", sep="")
      },
      content = function(file) {
        inttab <- inter_csv(input$sps3,input$cell)
        write.csv(inttab, file)
      })
    
    ## trait plot
    
    output$plotTrait <- renderPlotly({
      t <- trait_plot(input$sps4)
      ggplotly(t, tooltip = c("species")
               )
      })
    
    output$downloadData5 <- downloadHandler(
      filename = function() {
        name4 <- sub(" ","",input$sps4)       ## remove blank space in sps name
        paste(name4, "_trait.csv", sep="")
      },
      content = function(file) {
        trtab <- trait_csv(input$sps4)
        write.csv(trtab, file)
      })
  }
)