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
library(shinyBS)
library(zip)

##  Bird arrival date map - TAB 2 ----------------------------
## load data
arr_master <- readRDS("Data/data_arr.RDS")

cellnumbs <- as.data.frame(cbind(sort(unique(arr_master$cell)),
                                 seq(1,length(sort(unique(arr_master$cell))),1)))
colnames(cellnumbs) <- c("cell","cell2")
arr_master <- left_join(arr_master,cellnumbs, by="cell") %>% 
  select(-cell) %>% 
  rename(cell = cell2)

arr_master3 <- arr_master2 <- arr_master
arr_master2 <- arr_master2 %>% 
  rename(year2 = year)

## load the picture that corresponds to this combination of this arguments
picplot <- function(year, sps, mod, rang){
  name <- paste(year, sps, mod, rang, sep="_")
  return(name) 
}

##  Green-up map - TAB 3 ----------------------------
## load the picture that corresponds to this combination of this arguments
picgreen <- function(year){
  name <- paste(year)
  return(name) 
}

##  Sensitivity analyses - TAB 4 ----------------------------
# load maps and plot formatting
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
        legend.title=element_text(size=13),
        legend.spacing.y = grid::unit(0.5, "cm"),
        legend.text=element_text(size=rel(1.2)),
        legend.key.height=grid::unit(0.9,"cm"),
        legend.title.align=0.5,
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab('Longitude') +
  ylab('Latitude') +
  labs(fill = 'Sensitivity\n(Days/Day)') +
  geom_path(aes(x = long, y = lat, group = group),
            alpha = 0.4, color = 'black')

rm(worldmap)

TAB <- readRDS("Data/data_sensi.RDS")
TAB <- left_join(TAB,cellnumbs, by="cell") %>% 
  select(-cell) %>% 
  rename(cell = cell2)

load("Data/species_Grid.RData")

# sensitivity map
doplot <- function(species) {
  arr_f <- TAB[which(TAB$species == species),]
  
  #min/max for plotting using output data
  MIN <- round((floor((min(arr_f$beta_mean, na.rm = TRUE))*10)/10), 1)
  MAX <- round((ceiling((max(arr_f$beta_mean, na.rm = TRUE))*10)/10), 1)
  
  # get hex grid for species
  cell_grid <- get(paste('cell_grid', species, sep="_")) 
  
  #merge hex spatial data with HM data
  to_plt <- dplyr::inner_join(arr_f, cell_grid, by = 'cell')
  
  pp +
    geom_polygon(data = to_plt, aes(x = long, 
                                    y = lat, group = group, 
                                    fill = beta_mean), alpha = 0.5) +
    geom_path(data = to_plt, aes(x = long, 
                                 y = lat, group = group), 
              #alpha = 0.4, 
              color = 'black') + 
    scale_fill_viridis(option="magma",limits = c(MIN, MAX)) 
  
}

# Line plot
# all species
sensim_df <- readRDS('Data/fit_df_tab5.rds')
qq <- ggplot(sensim_df, aes(lat, sensim, group = species)) +
  geom_line(
    size = 1, col = "gray"
  ) +
  theme_classic() +
  xlab("Latitude (Degrees)") +
  ylab("Sensitivity (Days / Day)") +
  theme(axis.title.y = element_text(size = rel(0.9), angle = 90, margin = margin(r = 10)),
        axis.title.x = element_text(size = rel(0.9), angle = 00),
        axis.text=element_text(size=8, colour = "black")
        #axis.text.y = element_text(angle=90)
  ) 
# add species of interest on top 
doline <- function(species){
  sensim_df_f <- sensim_df[which(sensim_df$species == species),]
  qq + geom_line(data = sensim_df_f, 
                 #alpha = 0.8,
                 size = 1.1,
                 color = 'black')
}

##  Interannual variation - TAB 4 ----------------------------
f1a_green <- 'indianred'
f1a_bird <- '#2686A0'

## Range map
ran_sp <- arr_master3 

#create hex grid
cell_grid_tab4 <- readRDS("Data/master_cell_grid.rds") ## load grid - package not on CRAN
for_gr <- readRDS('Data/for_green-up_dl.rds')

for_gr2 <- left_join(for_gr, cellnumbs, by="cell") %>% 
  select(-cell) %>% 
  rename(cell = cell2)

#merge hex spatial data with HM data
ran_sp <- left_join(ran_sp, cell_grid_tab4, by = 'cell')%>%  
  transmute(species,
            cell,
            cell_lat,cell_lng,
            lat,long)

ran_sp3 <- distinct(ran_sp)
ran_sp3$species <- NA
ran_sp3 <- distinct(ran_sp3)

rr <- pp +
  geom_polygon(data = cell_grid_tab4, aes(x = long, y = lat),
               fill="white",
               inherit.aes = FALSE, alpha = 1) +
  geom_path(data = cell_grid_tab4,
            aes(x = long,y = lat, group = cell),
            inherit.aes = FALSE,
            color = 'black', alpha = 0.2) +
  annotate('text', x = ran_sp3$cell_lng, y = ran_sp3$cell_lat,
           label = ran_sp3$cell, col = 'black', alpha = 0.9,
           size = 3)

ran_map <- function(species,cell){
  
  ran_sp2 <- ran_sp[which(ran_sp == species),]
  
  centercoor <- ran_sp2[which(ran_sp2$cell == cell),3:4] 
  centercoor <-  centercoor[1,]
  
  rr +
    geom_path(data = ran_sp2,
              aes(x = long,y = lat, group = cell),
              color="goldenrod",
              inherit.aes = FALSE, alpha = 0.8, size = 1.1) +
    geom_point(data = centercoor, aes(x = cell_lng, y = cell_lat), size = 4, 
               shape = 21, fill = f1a_bird, inherit.aes = FALSE )
}

## Line plot
plot4 <- function(sp,cel){
  #sp <- 'Tree Swallow' ; cel <- 36  #;  cel <- 52  ;  cel <- 1
  #filter bird data
  sp1 <- dplyr::select(arr_master3,
                       species,
                       year,
                       arr_GAM_mean,
                       arr_IAR_mean,
                       arr_IAR_sd,
                       cell) %>%
    dplyr::filter(cell == cel,
                  species == sp,
                  !is.na(arr_GAM_mean))
  
  #filter forest data
  for1 <- dplyr::filter(for_gr2, cell == cel)
  
  #valid bird years
  vby <- sp1$year
  if (length(vby) < 1)
  {
    for1$sc_gr <- scale(for1$gr_mn, scale = FALSE)[,1]
  } else {
    tgr <- dplyr::filter(for1, year %in% vby)
    mean_tgr <- mean(tgr$gr_mn)
    for1$sc_gr <- for1$gr_mn - mean_tgr
  }
  
  #join gr and bird data
  sp1_t <- dplyr::left_join(for1, sp1, by = c('cell', 'year'))
  sp1_t$sc_arr_IAR_mean <- scale(sp1_t$arr_IAR_mean, scale = FALSE)[,1]
  
  #create values for error bars
  sp1_pt <- dplyr::mutate(sp1_t, low = sc_arr_IAR_mean - arr_IAR_sd,
                          high = sc_arr_IAR_mean + arr_IAR_sd)
  
  cols <- c("Bird Arrival"= f1a_bird,"Green-up" = f1a_green)
  ggplot(aes(x=year, y=sc_arr_IAR_mean, col = 'Bird Arrival'), #group=series, color=series), 
         data = sp1_pt) +
    geom_line(size = 0.8, alpha = 0.8) +
    geom_point(size = 1.8, alpha = 0.8) +
    #geom_ribbon(aes(ymin=low, ymax=high), alpha=0.1, size=0, fill="red")
    geom_errorbar(aes(year, 
                      ymin = low, 
                      ymax = high),
                  width = 0.3, 
                  alpha = 0.7,
                  data = sp1_pt
    ) +
    geom_line(aes(x=year, y=sc_gr, col = 'Green-up'), size = 0.8, alpha = 0.8) +
    geom_point(aes(x=year, y=sc_gr, col = 'Green-up'), size = 1.8, alpha = 0.8) +
    scale_color_manual(values = cols) +
    theme_bw() +
    xlab('Year') +
    ylab('Phenological anomaly') +
    scale_x_continuous(breaks=seq(2002, 2017, 2)) +
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

##  Migratory traits - TAB 6 ----------------------------
mrg2_xi_PC <- readRDS("Data/mrg2_xi_PC.rds")
mrg2_xi_PC <- mrg2_xi_PC[,1:4]
colnames(mrg2_xi_PC)[1] <- "species"
nm_key <- unique(arr_master3[,c('species', 'sci_name')])
#join sci name
mrg2_xi_PC <- dplyr::left_join(mrg2_xi_PC, nm_key, by = 'species')
fit_df <- readRDS(file="Data/fit_df_tab6.rds") 

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
  xlab('Migratory Trait Score (PC 1)') +
  ylab('Species-level Sensitivity (days/day)') +
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

##  Download functions: ----------------------------

#bird arrival data download - TAB 2

arr_csv <- function(species1, mod, rang){
  
  arr_master_ARR <- dplyr::filter(arr_master, 
                                  species == species1,
                                  per_ovr >= 0.05)
  
  if(rang == "bre"){ arr_master_ARR  <- arr_master_ARR[which(arr_master_ARR$breed_cell==TRUE),]}
  if(rang == "mig"){ arr_master_ARR  <- arr_master_ARR[which(arr_master_ARR$mig_cell==TRUE),]}
  
  if(mod == "GAM"){ 
    arr_master_ARR2  <- arr_master_ARR %>%
      transmute(species,
                sci_name,
                year,
                cell,
                cell_lat,
                cell_lng,
                mod = mod,
                posterior_mean = round(arr_GAM_mean, 2),
                posterior_sd = round(arr_GAM_sd, 2))
  }
  
  if(mod == "IAR"){ 
    arr_master_ARR2  <- arr_master_ARR %>%
      transmute(species,
                sci_name,
                year,
                cell,
                cell_lat,
                cell_lng,
                mod = mod,
                posterior_mean = round(arr_IAR_mean, 2),
                posterior_sd = round(arr_IAR_sd, 2))
  }
  
  for(i in 1:nrow(arr_master_ARR2)){
    if(is.na(arr_master_ARR2[i,'posterior_mean'])){
      arr_master_ARR2[i,'posterior_sd'] <- NA
    }
  }
  
  return(arr_master_ARR2)
  
}

# green up data download - TAB 3
green_csv <- function(){
  
  t_gr <- readRDS('Data/for_green-up_dl.rds')
  gr2 <- left_join(t_gr, cellnumbs, by = "cell") %>% 
    select(-cell) %>% 
    rename(cell = cell2) %>%
    transmute(year,
              cell,
              cell_lat = round(cell_lat, 2),
              cell_lng = round(cell_lng, 2),
              gr_mn,
              gr_ncell)
  
  return(gr2)
  
}

# csv file for sensitivity - file is TAB 5
sensi_csv <- function(species1){
  
  TAB2 <- TAB[which(TAB$species == species1),] 
  
  TAB3 <- TAB2 %>%
    transmute(species,
              sci_name,
              cell,
              cell_lat,
              cell_lng,
              beta_mean = round(beta_mean, 3),
              beta_sd = round(beta_sd, 3))
  
  return(TAB3)
  
}

# csv file for interannual arrival variation - file is TAB 4
inter_csv <- function(species1,cel){
  
  INT <- arr_master3 %>% 
    dplyr::filter(species == species1 &
                    cell == cel)
  na_idx <- which(is.na(INT$arr_GAM_mean))
  INT$arr_IAR_mean[na_idx] <- NA
  INT$arr_IAR_sd[na_idx] <- NA
  
  INT3 <- INT %>%
    transmute(species,
              sci_name,
              year, 
              cell,
              cell_lat,
              cell_lng,
              arr_IAR_mean = round(arr_IAR_mean, 2),
              arr_IAR_sd = round(arr_IAR_sd, 2),
              gr_mn)
  
  return(INT3)
  
}

# csv file for trait plot
trait_csv <- function(){
  spstab <- mrg2_xi_PC %>%
    transmute(species,
              sci_name,
              xi_mean = round(xi_mean, 3),
              xi_sd = round(xi_sd, 3),
              PC1 = round(PC1, 3))
}

## Create radio tooltip to explain species distributions ----------------------------
radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            opts = $.extend(", options, ", {html: true});
            $(this.parentElement).tooltip('destroy');
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}


##  APP ----------------------------
shinyApp(
  ui = navbarPage("Migratory Sensitivity", theme = shinytheme("flatly"),
                  
                  ##  Introduction  ----------------------------               
                  tabPanel("Introduction",
                           h3("Data Visualization for: Migratory strategy drives species-level variation in bird sensitivity to vegetation green-up"),
                           br(),
                           h5("Animals and plants are shifting the timing of key life events in response to climate change, yet despite recent documentation of escalating "),
                           h5("phenological change, scientists lack a full understanding of how and why phenological responses vary across space and among species. "),
                           h5("Here, we used over 7 million community-contributed bird observations from eBird to derive species-specific, spatially-explicit estimates of "),
                           h5("annual spring migration phenology for 56 species of birds across eastern North America. We show that changes in the spring arrival of "),
                           h5("migratory birds are coarsely synchronized with fluctuations in vegetation green-up and that the sensitivity of birds to plant phenology varied  "),
                           h5("extensively. Bird arrival responded more synchronously with vegetation green-up at higher latitudes, where phenological shifts over time  "),
                           h5("are also greater. Critically, species’ migratory traits explained variation in sensitivity to green-up, with species that migrate more slowly,  "),
                           h5("arrive earlier, and overwinter further north showing greater responsiveness to earlier springs. Identifying how and why species vary in their  "),
                           h5("ability to shift phenological events is fundamental to predicting species’ vulnerability to climate change. Such variation in sensitivity across  "),
                           h5("taxa, with long-distance neotropical migrants exhibiting reduced synchrony, may help to explain substantial declines in these species  "),
                           h5("over the last several decades. "),
                           br(),
                           h4(em("Citation:")),
                           h5("Casey Youngflesh, Jacob Socolar, Bruna R. Amaral, Ali Arab, Robert P. Guralnick, Allen H. Hurlbert, Raphael LaFrance, Stephen J. Mayor,"),
                           h5("David A. W. Miller, and Morgan W. Tingley. 2021. Migratory strategy drives species-level variation in bird sensitivity to vegetation green-up. "
                           ),
                           #h5("Nature Ecology and Evolution.",
                           uiOutput("tab"),
                           br(),
                           h4(em("How to use this website:")),
                           h5("The following tabs provide data visualization and exploration of the results reported in the above
                              paper. Individual results are also available for download."),
                           h5("For questions of interpretation or
                              methods, please see the full text referenced above. For any data re-use, please cite the above
                              publication."),
                           br()),
                  
                  ##  ui Bird Arrival  ----------------------------                  
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
                                            choices = list("IAR model (spatially smooth)"="IAR",
                                                           "GAM model (local estimation)"="GAM"),
                                            selected = "IAR"),
                               
                               radioButtons("radioSelection",
                                            "Species distribution:",
                                            choices = list("Breeding range"="bre",
                                                           "Migratory range"="mig",
                                                           "Breeding and migratory range" = "both"),
                                            selected = "both"),
                               #tags$div(title="Breeding range",verbatimTextOutput("Text")),
                               
                               radioTooltip(id = "radioSelection", choice = "bre", title = "The portion of the species’ range where breeding occurs", placement = "right", trigger = "hover"),
                               radioTooltip(id = "radioSelection", choice = "mig", title = "The portion of the species’ range where migration occurs", placement = "right", trigger = "hover"),
                               radioTooltip(id = "radioSelection", choice = "both", title = "Combination of all areas where the species breeds and/or migrates (but does not winter)", placement = "right", trigger = "hover"),
                               
                               ## size that this column will occupy   
                               sliderInput("year",                                   ## data that will be entered
                                           "Year:",                                  ## title
                                           min = min(arr_master$year),               ## limits of the sliderbar
                                           max = max(arr_master$year),
                                           step = 1,                                 ## interval of sliderbar unit
                                           value = 2013, #max(arr_master$year),             ## first value to be displayd when launch the app
                                           animate = animationOptions(loop = TRUE), #T,                                ## add the animation
                                           sep = ""),                                 ## remove comma from year number
                               #)
                               tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
                               
                             ),
                             mainPanel(
                               imageOutput("mapapic")
                             )
                           )
                  ),
                  
                  ##  ui Green-up  ----------------------------
                  tabPanel("Green-up", 
                           
                           sidebarLayout(
                             sidebarPanel(width = 4,
                                          #fluidPage(theme = shinytheme("cerulean"),                                               ## size that this column will occupy
                                          
                                          sliderInput("year2",                                   ## data that will be entered
                                                      "Year:",                                  ## title
                                                      min = min(arr_master2$year2),               ## limits of the sliderbar
                                                      max = max(arr_master2$year2),
                                                      step = 1,                                 ## interval of sliderbar unit
                                                      value = 2006, #min(arr_master2$year2),           ## first value to be displayd when launch the app
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
                  
                  ##  ui Interannual Variation  ----------------------------
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
                                                label = "Choose a cell to display (range in gold):",
                                                choices = "",
                                                selected = "")
                             ),
                             
                             column(3, 
                                    br(),
                                    downloadLink('downloadData3', 'Click here to download data',class = "butt"),
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
                  ##  ui Sensitivity Across Latitude ----------------------------      
                  tabPanel("Sensitivity Across Latitude", 
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
                                                 choices = sort(unique(sensim_df$species)),
                                                 selected = sensim_df$species[[2]])#,
                                  #br(),
                           ),  
                           
                           column(7,   
                                  br(),
                                  br(),
                                  downloadLink('downloadData4', 'Click  here  to download data',class = "butt"),
                                  tags$head(tags$style(".butt{background-color:lightgray;} .butt{color: black;}")) # background color and font color)
                                  
                           ), 
                           column(7,
                                  h6(em("* Put the cursor on top of the lines to find out which species they belong to")),
                                  style="color:gray",align = "right"),
                           
                           
                           fluidRow(
                             mainPanel(
                               splitLayout(cellWidths = c("700", "420"), 
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
                  
                  ##  ui Migratory Traits  ----------------------------
                  tabPanel("Migratory Traits",
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
                               fluidRow(column(10, align="center",
                                               plotlyOutput("plotTrait"))),
                               fluidRow(column(10, align="center",
                                               imageOutput("arrow") 
                               ))
                             )
                           )
                  )
  ),
  ##  SERVER  ----------------------------
  server = function(input, output, session) { 
    ## link to paper
    url <- a("DOI:10.1038/s41559-021-01442-y.", href="https://dx.doi.org/10.1038/s41559-021-01442-y")
    output$tab <- renderUI({
      tagList("Nature Ecology and Evolution. ", url)})
    
    ## server arrival date Tab 1 ----------------------------
    output$mapapic <- renderImage({
      # When input$n is 1, filename is ./images/image1.jpeg
      name <- picplot(input$year, input$sps, input$mod, input$radioSelection)
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
          name_part <- "_GAM"} else {
            name_part <- "_IAR"
          }
        paste(name1, '_', input$radioSelection, name_part, "_ArrivalDate.zip", sep="")
      },
      content = function(fname) {
        
        tabd1 <- arr_csv(input$sps, input$mod, input$radioSelection)
        write.csv(tabd1, file = "data_arrival.csv", row.names = FALSE)
        fs <- c("data_arrival.csv", "read_me_arrival.txt")
        zip(zipfile = fname, files = fs)
      },
      contentType = "application/zip"
    )    
    
    ## server green up Tab 2 ----------------------------
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
        "greenup.zip"
      },
      content = function(file) {
        
        gretab <- green_csv()
        write.csv(gretab, file = "data_greenup.csv", row.names = FALSE)
        fs <- c("data_greenup.csv", "read_me_greenup.txt")
        zip(zipfile = file, files = fs)
      },
      contentType = "application/zip"
    )
    
    ## server interannual arrival var Tab 3 ----------------------------
    observeEvent(
      input$sps3,
      updateSelectInput(session, "cell", "Choose a cell to display (range in gold):", 
                        choices = arr_master3$cell[arr_master3$species==input$sps3]))
    
    output$range1 <- renderPlot({
      ran_map(input$sps3,input$cell)
    })
    
    output$plot4 <- renderPlot({
      
      plot4(input$sps3,input$cell)
    })
    
    output$downloadData3 <- downloadHandler(
      filename = function() {
        name4 <- sub(" ","",input$sps3)       ## remove blank space in sps name
        paste(name4, '_', input$cell, "_interan.zip", sep="")
      },
      content = function(file) {
        inttab <- inter_csv(input$sps3,input$cell)
        write.csv(inttab, file = "data_interan.csv", row.names = FALSE)
        fs <- c("data_interan.csv", "read_me_interan.txt")
        zip(zipfile = file, files = fs)
      },
      contentType = "application/zip"
    )    
    ## server sensitivity Tab 4 ----------------------------
    output$distPlot <- renderPlot({
      doplot(input$sps2)
    })
    
    #  nms <- unique(TAB$species)
    
    output$plot <- renderPlotly({
      p <- doline(input$sps2)
      ggplotly(p, tooltip = c("species"))
      
    })
    
    output$downloadData4 <- downloadHandler(
      filename = function() {
        name3 <- sub(" ","",input$sps2)       ## remove blank space in sps name
        paste(name3, "_sensi.zip", sep = "")
      },
      content = function(file) {
        sentab <- sensi_csv(input$sps2)
        write.csv(sentab, file  = "data_sensi.csv", row.names = FALSE)
        fs <- c("data_sensi.csv", "read_me_sensi.txt")
        zip(zipfile = file, files = fs)
      },
      contentType = "application/zip"
    )
    
    ## server trait plot Tab 5 ----------------------------
    
    output$plotTrait <- renderPlotly({
      t <- trait_plot(input$sps4)
      ggplotly(t, tooltip = c("species")
      )
    })
    
    output$downloadData5 <- downloadHandler(
      filename = function() {
        'traits.zip'
      },
      content = function(file) {
        trtab <- trait_csv()
        write.csv(trtab, file = "data_trait.csv", row.names = FALSE)
        fs <- c("data_trait.csv", "read_me_trait.txt")
        zip(zipfile = file, files = fs)
      },
      contentType = "application/zip"
    )  
    
    output$arrow <- renderImage({
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path('./www',
                                          'pc1arrow2.png'))
      
      # Return a list containing the filename
      list(src = filename#,
           #height = 100,
           #width = 600
      )
    }, deleteFile = FALSE)
    
  }
)
