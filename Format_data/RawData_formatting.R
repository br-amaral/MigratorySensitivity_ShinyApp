## Data formating for files sent by Casey

## format data of arrival data

arr_master <- readRDS("Data/Raw/pheno-data-2020-08-25.rds")

## create column for tipe of cell
##   both = 3, breeding = 2, migration = 1

for(i in 1:nrow(arr_master)){
  
  if(arr_master$mig_cell[i] == TRUE & arr_master$breed_cell[i] == TRUE) {
    arr_master$cell_type[i] <- 3
  }
  if(arr_master$mig_cell[i] == FALSE & arr_master$breed_cell[i] == TRUE) {
    arr_master$cell_type[i] <- 2
  }
  if(arr_master$mig_cell[i] == TRUE & arr_master$breed_cell[i] == FALSE) {
    arr_master$cell_type[i] <- 1
  }
  if(arr_master$mig_cell[i] == FALSE & arr_master$breed_cell[i] == FALSE) {
    arr_master$cell_type[i] <- 0
  }
}

## change colnames for species names and add species names as the drop out menu in the shiny app
colnames(arr_master)[1] <- "sci_name"
specieskey <- read.csv("Data/Raw/species_reference.csv")

specieskey2 <- dplyr::select(specieskey, NoQuotes, PRIMARY_COM_NAME)
colnames(specieskey2) <- c("sci_name","species")
arr_master2 <- dplyr::left_join(arr_master,specieskey2,by = "sci_name")

saveRDS(arr_master2, file = "Data/data_arr.RDS")

## format sensitivity data
sensi <- readRDS("Data/Raw/arr-gr-SVC-sens-psummary-2020-08-25.rds")

## change colnames for species names and add species names as the drop out menu in the shiny app
colnames(sensi)[1] <- "sci_name"
sensi2 <- left_join(sensi,specieskey2,by = "sci_name")

sensi3 <- dplyr::select(sensi2, sci_name, species, cell, cell_lat, cell_lng, 
                        beta_mean, beta_sd, xi_mean, xi_sd, gamma_mean, gamma_sd)
saveRDS(sensi3, file = "Data/data_sensi.RDS")


##############
## sens plots


# load data ---------------------------------------------------------------

psummary <- sensi2
fit_subset <- readRDS('Data/Raw/arr-gr-SVC-sens-stan-output-subset-2020-08-25.rds')
DATA <- readRDS('Data/Raw/arr-gr-SVC-sens-data-2020-08-25.rds')

usp <- unique(psummary$species)

#get points for species-specific slopes
#number of lat to sim
L <- 10
#species-level lines
sensim_df <- data.frame(species = rep(NA, L * length(usp)),
                        slat = NA,
                        lat = NA,
                        sensim = NA)
counter <- 1
for (i in 1:length(usp))
{
  #i <- 1
  temp <- dplyr::filter(psummary, species == usp[i])
  t_xi <- temp$xi_mean[1]
  t_gamma <- temp$gamma_mean[1]
  sc_lat <- scale(temp$cell_lat, scale = FALSE)  
  SLAT <- seq(range(sc_lat)[1], range(sc_lat)[2], length.out = L)
  cnt <- attr(sc_lat, 'scaled:center')
  
  sensim_df$species[counter:(counter + L - 1)] <- usp[i]
  sensim_df$slat[counter:(counter + L - 1)] <- SLAT
  sensim_df$lat[counter:(counter + L - 1)] <- SLAT + cnt
  sensim_df$sensim[counter:(counter + L - 1)] <- t_xi + t_gamma * SLAT
  
  counter <- counter + L
}

saveRDS(sensim_df, file="Data/fit_df_tab5.rds")    


##############
## traits plot

# process -----------------------------------------------------------------

#merge xi estimates with PC1
mrg_xi_PC <- data.frame(unique(psummary[,c('species', 
                                           'xi_mean', 'xi_sd')]),
                        PC1 = DATA$PC1)


# get slope and ribbon ----------------------------------------------------

alpha_xi_ch <- MCMCvis::MCMCchains(fit_subset, params = c('alpha_xi'))
beta_xi_ch <- MCMCvis::MCMCchains(fit_subset, params = c('beta_xi'))

sim_PC1 <- seq(range(DATA$PC1)[1], range(DATA$PC1)[2], 
               length.out = 50)
fit_sim <- matrix(NA,
                  nrow = NROW(alpha_xi_ch), 
                  ncol = length(sim_PC1))

for (i in 1:NROW(alpha_xi_ch)){
  fit_sim[i,] <- alpha_xi_ch[i,1] + beta_xi_ch[i,1] * sim_PC1
}

fit_mn <- apply(fit_sim, 2, mean)
fit_LCI <- apply(fit_sim, 2, function(x) quantile(x, probs = 0.025))
fit_UCI <- apply(fit_sim, 2, function(x) quantile(x, probs = 0.975))

fit_df <- data.frame(sim_PC1, fit_mn, fit_LCI, fit_UCI)

colnames(mrg_xi_PC)[1] <- "sci_name"
mrg2_xi_PC <- left_join(mrg_xi_PC, specieskey2, by = "sci_name")

saveRDS(mrg2_xi_PC, file="Data/mrg2_xi_PC.rds")
saveRDS(fit_df, file="Data/fit_df_tab6.rds")       



# filter green-up for data download
gr_df <- readRDS('Data/Raw/MidGreenup-2020-08-06-forest.rds')
gr_df2 <- dplyr::select(gr_df, year, cell, cell_lat, cell_lng, gr_mn, gr_ncell, gr_pcell, gr_type)
ucell <- unique(arr_master$cell)
for_gr <- dplyr::filter(gr_df2, cell %in% ucell, year %in% 2002:2017)

saveRDS(for_gr, 'Data/for_green-up_dl.rds')

