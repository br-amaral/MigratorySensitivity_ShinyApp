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

## change colnames for species names and add speies names as the drop out menu in the shiny app
colnames(arr_master)[1] <- "sci_name"
specieskey <- read.csv("Data/Raw/CaseyCodes.csv") %>%
  transmute(CaseyCode,
            PRIMARY_COM_NAME) %>%
  as.data.frame()
colnames(specieskey) <- c("sci_name","species")
specieskey$sci_name <- as.character(specieskey$sci_name)
specieskey$species <- as.character(specieskey$species)
arr_master2 <- left_join(arr_master,specieskey,by = "sci_name")

arr_master <- arr_master2

saveRDS(arr_master, file = "data_arr.RDS")

## format sensitivity data

head(TAB)
colnames(TAB)

sensi <- readRDS("Data/Raw/arr-gr-SVC-sens-psummary-2020-08-25.rds")

## change colnames for species names and add speies names as the drop out menu in the shiny app
colnames(sensi)[1] <- "sci_name"
sensi2 <- left_join(sensi,specieskey,by = "sci_name")

sensi <- sensi2

saveRDS(sensi, file = "data_sensi.RDS")

##############
## traits plot

# load data ---------------------------------------------------------------

psummary <- readRDS('Data/Raw/arr-gr-SVC-sens-psummary-2020-08-25_2.rds')
DATA <- readRDS('Data/Raw/arr-gr-SVC-sens-data-2020-08-25.rds')
fit_subset <- readRDS('Data/Raw/arr-gr-SVC-sens-stan-output-subset-2020-08-25.rds')

# process -----------------------------------------------------------------

#merge xi estimates with PC1
mrg_xi_PC <- data.frame(unique(psummary[,c('species', 
                                           'xi_mean', 'xi_sd', 
                                           'gamma_mean')]),
                        PC1 = DATA$PC1)

agg_b <- aggregate(beta_mean ~ species, psummary, function(x) quantile(x, probs = 0.95))

mrg2_xi_PC <- dplyr::left_join(mrg_xi_PC, agg_b, by = 'species')

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

colnames(mrg2_xi_PC)[1] <- "sci_name"
mrg2_xi_PC <- left_join(mrg2_xi_PC,specieskey,by = "sci_name")

saveRDS(mrg2_xi_PC, file="Data/mrg2_xi_PC.rds")
saveRDS(fit_df, file="Data/fit_df.rds")       

