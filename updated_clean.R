library(cluster) 
library(dplyr) 
library(tidyr)
library(psych)  
library(reshape) 
library(TraMineR)  
library(TraMineRextras) 
library(ggplot2)
library(here)
library(readxl)
###################################################################
source("functions.R")
source("data_processing.R")
plot_path =  "Results_Findings"
###################################################################
rr  = read.csv(paste0("Results_Findings/","count_data.csv"))
################# Generate concatenated columns #####################
for (i in 1:(length(positions) - 1)) {
  col1 <- positions[i]
  col2 <- positions[i + 1]
  new_col <- paste0("Merged_", i, "_", i + 1)
  data[[new_col]] <- paste(data[[col1]], data[[col2]], sep = "_")
}
merged_cols <- paste0("Merged_", 1:12, "_", 2:13)

## Clean up entries, removing spaces and special characters
raw_entries          <- unique(unlist(data[merged_cols]))
clean_entries        <- gsub("[^A-Za-z0-9_]", "", raw_entries)
unique_clean_entries <- unique(clean_entries)
entry_map            <-  setNames(clean_entries, raw_entries)
########################################################################
clean_data =  data
clean_data[merged_cols] <- lapply(clean_data[merged_cols], function(col) {
  entry_map[as.character(col)]
})
########################################################################
###' Create transfer data by merging columns and 
###' rename merged columns to transfer1, ..., transfer12
transfer_data <- clean_data[, grepl("^ID$", names(clean_data)) | 
                              grepl("Merged", names(clean_data))]

n_transfers <- ncol(transfer_data) - 1  
names(transfer_data) <- c("ID", paste0("transfer", 1:n_transfers))
###############################################################
selected_states <- c(
  "EDU_GOV_EDU_IG",
  "EDU_IG_EDU_IG",
  "EDU_INST_EDU_IG",
  "LOC_GOV_EDU_IG",
  "OTH_GOV_EDU_IG",
  "OTH_IG_EDU_IG",
  "PARL_EDU_IG",
  "POL_PA_EDU_IG",
  "PRIV_SEC_EDU_IG",
  "Media_EDU_IG",
  "EDU_IG_EDU_INST", 
  "EDU_IG_EDU_GOV",
  "EDU_IG_OTH_GOV", 
  "EDU_IG_PARL", 
  "EDU_IG_LOC_GOV")
###############################################################
## set everything other than those in selected_states to NA
clean_filtered_data  <-  transfer_data
clean_filtered_data[ , -1] <- lapply(clean_filtered_data[ , -1], function(col) {
  col <- ifelse(col %in% selected_states, as.character(col), NA)
  return(col)
})

unique_transfers <- sort(unique(unlist(clean_filtered_data[, -1])))
################################################################
seq_func(clean_filtered_data, unique_transfers,
         plot_title = "Interest Group Leaders' Career Transfer Events")
#############################################################
# Define your category mapping
category_map <- list(
  "Intra_NS_to_NS"     =   c("EDU_IG_EDU_IG"),
  
  "Intra_NS_to_S"      =   c("EDU_IG_EDU_INST", "EDU_IG_EDU_GOV"),
  
  "Intra_S_to_NS"      =  c("EDU_INST_EDU_IG", "EDU_GOV_EDU_IG"),
  
  "Inter_NS_to_NS"     =  c("OTH_IG_EDU_IG", "POL_PA_EDU_IG", 
                            "PRIV_SEC_EDU_IG", "Media_EDU_IG"),
  
  "Inter_NS_to_S"      =  c("EDU_IG_OTH_GOV", "EDU_IG_PARL", 
                            "EDU_IG_LOC_GOV"),
  
  "Inter_S_to_NS"   =     c("OTH_GOV_EDU_IG", "PARL_EDU_IG", 
                            "LOC_GOV_EDU_IG")
)

# Flatten to a named character vector
transfer_to_category <- unlist(lapply(names(category_map), function(category) {
  setNames(rep(category, length(category_map[[category]])), category_map[[category]])
}))

# Make a copy
recat_data <- clean_filtered_data

# Apply the recoding to each column except ID
recat_data[ , -1] <- lapply(recat_data[ , -1], function(col) {
  recoded <- transfer_to_category[as.character(col)]  # use names to map
  return(recoded)
})

unique(unlist(recat_data[ , -1]))

unique_labels <- sort(na.omit(unique(unlist(recat_data[ , -1]))))
seq_func(recat_data, unique_labels,
         plot_title = "Interest Group Leaders'Career Transfer Patterns")
################################################################
transfer_data2  = transfer_data
################################################################
transfer_cols  <-  setdiff(names(transfer_data), "ID")
#######################################################
target_states = category_map$Intra_NS_to_NS 
seq_func(clean_filtered_data, target_states, 
         plot_title = "Intra-sectoral NonState to Non- Transfer Events")
#######################################################
target_states = category_map$Intra_S_to_NS 
seq_func(clean_filtered_data, target_states, 
         plot_title = "Intra-sectoral State to Non-state Transfer Events")
######################################################
target_states = category_map$Inter_NS_to_NS 
seq_func(clean_filtered_data, target_states, 
         plot_title = "Inter-sectoral NonState to Non-state Transfer Events")
#####################################################
target_states = category_map$Inter_S_to_NS 
seq_func(clean_filtered_data, target_states, 
         plot_title = "Inter-sectoral State to Non-state Transfer")
####################################################
target_states = category_map$Inter_NS_to_S 
seq_func(clean_filtered_data, target_states, 
         plot_title = "Inter-sectoral NonState to State Transfer Events")
####################################################
target_states = category_map$Intra_NS_to_S 
seq_func(clean_filtered_data, target_states, 
         plot_title = "Intra-sectoral NonState to State Transfer Events")
####################################################
# Only intra sectoral
target_states = c(category_map$Intra_NS_to_NS,
                  category_map$Intra_S_to_NS, 
                  category_map$Intra_NS_to_S) 

seq_func(clean_filtered_data, target_states, 
         plot_title = "Intra-sectoral Transfer Events")
###################################################
#Only inter sectoral
target_states = c(category_map$Inter_NS_to_NS,
                  category_map$Inter_S_to_NS,
                  category_map$Inter_NS_to_S) 

seq_func(clean_filtered_data, target_states,
         plot_title = "Inter-sectoral Transfer Events")
###################################################
#Only NS_NS
target_states = c(category_map$Intra_NS_to_NS,
                  category_map$Inter_NS_to_NS) 

seq_func(clean_filtered_data, target_states, 
         plot_title = "Non-state to Non-state Transfer Events")
###################################################
#Only S_NS Sectors
target_states = c(category_map$Intra_S_to_NS,
                  category_map$Inter_S_to_NS) 

seq_func(clean_filtered_data, target_states, 
         plot_title = "State to Non-state Transfer Events")
###################################################
#Only S_NS Actors
target_states = c(category_map$Intra_S_to_NS,
                  category_map$Inter_S_to_NS) 

target_states <- target_states[target_states
                             != "EDU_INST_EDU_IG"]

seq_func(clean_filtered_data, target_states, 
         plot_title = "State to Non-state Transfer Events")
###################################################

#Only NS_S
target_states = c(category_map$Intra_NS_to_S,
                  category_map$Inter_NS_to_S) 

target_states <- target_states[target_states 
                               != "EDU_IG_EDU_INST"]

seq_func(clean_filtered_data, target_states, 
         plot_title = "Non-state to State Transfer Events")
###################################################
ddf   =  data.frame(ID    =  data$ID,
                    Type   =  data$Recruitment.Type)
ddf2  =  left_join(clean_filtered_data, ddf, by  = "ID")

ddf_elected  <- ddf2[ddf2$Type == "Elected", ]
ddf_employed <- ddf2[ddf2$Type == "Employed", ]

target_states   =  unlist(category_map)
seq_func(ddf_elected, target_states, plot_title = "Elected Leaders Career Transfer Events")
seq_func(ddf_employed, target_states, plot_title = "Employed Leadders Career Transfer Events")

###################################################
## elected and employed categorised to the 6 sectors
# Apply the recoding to each column except ID
ddf_elected_sec   =  ddf_elected
ddf_elected_sec[ , -1] <- lapply(ddf_elected_sec[ , -1], function(col) {
  recoded <- transfer_to_category[as.character(col)]  # use names to map
  #recoded[is.na(col)] <- NA  # Preserve NA
  return(recoded)
})

############################################################
#Only employed
ddf_employed_sec   =  ddf_employed
ddf_employed_sec[ , -1] <- lapply(ddf_employed_sec[ , -1], function(col) {
  recoded <- transfer_to_category[as.character(col)]  # use names to map
  #recoded[is.na(col)] <- NA  # Preserve NA
  return(recoded)
})

unique_labels <- sort(na.omit(unique(unlist(ddf_employed_sec[ , -1]))))

seq_func(ddf_employed_sec[ , -1], unique_labels, 
         plot_title = "Employed Leaders Career Transfer Patterns")

####################################################
#Only elected
ddf_elected_sec   =  ddf_elected
ddf_elected_sec[ , -1] <- lapply(ddf_elected_sec[ , -1], function(col) {
  recoded <- transfer_to_category[as.character(col)]  # use names to map
  #recoded[is.na(col)] <- NA  # Preserve NA
  return(recoded)
})

unique_labels <- sort(na.omit(unique(unlist(ddf_elected_sec[ , -1]))))

seq_func(ddf_elected_sec[ , -1], unique_labels, 
         plot_title = "Elected Leaders Career Transfer Patterns")

###################################################
ddf_Prof  <- data.frame(ID = data$ID, 
                        Transfer_pattern = data$Type.of.represented.interest)

ddf2  =  left_join(clean_filtered_data, ddf_Prof, by  = "ID")
#########################################################################
ddf_Profession  <- ddf2[ddf2$Transfer_pattern == "Profession", ]
ddf_Users       <-    ddf2[ddf2$Transfer_pattern == "Users", ]
ddf_Providers   <- ddf2[ddf2$Transfer_pattern == "Providers", ]

ddf_profession_sec   =  ddf_Profession
ddf_profession_sec[ , -1] <- lapply(ddf_profession_sec[ , -1], function(col) {
  recoded <- transfer_to_category[as.character(col)]  
  return(recoded)
})

ddf_user_sec   =  ddf_Users
ddf_user_sec[ , -1] <- lapply(ddf_user_sec[ , -1], function(col) {
  recoded <- transfer_to_category[as.character(col)]  
  return(recoded)
})

ddf_providers_sec   =  ddf_Providers
ddf_providers_sec[ , -1] <- lapply(ddf_providers_sec[ , -1], function(col) {
  recoded <- transfer_to_category[as.character(col)]  
  return(recoded)
})
########################################################
unique_labels <- sort(na.omit(unique(unlist(ddf_profession_sec[ , -1]))))
seq_func(ddf_profession_sec, unique_labels,
         plot_title = "Career Transfer Patterns of Leaders Representing Employees")

seq_func(ddf_user_sec, unique_labels,
         plot_title = "Career Transfer Patterns of Leaders Representing Service Users")

seq_func(ddf_providers_sec, unique_labels,
         plot_title = "Career Transfer Patterns of Leaders Representing Employers")
#########################################################



