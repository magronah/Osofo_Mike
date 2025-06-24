library(cluster) 
library(dplyr) 
library(tidyr)
library(psych)  
library(reshape) 
library(TraMineR)  
library(TraMineRextras) 
library(ggplot2)
library(here)
###################################################################
source("functions.R")
plot_path =  "Results_Findings"
###################################################################
## Reading the csv file
data  =  read.csv("mike_data.csv")
###################################################################
## Data cleaning 
#### Rename Step10 to Step13 to their corresponding position names
position_renames <- c(
  Step10 = "Tenth_Position",
  Step11 = "Eleventh_Position",
  Step12 = "Twelfth_Position",
  Step13 = "Thirteenth_Position"
)

names(data)[names(data) %in% names(position_renames)] <- 
  position_renames[names(data)[names(data) %in% names(position_renames)]]
###################################################################
######### Define the sequence of positions ############
positions <- c("First_Position", "Second_Position", "Third_Position", 
               "Fourth_Position", "Fifth_Position", "Sixth_Position", 
               "Seventhth_Position", "Eigth_Position", "Nineth_Position", 
               "Tenth_Position", "Eleventh_Position", "Twelfth_Position", 
               "Thirteenth_Position")
##########################################################################
sub_data   <-   data[ , grepl("Position", names(data))]
sub_data[] <- lapply(sub_data, function(x) {
  if (is.factor(x)) x <- as.character(x)
  x <- gsub("\\n", "", x)  
  x <- trimws(x)    
})

dd           =  as.data.frame(table(na.omit(unlist(sub_data))))
dd$Prop      =  dd$Freq/sum(dd$Freq)
colnames(dd) =  c("Transfer", "Frequency", "Proportion")

plot_size =  12; plot_width  =  10; plot_height = 4
plot1  =  ggplot(dd, aes(Transfer,Proportion, group = 1)) + 
         geom_point() +
         geom_line() + 
         custom_theme(plot_size) +
  ggtitle("Proportion of Transfer Categories")

  
ggsave(paste0("Results_Findings/","transfer_count.png"), 
       plot = plot1, width = plot_width, height = plot_height, dpi = 300)

write.csv(dd, file = paste0("Results_Findings/","count_data.csv"),
          row.names = FALSE)
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

#n           <- length(unique_clean_entries)
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
## Set colors
na_index <- which(unique_clean_entries == "NA_NA")
transfer_colors  <-  rainbow(length(unique_clean_entries))
transfer_colors[na_index] <- "white"
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
#transfer_colors <- rainbow(length(unique_transfers))

########### White color for "NA_NA"
# if (NA %in% unique_transfers) {
#  transfer_colors[which(unique_transfers == NA)] <- "white"
# }
################################################################
seq_func(clean_filtered_data[ , -1], unique_transfers,
         plot_title = "Type of represented interest (Provider)")
#############################################################
# Get all transitions into one vector
all_transfers <- unlist(clean_filtered_data[, -1])

# Count occurrences
transfer_counts <- table(all_transfers)

# Convert to named vector for easy lookup
transfer_counts_named <- as.list(transfer_counts)

# Create labels like "EDU_IG_EDU_IG (105)"
labels_with_counts <- sapply(unique_transfers, function(tr) {
  count <- transfer_counts_named[[tr]]
  if (is.null(count)) count <- 0
  paste0(tr, " (", count, ")")
})

###############################################################
# Clean whitespace and special characters in transfer columns
data_clean <- transfer_data %>%
  mutate(across(starts_with("transfer"), ~ gsub("[[:space:]]|\\n|\\\\|&", "", .)))

# Extract unique transitions actually used in data
unique_transfers <- unique(unlist(data_clean[ , -1]))  

unique_transfers <- sort(unique_transfers)

# Generate colors for each unique transition
transfer_colors <- rainbow(length(unique_transfers))

# Define sequence object
transfers_seq <- seqdef(data_clean[ , -1],
                        alphabet = unique_transfers,
                        labels = unique_transfers,
                        xtstep = 1,
                        cpal = transfer_colors)

########################################################################
data_clean <- transfer_data %>%
  mutate(across(starts_with("Transfer"), ~ gsub("[[:space:]]|\\n|\\\\|&", "", .)))

unique_transfers <- unique(unlist(data_clean[ , -1]))  

# Create a color vector using rainbow
transfer_colors <- rainbow(length(unique_transfers))

# transfers_seq <- seqdef(data_clean[ , -1],
#                         alphabet = selected_states,
#                         labels = selected_states,
#                         xtstep = 1,
#                         cpal = transfer_colors_filtered)
# transfers_seq <- seqdef(clean_filtered_data[ , -1],
#                         alphabet = unique_transfers,
#                         labels   = labels_with_counts,  # <-- counts shown here
#                         xtstep   = 1,
#                         cpal     = transfer_colors)

# seqIplot(transfers_seq,
#          with.legend = "right",
#          main = "Individual Transfer Trajectories (with Counts)",
#          cex.legend = 0.6)

#########################################################################
# Define your category mapping
category_map <- list(
  "Intra_NS_to_NS"     =   c("EDU_IG_EDU_IG"),
  
  "Intra_NS_to_S"      =   c("EDU_IG_EDU_INST", "EDU_IG_EDU_GOV"),
  
  "Intra_S_to_NS"      =  c("EDU_INST_EDU_IG", "EDU_GOV_EDU_IG"),
  
  "Inter_NS_to_NS"     =  c("OTH_IG_EDU_IG", "POL_PA_EDU_IG", 
                                  "PRIV_SEC_EDU_IG", "MEDIA_EDU_IG"),
  
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
  #recoded[is.na(col)] <- NA  # Preserve NA
  return(recoded)
})

unique(unlist(recat_data[ , -1]))

unique_labels <- sort(na.omit(unique(unlist(recat_data[ , -1]))))
seq_func(recat_data[ , -1], unique_labels)
######################################################################



################################################################
transfer_data2  = transfer_data # clean_filtered_data#
# ddf   =  data.frame(ID    =  data$ID,
#                    Type   =  data$Recruitment.Type)
# 
# 
# ddf2  =  left_join(transfer_data2, ddf, by  = "ID")

################################################################
transfer_cols  <-  setdiff(names(transfer_data), "ID")

#INTER_NS_TO_NS
clean_filtered_data_interNSNS <- transfer_data2[apply(transfer_data2[transfer_cols], 1, function(row) {
  any(row == "EDU_IG_EDU_IG", na.rm = TRUE)
}), ]

clean_filtered_data_interNSNS[transfer_cols] <- lapply(clean_filtered_data_interNSNS[transfer_cols], function(col) {
  ifelse(col == "EDU_IG_EDU_IG", col, NA)
})
all_statesNSNS  = "EDU_IG_EDU_IG"

transfer_colors <- c("EDU_IG_EDU_IG" = "#1f77b4") 
seq_func(clean_filtered_data_interNSNS[ , -1], all_statesNSNS)


# seq_employed <- seqdef(clean_filtered_data_interNSNS[,-1],
#                        alphabet = all_statesNSNS,
#                        cpal     = transfer_colors,
#                        labels   = all_statesNSNS,
#                        xtstep   = 1)
# 
# 
# seqIplot(seq_employed,
#          with.legend = "right",
#          main = "Individual Transfer Trajectories",
#          cex.legend = 0.6)


#######################################################
#INTER_S_TO_NS:  EDU_INST_EDU_IG", "EDU_GOV_EDU_IG
# data = transfer_data2
# category_map <- list(
#   "Intra_NS_to_NS"            = c("EDU_IG_EDU_IG"),
#   "Intra_S_to_NS"      =    c("EDU_INST_EDU_IG", "EDU_GOV_EDU_IG"),
#   "Inter_NS_to_NS"            = c("OTH_IG_EDU_IG", "POL_PA_EDU_IG", "PRIV_SEC_EDU_IG", "MEDIA_EDU_IG"),
#   "Inter_S_to_NS"   = c("OTH_GOV_EDU_IG", "PARL_EDU_IG", "LOC_GOV_EDU_IG")
# )
#######################################################
target_states = category_map$Intra_NS_to_NS 
seq_func(clean_filtered_data, target_states)
#######################################################
target_states = category_map$Intra_S_to_NS 
seq_func(clean_filtered_data, target_states)
######################################################
target_states = category_map$Inter_NS_to_NS 
seq_func(clean_filtered_data, target_states)
#####################################################
target_states = category_map$Inter_S_to_NS 
seq_func(clean_filtered_data, target_states)
####################################################
target_states = category_map$Inter_NS_to_S 
seq_func(clean_filtered_data, target_states)
####################################################
target_states = category_map$Intra_NS_to_S 
seq_func(clean_filtered_data, target_states)
####################################################
# Only intra sectoral
target_states = c(category_map$Intra_NS_to_NS,
                  category_map$Intra_S_to_NS, 
                  category_map$Intra_NS_to_S) 

seq_func(clean_filtered_data, target_states)
###################################################
#Only intersectoral
target_states = c(category_map$Inter_NS_to_NS,
                  category_map$Inter_S_to_NS,
                  category_map$Inter_NS_to_S) 

seq_func(clean_filtered_data, target_states)
###################################################
#Only NS_NS
target_states = c(category_map$Intra_NS_to_NS,
                  category_map$Inter_NS_to_NS) 

seq_func(clean_filtered_data, target_states)
###################################################
#Only S_NS
target_states = c(category_map$Intra_S_to_NS,
                  category_map$Inter_S_to_NS) 

seq_func(clean_filtered_data, target_states)
###################################################
ddf   =  data.frame(ID    =  data$ID,
                   Type   =  data$Recruitment.Type)
ddf2  =  left_join(clean_filtered_data, ddf, by  = "ID")

ddf_elected  <- ddf2[ddf2$Type == "Elected", ]
ddf_employed <- ddf2[ddf2$Type == "Employed", ]

target_states   =  unlist(category_map)
seq_func(ddf_elected, target_states)
seq_func(ddf_employed, target_states)

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
         plot_title = "Employed Leaders Career Transfer")

###################################################
ddf_Prof  <- data.frame(ID = data$ID, 
                        Transfer_pattern = data$Type.of.represented.interest)
# ddf_Profession  <-  data[data$Type.of.represented.interest == "Profession", ]
# ddf_Users <- data[data$Type.of.represented.interest == "Users", ]
# ddf_Providers   <- data[data$Type.of.represented.interest == "Providers", ]

ddf2  =  left_join(clean_filtered_data, ddf_Prof, by  = "ID")

ddf_Profession  <- ddf2[ddf2$Transfer_pattern == "Profession", ]
ddf_Users       <-    ddf2[ddf2$Transfer_pattern == "Users", ]
ddf_Providers   <- ddf2[ddf2$Transfer_pattern == "Providers", ]

ddf_profession_sec   =  ddf_Profession
ddf_profession_sec[ , -1] <- lapply(ddf_profession_sec[ , -1], function(col) {
  recoded <- transfer_to_category[as.character(col)]  # use names to map
  #recoded[is.na(col)] <- NA  # Preserve NA
  return(recoded)
})

ddf_user_sec   =  ddf_Users
ddf_user_sec[ , -1] <- lapply(ddf_user_sec[ , -1], function(col) {
  recoded <- transfer_to_category[as.character(col)]  # use names to map
  #recoded[is.na(col)] <- NA  # Preserve NA
  return(recoded)
})

ddf_providers_sec   =  ddf_Providers
ddf_providers_sec[ , -1] <- lapply(ddf_providers_sec[ , -1], function(col) {
  recoded <- transfer_to_category[as.character(col)]  # use names to map
  #recoded[is.na(col)] <- NA  # Preserve NA
  return(recoded)
})

unique_labels <- sort(na.omit(unique(unlist(ddf_profession_sec[ , -1]))))
seq_func(ddf_profession_sec, unique_labels,
         plot_title = "Type of represented interest (Profession)")

seq_func(ddf_user_sec, unique_labels,
         plot_title = "Type of represented interest (Users)")

seq_func(ddf_providers_sec, unique_labels,
         plot_title = "Type of represented interest (Provider)")
#########################################################
elected <- data %>%
  filter(ID %in% ddf_elected$ID)

employed <- data %>%
  filter(ID %in% ddf_employed$ID)
###################################################
target_states = unlist(category_map)
pp(elected, target_states)

transfer_to_category <- unlist(lapply(names(category_map), function(category) {
  setNames(rep(category, length(category_map[[category]])), category_map[[category]])
}))

elected1  =  elected
elected1[ , -1] <- lapply(elected1[ , -1], function(col) {
  recoded <- transfer_to_category[as.character(col)]  # use names to map
  recoded[is.na(col)] <- NA  # Preserve NA
  return(recoded)
})

target_states  =  unlist(elected1[,-1])
pp(elected1, target_states)
#############################################################
target_states = unlist(category_map)
pp(employed, target_states)

transfer_to_category <- unlist(lapply(names(category_map), function(category) {
  setNames(rep(category, length(category_map[[category]])), category_map[[category]])
}))

employed1  =  employed
employed1[ , -1] <- lapply(employed1[ , -1], function(col) {
  recoded <- transfer_to_category[as.character(col)]  # use names to map
  recoded[is.na(col)] <- NA  # Preserve NA
  return(recoded)
})

target_states  =  unlist(employed1[,-1])
pp(employed1, target_states)
#######################################################


#######################################################
all_states      <-   unlist(filtered_data[, -1])
all_states      <-   all_states[!is.na(all_states)]
category_counts <-   sort(table(all_states))

filtered_data_interSNS <- transfer_data2[apply(transfer_data2[transfer_cols], 1, 
                                                   function(row) {
                                                     any(row %in% target_states, na.rm = TRUE)
                                                   }), ]


all_states <- unlist(filtered_data_interSNS[, -1])
all_states <- all_states[!is.na(all_states)]

category_counts <- sort(table(all_states))

# Create labels like "Intra Non-State (n = 45)"
category_labels_with_counts <- paste0(names(category_counts), " (n = ", category_counts, ")")
names(category_labels_with_counts) <- names(category_counts)

# Final unique alphabet (in same order as used in the sequence object)
unique_labels <- sort(unique(all_states))

# Match category label with counts
final_labels <- category_labels_with_counts[unique_labels]
category_colors <- rainbow(length(unique_labels))

cat_seq <- seqdef(recat_data[, -1],
                  alphabet = unique_labels,
                  labels   = final_labels,
                  xtstep   = 1,
                  cpal     = category_colors)



################################################################

anyDuplicated(transfer_colors)
anyDuplicated(all_states)

transfer_colors <- rainbow(length(all_states))
names(transfer_colors) <- all_states

ddf_elected  <- ddf2[ddf2$Type == "Elected", ]
ddf_employed <- ddf2[ddf2$Type == "Employed", ]

View(transfers_elected)
transfers_elected  <- ddf_elected[, grep("^transfer", names(ddf_elected))]
transfers_employed <- ddf_employed[, grep("^transfer", names(ddf_employed))]


# Make a copy
recat_data <- transfers_elected

# Apply the recoding to each column except ID
recat_data[ , -1] <- lapply(recat_data[ , -1], function(col) {
  recoded <- transfer_to_category[as.character(col)]  # use names to map
  recoded[is.na(col)] <- NA  # Preserve NA
  return(recoded)
})

unique_labels <- sort(na.omit(unique(unlist(recat_data[ , -1]))))
category_colors <- rainbow(length(unique_labels))

seq_elected <- seqdef(recat_data,
                      alphabet = all_states,
                      cpal     = category_colors,
                      labels   = all_states,
                      xtstep   = 1)

seq_employed <- seqdef(transfers_employed,
                       alphabet = all_states,
                       cpal     = transfer_colors,
                       labels   = all_states,
                       xtstep   = 1)

seqIplot(seq_employed,
         with.legend = "right",
         main = "Individual Transfer Trajectories",
         cex.legend = 0.6)

seqIplot(transfers_elected,
         with.legend = "right",
         main = "Individual Transfer Trajectories",
         cex.legend = 0.6)

### elected and employed!!!

filtered_data_interSNS[transfer_cols] <- lapply(filtered_data_interSNS[transfer_cols], function(col) {
  ifelse(col %in% target_states, col, NA)
})

all_statesNSNS  <- target_states
transfer_colors <- c("EDU_INST_EDU_IG" = "#1f77b4", 
                     "EDU_GOV_EDU_IG" = "#ff7f0e")

all_states <- unlist(filtered_data_interSNS[, -1])
all_states <- all_states[!is.na(all_states)]

category_counts <- sort(table(all_states))

category_counts <- sort(table(all_states))
category_labels_with_counts <- paste0(names(category_counts), 
                                      " (n = ", category_counts, ")")

names(category_labels_with_counts) <- names(category_counts)
unique_labels <- sort(unique(all_states))

final_labels <- category_labels_with_counts[unique_labels]

seq_employed <- seqdef(filtered_data_interSNS[,-1],
                       alphabet = unique_labels,
                       cpal     = transfer_colors,
                       labels   = final_labels,
                       xtstep   = 1)

seqIplot(seq_employed,
         with.legend = "right",
         main = "Individual Transfer Trajectories",
         cex.legend = 0.6)


target_states <- c("EDU_INST_EDU_IG", "EDU_GOV_EDU_IG")

#############delete
transfer_cols  <-  setdiff(names(data), "ID")

filtered_data_interSNS[transfer_cols] <- lapply(filtered_data_interSNS[transfer_cols], function(col) {
  ifelse(col %in% target_states, col, NA)
})

all_statesNSNS  <- target_states
transfer_colors <- c("EDU_INST_EDU_IG" = "#1f77b4", 
                     "EDU_GOV_EDU_IG" = "#ff7f0e")

all_states <- unlist(filtered_data_interSNS[, -1])
all_states <- all_states[!is.na(all_states)]

category_counts <- sort(table(all_states))

category_counts <- sort(table(all_states))
category_labels_with_counts <- paste0(names(category_counts), 
                                      " (n = ", category_counts, ")")

names(category_labels_with_counts) <- names(category_counts)
unique_labels <- sort(unique(all_states))

final_labels <- category_labels_with_counts[unique_labels]

seq_employed <- seqdef(filtered_data_interSNS[,-1],
                       alphabet = unique_labels,
                       cpal     = transfer_colors,
                       labels   = final_labels,
                       xtstep   = 1)

seqIplot(seq_employed,
         with.legend = "right",
         main = "Individual Transfer Trajectories",
         cex.legend = 0.6)


target_states <- c("EDU_INST_EDU_IG", "EDU_GOV_EDU_IG")




#category_colors <- rainbow(length(unique_labels))
# Define the sequence object
# cat_seq <- seqdef(recat_data[ , -1],
#                   alphabet = unique_labels,
#                   labels = unique_labels,
#                   xtstep = 1,
#                   cpal = category_colors)

# Plot
# seqIplot(cat_seq,
#          with.legend = "right",
#          main = "Transfer Category Trajectories",
#          cex.legend = 0.6)
############################################################
# all_states <- unlist(recat_data[, -1])
# all_states <- all_states[!is.na(all_states)]
# 
# category_counts <- sort(table(all_states))
# 
# # Create labels like "Intra Non-State (n = 45)"
# category_labels_with_counts <- paste0(names(category_counts), " (n = ", category_counts, ")")
# names(category_labels_with_counts) <- names(category_counts)
# 
# # Final unique alphabet (in same order as used in the sequence object)
# unique_labels <- sort(unique(all_states))

# Match category label with counts
#final_labels <- category_labels_with_counts[unique_labels]
#category_colors <- rainbow(length(unique_labels))

# cat_seq <- seqdef(recat_data[, -1],
#                   alphabet = unique_labels,
#                   labels   = final_labels,
#                   xtstep   = 1,
#                   cpal     = category_colors)
# 
# seqIplot(cat_seq,
#          with.legend = "right",
#          main = "Transfer Category Trajectories (with Counts)",
#          cex.legend = 0.6)