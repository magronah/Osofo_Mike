# Load the package
library(nnet)
library(dplyr)
library(tidyr)
library(readxl)
#########################
data1  =  read.csv("mike_data.csv")
#########################
data2 =  read_excel("NEW_Sheet_analysis.xlsx")
data22  <-   data2 %>%
  dplyr::select("ID", "\r\nSex","Education Attainment")

View(data)
names(data2)
########################################################
data   =   left_join(data1, data22, by ="ID")
data2$Leadership_Position
data2$`Education Field...8`
data2$`Sex`
data2$AGE
#########################
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
########################
sub_data$ID = data$ID
sub_data$represented_interest = data$Type.of.represented.interest
sub_data$leadership_type = data$Recruitment.Type
sub_data$Sex      =   data$Sex
sub_data$Education_Attainment      =   data$`Education Attainment`


#####################
View(sub_data)

dd_long <- sub_data %>%
  pivot_longer(
    cols = ends_with("Position"),     # Melt only transfer columns
    names_to = "transfer_number",       # New column for original column names
    values_to = "transfer_value"        # New column for values
  )


dim(dd_long)
View(na.omit(dd_long))
##########################
################# Generate concatenated columns #####################
for (i in 1:(length(positions) - 1)) {
  col1 <- positions[i]
  col2 <- positions[i + 1]
  new_col <- paste0("Merged_", i, "_", i + 1)
  data[[new_col]] <- paste(data[[col1]], data[[col2]], sep = "_")
}

merged_cols <- paste0("Merged_", 1:12, "_", 2:13)
sub_data$ID = data$ID
sub_data$represented_interest = data$Type.of.represented.interest

sub_data$leadership_type = data$Recruitment.Type
#####################
View(sub_data)

dd_long1 <- sub_data %>%
  pivot_longer(
    cols = ends_with("Position"),     # Melt only transfer columns
    names_to = "transfer_institution",       # New column for original column names
    values_to = "position_value"        # New column for values
  )


dim(dd_long1)
View(na.omit(dd_long1))

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
dd_long2 <- transfer_data %>%
  pivot_longer(
    cols = starts_with("transfer"),     # Melt only transfer columns
    names_to = "transfer_number",       # New column for original column names
    values_to = "transfer_value"        # New column for values
  )

ddf =  left_join(dd_long2, dd_long1, by  = "ID")
ddf_long_clean <- na.omit(ddf) %>%
  filter(
    !is.na(transfer_value),                     # remove missing values
    !grepl("_NA", transfer_value),              # remove values that contain "_NA"
    transfer_value != "NA_NA"                   # remove exact match "NA_NA"
  )
##########################

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

dd_filtered <- ddf_long_clean %>%
  filter(transfer_value %in% unlist(category_map)) %>%
  mutate(transfer_pattern = case_when(
    transfer_value %in% category_map$Intra_NS_to_NS ~ "Intra_NS_to_NS",
    transfer_value %in% category_map$Intra_NS_to_S ~ "Intra_NS_to_S",
    transfer_value %in% category_map$Intra_S_to_NS ~ "Intra_S_to_NS",
    transfer_value %in% category_map$Inter_NS_to_NS ~ "Inter_NS_to_NS",
    transfer_value %in% category_map$Inter_NS_to_S ~ "Inter_NS_to_S",
    transfer_value %in% category_map$Inter_S_to_NS ~ "Inter_S_to_NS"
  ))
unique(dd_filtered$position_value)

##########################
category_map2 =  list(state =  c("EDU_INST","EDU_GOV", "LOC_GOV","OTH_GOV","PARL"),
                       nonstate = c("EDU_IG","OTH_IG","PRIV_SEC","Media","POL_PA"))
dd_filtered <- dd_filtered %>%
  filter(position_value %in% unlist(category_map2)) %>%
  mutate(type_of_actors = case_when(
    position_value %in% category_map2$state ~ "state",
    position_value %in% category_map2$nonstate ~ "nonstate"
  ))
###########################################################################
category_map3 <- list(

  "Intra_NS_to_S"      =   c("EDU_IG_EDU_INST", "EDU_IG_EDU_GOV"),
  
  "Intra_S_to_NS"      =  c("EDU_INST_EDU_IG", "EDU_GOV_EDU_IG"),
  
  "Inter_NS_to_S"      =  c("EDU_IG_OTH_GOV", "EDU_IG_PARL", 
                            "EDU_IG_LOC_GOV"),
  
  "Inter_S_to_NS"   =     c("OTH_GOV_EDU_IG", "PARL_EDU_IG", 
                            "LOC_GOV_EDU_IG")
)

dd_filtered3 <- ddf_long_clean %>%
  filter(transfer_value %in% unlist(category_map)) %>%
  mutate(transfer_pattern = case_when(
    transfer_value %in% c(category_map$Intra_NS_to_S,category_map$Inter_NS_to_S) ~ "Nonstate_State",
    transfer_value %in% c(category_map$Intra_S_to_NS,category_map$Inter_S_to_NS) ~ "State_Nonstate"
  ))


unique(dd_filtered3$transfer_pattern)

state_to_nonstate <- sum(na.omit(dd_filtered3$transfer_pattern) == "State_Nonstate")
nonstate_to_state <- sum(na.omit(dd_filtered3$transfer_pattern) == "Nonstate_State")



x <- c(state_to_nonstate, nonstate_to_state)  
n <- c(state_to_nonstate + nonstate_to_state, 
       state_to_nonstate + nonstate_to_state)  # total in both groups (same)

prop.test(x = x, n=n, alternative = "greater")
###########################################################################


####################################
# Simulate some data
# Fit multinomial logistic regression model
# Note: multinom() automatically uses the first level as the baseline
model1 <- multinom(transfer_pattern ~ represented_interest + leadership_type +
                    type_of_actors, data = dd_filtered)

# Summary of the model
sum=summary(model1)

# Get p-values
#z <- summary(model)$coefficients / summary(model)$standard.errors
z <- sum$coefficients / sum$standard.errors
p <- (1 - pnorm(abs(z))) * 2
print(p)

##########################################################################
model2 <- multinom(transfer_pattern ~ represented_interest + leadership_type +
                    Sex + Education_Attainment + type_of_actors, 
                  data = dd_filtered)

sum2=summary(model2)

# Get p-values
#z <- summary(model)$coefficients / summary(model)$standard.errors
z2 <- sum2$coefficients / sum2$standard.errors
p2 <- (1 - pnorm(abs(z2))) * 2
print(p2)


#install.packages("broom")

library(broom)
library(dplyr)
library(tibble)

coefs1 <- tidy(model1) %>%
  select(term, estimate) %>%
  rename(estimate_model1 = estimate)

View(coefs2)
coefs2 <- tidy(model2) %>%
  select(term, estimate) %>%
  rename(estimate_model2 = estimate)

# Join the two tables by term
comparison_table <- full_join(coefs1, coefs2, by = "term", relationship = "many-to-many")

# Print as a neat tibble
View(comparison_table)

model <- multinom(type_of_actors ~ represented_interest + leadership_type,
                     data = dd_filtered)

# Summary of the model
sum=summary(model)

# Get p-values
#z <- summary(model)$coefficients / summary(model)$standard.errors
z <- sum$coefficients / sum$standard.errors
p <- (1 - pnorm(abs(z))) * 2
print(p)
