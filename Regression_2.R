# Load the package
library(nnet)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(forcats)
library(broom)
library(margins)
library(ggeffects)
#########################
data1  =  read.csv("mike_data.csv")
##################################################
data2 =  read_excel("NEW_Sheet_analysis.xlsx")
data22  <-   data2 %>%
  dplyr::select("ID", "AGE", "\r\nSex","Education Attainment", 
                "Education Field...8", "Leadership_Position")%>%
  setNames(c("ID", "AGE", "Sex", "Education_Attainment", "Education_Field",
             "Leadership_Position"))
########################################################
data   =   left_join(data1, data22, by ="ID")
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
sub_data <- data[, grepl("Position", names(data)) & names(data) != "Leadership_Position"]
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
sub_data$Leadership_Position      =   data$Leadership_Position
sub_data$Age = data$AGE
sub_data$Education.Field = data$Education.Field
#####################
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

dd_long <- transfer_data %>%
  pivot_longer(
    cols = starts_with("transfer"),     # Melt only transfer columns
    names_to = "transfer_number",       # New column for original column names
    values_to = "transfer_value"        # New column for values
  )

ddf_long_clean <- na.omit(dd_long) %>%
  filter(
    !is.na(transfer_value),                     # remove missing values
    !grepl("_NA", transfer_value),              # remove values that contain "_NA"
    transfer_value != "NA_NA")                   # remove exact match "NA_NA"

###########################################################################
# Proportion Test to answer hypothesis 1.
# H1: Transfer patterns from state to Non-state are more likely than 
# Non-state to state

#' Proportion of individuals moving from state to Non-state is greater than 
# individuals moving from Non-state to state?

category_map <- list(
  "Intra_NS_to_S"      =   c("EDU_IG_EDU_GOV"),
  "Intra_S_to_NS"      =  c("EDU_GOV_EDU_IG"),
  "Inter_NS_to_S"      =  c("EDU_IG_OTH_GOV", "EDU_IG_PARL",
                            "EDU_IG_LOC_GOV"),
  "Inter_S_to_NS"   =     c("OTH_GOV_EDU_IG", "PARL_EDU_IG",
                            "LOC_GOV_EDU_IG"))

dd_filtered <- ddf_long_clean %>%
  filter(transfer_value %in% unlist(category_map)) %>%
  mutate(transfer_pattern = case_when(
    transfer_value %in% c(category_map$Intra_NS_to_S, category_map$Inter_NS_to_S) ~ "Nonstate_State",
    transfer_value %in% c(category_map$Intra_S_to_NS, category_map$Inter_S_to_NS) ~ "State_Nonstate"
  )) %>%
  filter(!is.na(transfer_pattern))  


state_to_nonstate <- sum((dd_filtered$transfer_pattern) == "State_Nonstate")
nonstate_to_state <- sum((dd_filtered$transfer_pattern) == "Nonstate_State")

x <- c(state_to_nonstate, nonstate_to_state)  
## First, we shouldn't be doubling the total, it is just one total

### H_null : p_{state_nonstate} = p_{nonstate_state}
### H_alternate : p_{state_nonstate} > p_{nonstate_state}
### pvalue < 0.05, then we reject H_null, accepting that H_alternate
### pvalue > 0.05, then we dont have enough evidence from our data to 
###               reject H_null

### X-squared  
chisq.test(x = x, p = c(0.5, 0.5))
##########################

