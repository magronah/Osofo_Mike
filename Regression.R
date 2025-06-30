# Load the package
library(nnet)
library(dplyr)
library(tidyr)
#########################
data  =  read.csv("mike_data.csv")
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
to  Everyone
sub_data$ID = data$ID
sub_data$represented_interest = data$Type.of.represented.interest

sub_data$leadership_type = data$Recruitment.Type
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


##########################
# Simulate some data
set.seed(123)

n <- 200  # number of observations

data <- data.frame(
  Gender     = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  Education  = factor(sample(c("HighSchool", "University"), n, replace = TRUE)),
  Region     = factor(sample(c("North", "South"), n, replace = TRUE)),
  Employment = factor(sample(c("Employed", "Unemployed"), n, replace = TRUE)),
  Transport  = factor(sample(c("Car", "Bus", "Bike"), n, replace = TRUE))
)

# Check structure
str(data)

# Fit multinomial logistic regression model
# Note: multinom() automatically uses the first level as the baseline
model <- multinom(Transport ~ Gender + Education + Region + Employment, data = data)

# Summary of the model
summary(model)

# Get p-values
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- (1 - pnorm(abs(z))) * 2
print(p)
