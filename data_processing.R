## Reading the csv file
data  =  read.csv("mike_data.csv")
data2=read_excel("NEW_Sheet_analysis.xlsx")
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

### Descriptive statistics
plot_size =  12; plot_width  =  10; plot_height = 4
plot1  =  ggplot(dd, aes(Transfer,Proportion, group = 1)) + 
  geom_point() +
  geom_line() + 
  custom_theme(plot_size) +
  ggtitle("Proportion of Transfer Categories")

ggsave(paste0("Results_Findings/","transfer_count.png"), 
       plot = plot1, width = plot_width, height = plot_height, dpi = 300)
########################################################################
plot2=ggplot(dd, aes(x = Transfer, y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frequency), vjust = -0.5, size = 4) +
  labs(title = "Frequency of Categories",
       x = "Transfer",
       y = "Frequency") +
  custom_theme(plot_size) +
  ggtitle("Frequency of Transfer Categories")
ggsave(paste0("Results_Findings/","transfer_frequency.png"), 
       plot = plot2, width = plot_width, height = 6, dpi = 300)
########################################################################

write.csv(dd, file = paste0("Results_Findings/","count_data.csv"),
          row.names = FALSE)

