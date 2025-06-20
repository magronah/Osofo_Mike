custom_theme <- function(n) {
  theme_bw(base_size = n) +
    theme(
      plot.title = element_text(hjust = 0.5,size = n, family = "Roboto", color = "black"),
      text = element_text(size = n, family = "Roboto", color = "black"),
      axis.text.x = element_text(family = "Roboto", size = n, color = "black"),
      axis.text.y = element_text(family = "Roboto", size = n, color = "black")
    )
}



seq_func  =  function(data, target_states,
                      plot_title = "Individual Transfer Trajectories"){
  
  transfer_cols  <-  setdiff(names(data), "ID")
  filtered_data <- data[apply(data[transfer_cols], 1, 
                              function(row) {any(row %in% target_states,
                                                 na.rm = TRUE) }), ]
  
  filtered_data[transfer_cols] <- lapply(filtered_data[transfer_cols], function(col) {
    ifelse(col %in% target_states, col, NA)
  })
  
  
  all_states <- unlist(filtered_data[, -1])
  all_states <- all_states[!is.na(all_states)]
  
  category_counts <- sort(table(all_states))
  
  category_counts <- sort(table(all_states))
  category_labels_with_counts <- paste0(names(category_counts), 
                                        " (n = ", category_counts, ")")
  
  names(category_labels_with_counts) <- names(category_counts)
  unique_labels <- sort(unique(all_states))
  
  final_labels <- category_labels_with_counts[unique_labels]
  transfer_colors <- rainbow(length(unique_labels))
  
  if ("NA" %in% unique_labels) {
    transfer_colors[which(unique_labels == "NA")] <- "white"
  }
  
  seq_employed <- seqdef(filtered_data[,-1],
                         alphabet = unique_labels,
                         cpal     = transfer_colors,
                         labels   = final_labels,
                         xtstep   = 1)
  
  seqIplot(seq_employed,
           with.legend = "right",
           main = plot_title,
           cex.legend = 0.6)
  
}
