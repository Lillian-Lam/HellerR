significance_label <- function(df, plot_name, x="", y="", bar_order=c(), signif_list, label_sym="†", avoid_labels=c(), step_increase=0.05, label_size=8, base_increase=0.1, existing_ymax=NULL) {
  labels_list <- vector("list", length(bar_order))  # Initialize list to store labels for each bar
  ymax_list <- vector("list", length(bar_order))    # Initialize list to store ymax values for each bar

  # Initialize with current maximum heights of the bars if no existing ymax is provided
  if (is.null(existing_ymax)) {
    existing_ymax <- sapply(1:length(bar_order), function(i) max(ggplot_build(plot_name)$data[[1]][["y"]][i], na.rm=TRUE))
  }

  for (i in 1:length(signif_list)) {
    pair <- signif_list[[i]]  # Extract each pair from the significance list
    p_value <- t.test(df[which(df[[x]] == pair[[1]]), ][[y]], df[which(df[[x]] == pair[[2]]), ][[y]])$p.value  # Perform t-test

    label <- p_value_to_symbol(p_value, label_sym)  # Convert p-value to significance symbol

    index1 <- which(bar_order == pair[[1]])  # Find the index of the first item in the pair
    index2 <- which(bar_order == pair[[2]])  # Find the index of the second item in the pair

    # Add the label to the list if the bar is not in avoid_labels
    if (!(bar_order[index1] %in% avoid_labels)) {
      labels_list[[index1]] <- c(labels_list[[index1]], label)
      ymax_list[[index1]] <- c(ymax_list[[index1]], existing_ymax[index1])
    }

    if (!(bar_order[index2] %in% avoid_labels)) {
      labels_list[[index2]] <- c(labels_list[[index2]], label)
      ymax_list[[index2]] <- c(ymax_list[[index2]], existing_ymax[index2])
    }
  }

  all_labels <- unlist(labels_list)  # Flatten the labels list
  all_positions <- unlist(lapply(seq_along(labels_list), function(i) rep(i, length(labels_list[[i]]))))  # Get positions for the labels
  all_ymax <- unlist(lapply(seq_along(ymax_list), function(i) ymax_list[[i]] + base_increase + seq_along(ymax_list[[i]]) * step_increase))  # Adjust ymax with step increase

  existing_ymax[all_positions] <- all_ymax  # Update existing ymax with new ymax values

  d <- data.frame(
    x = all_positions,  # x positions for the labels
    y = all_ymax,       # y positions for the labels
    label = all_labels  # The labels themselves
  )

  plot_name <- plot_name +
    geom_text(data = d, aes(x = x, y = y, label = label), position = position_dodge(width = 0.9), size = label_size, vjust = -0.5)  # Add labels to the plot

  return(list(plot = plot_name, ymax = existing_ymax))  # Return the plot and updated ymax values
}
