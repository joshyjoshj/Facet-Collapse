library(tidyverse)

# facet wrap


collapse_facet_wrap <- function(plot) {
  # Extracting Facet Argument from plot data
  facets <- plot[["facet"]][["params"]][["facets"]]

  # Verifying the facets are contained in the plot data

  if (is.null(facets)) {
    stop("Error the plot does not contain a facet object")
  }

  # Extracting groups from plot data
  groups <- plot$data %>%
    select(!!!facets) %>%
    unique() %>%
    pull() %>%
    sort()

  # Printing the plots
  for (i in 1:length(groups)) {
    if (i == 2) {
      devAskNewPage(TRUE)
    } # Setting Ask for new page the True for each new plot if there is more than one
    plot_out <- plot # Creating an Output plot
    plot_out$data <- plot_out$data %>% filter(!!facets[[1]] == groups[i]) # Filtering data for groups
    print(plot_out) # Rendering plot
  }
  devAskNewPage(options("device.ask.default")[[1]]) # Returning to default page settings
}


# facet grid

collapse_facet_grid <- function(plot) {
  # Extracting rows and cols from plot data
  facet_rows <- plot[["facet"]][["params"]][["rows"]]
  facet_cols <- plot[["facet"]][["params"]][["cols"]]

  # Verifying facets and cols are contained in the plot data
  if (is.null(facet_rows) | is.null(facet_cols)) {
    stop("Error the plot does not contain a rows or cols object")
  }

  # Extracting rows and groups

  row_groups <- p$data %>%
    select(!!!facet_rows) %>%
    unique() %>%
    pull() %>%
    sort()
  col_groups <- p$data %>%
    select(!!!facet_cols) %>%
    unique() %>%
    pull() %>%
    sort()

  for (i in 1:length(row_groups)) {
    for (j in 1:length(col_groups)) {
      if (i * j == 2) {
        devAskNewPage(TRUE)
      } # Setting Ask for new page the True for each new plot if there is more than one
      plot_out <- plot # Creating an Output plot
      plot_out$data <- plot_out$data %>% filter(!!facet_rows[[1]] == row_groups[i], !!facet_cols[[1]] == col_groups[j]) ## Filtering data for groups
      if (nrow(plot_out$data) == 0) {
        next
      } # If data is null skip iteration
      print(plot_out) # Rendering plot
    }
  }

  devAskNewPage(options("device.ask.default")[[1]]) # Returning to default page settings
}
