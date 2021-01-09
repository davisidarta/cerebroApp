##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##

expression_projection_selected_cells <- reactive({

  ## make sure plot parameters are set because it means that the plot can be
  ## generated
  req(
    expression_projection_inputs(),
    expression_projection_color_inputs(),
    expression_plot_data()
  )

  ## check selection
  ## ... selection has not been made or there is no cell in it
  if (
    is.null(plotly::event_data("plotly_selected", source = "expression_projection")) ||
    length(plotly::event_data("plotly_selected", source = "expression_projection")) == 0
  ) {

    return(NULL)

  ## ... selection has been made and at least 1 cell is in it
  } else {

    ## get number of selected cells
    plotly::event_data("plotly_selected", source = "expression_projection") %>%
    dplyr::mutate(identifier = paste0(x, '-', y)) %>%
    return()
  }
})
