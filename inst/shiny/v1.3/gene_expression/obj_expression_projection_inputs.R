##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##

expression_projection_inputs <- reactive({

  ## require input UI elements
  req(
    input[["expression_projection_to_display"]],
    input[["expression_projection_plotting_order"]],
    input[["expression_projection_point_size"]],
    input[["expression_projection_point_opacity"]],
    input[["expression_projection_percentage_cells_to_show"]],
    !is.null(input[["expression_projection_point_border"]]),
    !is.null(input[["expression_projection_genes_in_separate_panels"]]),
    input[["expression_projection_scale_x_manual_range"]],
    input[["expression_projection_scale_y_manual_range"]],
    !is.null(preferences[["use_webgl"]]),
    !is.null(preferences[["show_hover_info_in_projections"]])
  )

  ## require group filters UI elements and at least 1 group level to be selected
  for ( i in getGroups() ) {
    req(input[[paste0("expression_projection_group_filter_", i)]])
  }

  ## collect parameters
  parameters <- list(
    projection = input[["expression_projection_to_display"]],
    plot_order = input[["expression_projection_plotting_order"]],
    point_size = input[["expression_projection_point_size"]],
    point_opacity = input[["expression_projection_point_opacity"]],
    pct_cells = input[["expression_projection_percentage_cells_to_show"]],
    draw_border = input[["expression_projection_point_border"]],
    separate_panels = input[["expression_projection_genes_in_separate_panels"]],
    x_range = input[["expression_projection_scale_x_manual_range"]],
    y_range = input[["expression_projection_scale_y_manual_range"]],
    group_filters = list(),
    webgl = preferences[["use_webgl"]],
    hover_info = preferences[["show_hover_info_in_projections"]]
  )

  ## store group filters
  for ( i in getGroups() ) {
    parameters[['group_filters']][[ i ]] <- input[[paste0("expression_projection_group_filter_", i)]]
  }

  message(str(parameters))

  ## return parameters
  return(parameters)
})
