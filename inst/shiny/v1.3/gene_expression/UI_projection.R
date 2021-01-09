##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##
## Expression in projection.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element with layout for user input and plot.
##----------------------------------------------------------------------------##

output[["expression_projection_UI"]] <- renderUI({
  fluidRow(
    column(
      width = 3, offset = 0, style = "padding: 0px;",
      tagList(
        cerebroBox(
          title = tagList(
            "Main parameters",
            actionButton(
              inputId = "expression_projection_main_parameters_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px"
            )
          ),
          tagList(
            shinyWidgets::radioGroupButtons(
               inputId = "expression_analysis_mode",
               label = NULL,
               choices = c("Gene(s)", "Gene set"),
               status = "primary",
               justified = TRUE,
               width = "100%"
            ),
            uiOutput("expression_projection_input_type_UI"),
            uiOutput("expression_projection_select_projection_UI")
          )
        ),
        cerebroBox(
          title = tagList(
            "Additional parameters",
            actionButton(
              inputId = "expression_projection_additional_parameters_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px"
            )
          ),
          uiOutput("expression_projection_select_additional_parameters_UI"),
          collapsed = TRUE
        ),
        cerebroBox(
          title = tagList(
            "Group filters",
            actionButton(
              inputId = "expression_projection_group_filters_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px"
            )
          ),
          uiOutput("expression_projection_group_filters_UI"),
          collapsed = TRUE
        ),
        cerebroBox(
          title = tagList(
            "Color scale",
            actionButton(
              inputId = "expression_projection_color_scale_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px"
            )
          ),
          tagList(
            uiOutput("expression_projection_color_scale_UI"),
            uiOutput("expression_projection_color_scale_range_UI"),
          ),
          collapsed = TRUE
        )
      )
    ),
    column(
      width = 9, offset = 0, style = "padding: 0px;",
      cerebroBox(
        title = tagList(
          boxTitle("Dimensional reduction"),
          tagList(
            actionButton(
              inputId = "expression_projection_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-right: 3px"
            ),
            shinyFiles::shinySaveButton(
              "expression_projection_export",
              label = "export to PDF",
              title = "Export dimensional reduction to PDF file.",
              filetype = "pdf",
              viewtype = "icon",
              class = "btn-xs",
              style = "margin-right: 3px"
            ),
            shinyWidgets::dropdownButton(
              tags$div(
                tags$style(
                  HTML("div.awesome-checkbox {margin-top: 10px;}")
                ),
                style = "color: black !important;",
                tagList(
                  uiOutput("expression_projection_point_border_UI"),
                  uiOutput("expression_projection_genes_in_separate_panels_UI"),
                  uiOutput("expression_projection_scales_UI")
                )
              ),
              circle = FALSE,
              icon = icon("cog"),
              inline = TRUE,
              size = "xs"
            )
          )
        ),
        tagList(
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(
              "expression_projection",
              width = "auto",
              height = "85vh"
            ),
            type = 8,
            hide.ui = FALSE
          ),
          tags$br(),
          htmlOutput("expression_number_of_selected_cells"),
          tags$br(),
          htmlOutput("expression_genes_displayed")
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI elements to choose whether gene(s) or gene sets should be analyzed
##----------------------------------------------------------------------------##

output[["expression_projection_input_type_UI"]] <- renderUI({

  req(
    input[["expression_analysis_mode"]]
  )

  if ( input[["expression_analysis_mode"]] == "Gene(s)" ) {
    selectizeInput(
      'expression_genes_input',
      label = 'Gene(s)',
      choices = data.table::as.data.table(data.frame("Genes" = getGeneNames())),
      multiple = TRUE,
      options = list(
        create = TRUE
      )
    )
  } else if ( input[["expression_analysis_mode"]] == "Gene set" ) {
    selectizeInput(
      'expression_select_gene_set',
      label = 'Gene set',
      choices = data.table::as.data.table(
        data.frame("Gene sets" = c("-", msigdbr:::msigdbr_genesets$gs_name))
      ),
      multiple = FALSE
    )
  }
})

##----------------------------------------------------------------------------##
## UI elements to choose which projection/trajectory to show.
##----------------------------------------------------------------------------##

output[["expression_projection_select_projection_UI"]] <- renderUI({

  ## get available projections
  available_projections <- availableProjections()

  ## collect available trajectories across all methods and create selectable
  ## options
  available_trajectories <- c()
  available_trajectory_method <- getMethodsForTrajectories()

  ## check if at least 1 trajectory method exists
  if ( length(available_trajectory_method) > 0 ) {

    ## cycle through trajectory methods
    for ( i in seq_along(available_trajectory_method) ) {

      ## get current method and names of trajectories for this method
      current_method <- available_trajectory_method[i]
      available_trajectories_for_this_method <- getNamesOfTrajectories(current_method)

      ## check if at least 1 trajectory is available for this method
      if ( length(available_trajectories_for_this_method) > 0 ) {

        ## cycle through trajectories for this method
        for ( j in seq_along(available_trajectories_for_this_method) ) {

          ## create selectable combination of method and trajectory name and add
          ## it to the available trajectories
          current_trajectory <- available_trajectories_for_this_method[j]
          available_trajectories <- c(
            available_trajectories,
            glue::glue("{current_method} // {current_trajectory}")
          )
        }
      }
    }
  }

  selectInput(
    "expression_projection_to_display",
    label = "Projection",
    choices = list(
      "Projections" = as.list(available_projections),
      "Trajectories" = as.list(available_trajectories)
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["expression_projection_main_parameters_info"]], {
  showModal(
    modalDialog(
      expression_projection_main_parameters_info$text,
      title = expression_projection_main_parameters_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

expression_projection_main_parameters_info <- list(
  title = "Main parameters for gene (set) expression",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Gene(s) / Gene set:</b> Select whether you would like to select individual genes or gene sets. In the case of 'Gene(s)', you can select one or multiple genes from the input field below. If you select multiple genes, the mean expression across the selected genes will be calculated for each cell. If you select 'Gene set', you can select a gene set from the MSigDB. Species-specific gene names will be tried to retrieve, otherwise gene name matching is attempted. A list of which genes are present or missing in the data set can be found below the projection.</li>
      <li><b>Projection:</b> Select here which projection you want to see in the scatter plot on the right.</li>
    </ul>
    "
  )
)

##----------------------------------------------------------------------------##
## UI elements to set additional plotting parameters.
##----------------------------------------------------------------------------##

output[["expression_projection_select_additional_parameters_UI"]] <- renderUI({
  tagList(
    selectInput(
      "expression_projection_plotting_order",
      label = "Plotting order",
      choices = c("Random", "Highest expression on top"),
      selected = "Random"
    ),
    sliderInput(
      "expression_projection_point_size",
      label = "Point size",
      min = scatter_plot_point_size[["min"]],
      max = scatter_plot_point_size[["max"]],
      step = scatter_plot_point_size[["step"]],
      value = scatter_plot_point_size[["default"]]
    ),
    sliderInput(
      "expression_projection_point_opacity",
      label = "Point opacity",
      min = scatter_plot_point_opacity[["min"]],
      max = scatter_plot_point_opacity[["max"]],
      step = scatter_plot_point_opacity[["step"]],
      value = scatter_plot_point_opacity[["default"]]
    ),
    sliderInput(
      "expression_projection_percentage_cells_to_show",
      label = "Show % of cells",
      min = scatter_plot_percentage_cells_to_show[["min"]],
      max = scatter_plot_percentage_cells_to_show[["max"]],
      step = scatter_plot_percentage_cells_to_show[["step"]],
      value = scatter_plot_percentage_cells_to_show[["default"]]
    )
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_select_additional_parameters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["expression_projection_additional_parameters_info"]], {
  showModal(
    modalDialog(
      expression_projection_additional_parameters_info$text,
      title = expression_projection_additional_parameters_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

expression_projection_additional_parameters_info <- list(
  title = "Additional parameters for gene (set) expression",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Plotting order:</b> Cells can be plotted in random order or so that cells with highest expression are on top.</li>
      <li><b>Point size:</b> Controls how large the cells should be.</li>
      <li><b>Point opacity:</b> Controls the transparency of the cells.</li>
      <li><b>Show % of cells:</b> Using the slider, you can randomly remove a fraction of cells from the plot. This can be useful for large data sets and/or computers with limited resources.</li>
    </ul>
    "
  )
)

##----------------------------------------------------------------------------##
## UI elements to set group filters.
##----------------------------------------------------------------------------##

output[["expression_projection_group_filters_UI"]] <- renderUI({

  group_filters <- list()

  for ( i in getGroups() ) {
    group_filters[[i]] <- shinyWidgets::pickerInput(
      paste0("expression_projection_group_filter_", i),
      label = i,
      choices = getGroupLevels(i),
      selected = getGroupLevels(i),
      options = list(
        "actions-box" = TRUE
      ),
      multiple = TRUE
    )
  }

  group_filters
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_group_filters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["expression_projection_group_filters_info"]], {
  showModal(
    modalDialog(
      expression_projection_group_filters_info$text,
      title = expression_projection_group_filters_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
# <li><b>Range of X/Y axis (located in dropdown menu above the projection):</b> Set the X/Y axis limits. This is useful when you want to change the aspect ratio of the plot.</li>

expression_projection_group_filters_info <- list(
  title = "Group filters for gene (set) expression",
  text = HTML("
    The elements in this panel allow you to select which cells should be plotted based on the group(s) they belong to. For each grouping variable, you can activate or deactivate group levels. Only cells that are pass all filters (for each grouping variable) are shown in the projection, the expression by group, and expression by pseudotime (if applicable).
    "
  )
)

##----------------------------------------------------------------------------##
## UI elements to set color scale.
##----------------------------------------------------------------------------##

output[["expression_projection_color_scale_UI"]] <- renderUI({
  selectInput(
    "expression_projection_color_scale",
    label = "Color scale",
    choices = c("YlGnBu", "YlOrRd","Blues","Greens","Reds","RdBu","Viridis"),
    selected = "YlGnBu"
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_color_scale_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## UI elements to set color scale range.
##----------------------------------------------------------------------------##

output[["expression_projection_color_scale_range_UI"]] <- renderUI({

  ##
  req(
    expression_plot_data()
  )

  ## get range of expression levels
  expression_range <- range(expression_plot_data()$level)

  ## adjust expression range for color scale
  ## ... there is no range (from 0 to 0)
  if (
    expression_range[1] == 0 &&
    expression_range[2] == 0
  ) {

    ## set range to 0-1
    expression_range[2] <- 1

  ## ... otherwise
  } else {

    ## round min and max values to 2 digits
    expression_range <- round(expression_range, digits = 2)
  }

  sliderInput(
    "expression_projection_color_scale_range",
    label = "Range of color scale",
    min = expression_range[1],
    max = expression_range[2],
    value = expression_range
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_color_scale_range_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["expression_projection_color_scale_info"]], {
  showModal(
    modalDialog(
      expression_projection_color_scale_info$text,
      title = expression_projection_color_scale_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

expression_projection_color_scale_info <- list(
  title = "Color scale for gene (set) expression",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Color scale:</b> Choose your prefered color scale.</li>
      <li><b>Range of color scale:</b> Using the sliders, you can set the limits for the color scale. Values outside the scale will be shown in the color corresponding to the min/max value, respectively.</li>
    </ul>
    "
  )
)

##----------------------------------------------------------------------------##
## UI elements with switch to draw border around cells.
##----------------------------------------------------------------------------##

output[["expression_projection_point_border_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "expression_projection_point_border",
    label = "Draw border around cells",
    value = FALSE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_point_border_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## UI elements with switch to plot genes in separate panels.
##----------------------------------------------------------------------------##

output[["expression_projection_genes_in_separate_panels_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "expression_projection_genes_in_separate_panels",
    label = HTML("Show genes in separate panels<br>(experimental)"),
    value = FALSE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_genes_in_separate_panels_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## UI elements to set X and Y scales in plot. Separate element because it
## requires user input from other UI elements.
##----------------------------------------------------------------------------##
output[["expression_projection_scales_UI"]] <- renderUI({

  req(input[["expression_projection_to_display"]])

  ##
  if (
    is.null(input[["expression_projection_to_display"]]) ||
    is.na(input[["expression_projection_to_display"]])
  ) {
    projection_to_display <- availableProjections()[1]
  } else {
    projection_to_display <- input[["expression_projection_to_display"]]
  }

  ## check if projection or trajectory should be shown
  ## ... projection
  if ( input[["expression_projection_to_display"]] %in% availableProjections() ) {

    ##
    XYranges <- getXYranges(getProjection(input[["expression_projection_to_display"]]))

  ## ... trajectory
  } else {

    ## split selection into method and name
    selection <- strsplit(input[["expression_projection_to_display"]], split = ' // ')[[1]]

    ## check if method and name exist and don't proceed if not
    req(
      selection[1] %in% getMethodsForTrajectories(),
      selection[2] %in% getNamesOfTrajectories(selection[1])
    )

    ## collect trajectory data
    trajectory_data <- getTrajectory(
      selection[1],
      selection[2]
    )

    ##
    XYranges <- getXYranges(trajectory_data[["meta"]])
  }

  ##
  tagList(
    sliderInput(
      "expression_projection_scale_x_manual_range",
      label = "Range of X axis",
      min = XYranges$x$min,
      max = XYranges$x$max,
      value = c(XYranges$x$min, XYranges$x$max)
    ),
    sliderInput(
      "expression_projection_scale_y_manual_range",
      label = "Range of Y axis",
      min = XYranges$y$min,
      max = XYranges$y$max,
      value = c(XYranges$y$min, XYranges$y$max)
    )
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_scales_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Plot of projection.
##----------------------------------------------------------------------------##

output[["expression_projection"]] <- plotly::renderPlotly({

  ## don't proceed without these inputs
  req(
    expression_projection_inputs(),
    expression_projection_color_inputs(),
    expression_plot_data()
  )

  ##
  parameters <- expression_projection_inputs()
  parameters_color <- expression_projection_color_inputs()

  ## isolate() because we don't want the plot to update when the expression data
  ## updates; instead, it will be updated when the color range changes, which is
  ## triggered to be updated with a change in the expression data
  cells_df <- isolate(expression_plot_data())

  ##
  projection_to_display <- parameters[["projection"]]

  # ## check if border around cells should be drawn and set parameters if so
  # if ( parameters[["draw_border"]] == TRUE ) {
  #   point_border <- list(
  #     color = "rgb(196,196,196)",
  #     width = 1
  #   )
  # } else {
  #   point_border <- NULL
  # }

  # ## bring cells in order, either random or highest expression on top
  # ## ... random
  # if ( parameters[["plot_order"]] == 'Random' ) {
  #   cells_df <- cells_df[ sample(1:nrow(cells_df)) , ]

  # ## ... highest expression on top
  # } else if ( parameters[["plot_order"]] == "Highest expression on top" ) {
  #   cells_df <- dplyr::arrange(cells_df, level)
  # }


  # ## prepare hover info
  # if ( parameters[["hover_info"]] == TRUE ) {

  #   hover_info <- buildHoverInfoForProjections(cells_df)

  #   ## add expression levels to hover info
  #   hover_info <- glue::glue(
  #     "{hover_info}
  #     <b>Expression level</b>: {formatC(cells_df$level, format = 'f', digits = 3)}"
  #   )

  #   ##
  #   parameter_hoverinfo <- "text"
  #   parameter_text <- ~hover_info

  # ##
  # } else {

  #   hover_info <- NULL
  #   parameter_hoverinfo <- "skip"
  #   parameter_text <- NULL
  # }


  ## what kind of plot
  ## ... projection, 2D, single panel
  if (
    parameters[["projection"]] %in% availableProjections() &&
    ncol(getProjection(parameters[["projection"]])) == 2 &&
    parameters[["separate_panels"]] == FALSE &&
    "gene" %in% colnames(cells_df) == FALSE
  ) {

    plot <- pltExpProj2DSglPan(
      df = cells_df,
      point_size = parameters[["point_size"]],
      point_opacity = parameters[["point_opacity"]],
      draw_border = parameters[["draw_border"]],
      plot_order = parameters[["plot_order"]],
      color_scale = parameters_color[["color_scale"]],
      color_range = parameters_color[["color_range"]],
      x_range = parameters[["x_range"]],
      y_range = parameters[["y_range"]],
      show_hover_info = parameters[["hover_info"]]
    )

  ## ... projection, 2D, multiple panels
  } else if (
    parameters[["projection"]] %in% availableProjections() &&
    ncol(getProjection(parameters[["projection"]])) == 2 &&
    parameters[["separate_panels"]] == TRUE &&
    "gene" %in% colnames(cells_df) == TRUE
  ) {

    plot <- pltExpProj2DMultPan(
      df = cells_df,
      point_size = parameters[["point_size"]],
      point_opacity = parameters[["point_opacity"]],
      plot_order = parameters[["plot_order"]],
      color_scale = parameters_color[["color_scale"]],
      color_range = parameters_color[["color_range"]],
      x_range = parameters[["x_range"]],
      y_range = parameters[["y_range"]]
    )

    ## convert ggplot to plotly
    plot <- plotly::ggplotly(
      plot,
      source = "expression_projection"
    )

  ## ... projection, 3D
  } else if (
    parameters[["projection"]] %in% availableProjections() &&
    ncol(getProjection(parameters[["projection"]])) == 3 &&
    parameters[["separate_panels"]] == FALSE &&
    "gene" %in% colnames(cells_df) == FALSE
  ) {

    plot <- pltExpProj3DSglPan(
      df = cells_df,
      point_size = parameters[["point_size"]],
      point_opacity = parameters[["point_opacity"]],
      draw_border = parameters[["draw_border"]],
      plot_order = parameters[["plot_order"]],
      color_scale = parameters_color[["color_scale"]],
      color_range = parameters_color[["color_range"]],
      x_range = parameters[["x_range"]],
      y_range = parameters[["y_range"]],
      show_hover_info = parameters[["hover_info"]]
    )

  ## ... trajectory, 2D, single panel
  } else if (
    parameters[["projection"]] %in% availableProjections() == FALSE &&
    grepl(parameters[["projection"]], pattern = ' // ') &&
    parameters[["separate_panels"]] == FALSE &&
    "gene" %in% colnames(cells_df) == FALSE
  ) {

    ## split selection into method and name
    selection <- strsplit(projection_to_display, split = ' // ')[[1]]

    ##
    req(
      selection[1] %in% getMethodsForTrajectories(),
      selection[2] %in% getNamesOfTrajectories(selection[1])
    )

    ## collect trajectory data
    trajectory_data <- getTrajectory(
      selection[1],
      selection[2]
    )

    ##
    plot <- pltExpTrj2DSglPan(
      df = cells_df,
      trajectory_edges = trajectory_data[["edges"]],
      point_size = parameters[["point_size"]],
      point_opacity = parameters[["point_opacity"]],
      draw_border = parameters[["draw_border"]],
      plot_order = parameters[["plot_order"]],
      color_scale = parameters_color[["color_scale"]],
      color_range = parameters_color[["color_range"]],
      x_range = parameters[["x_range"]],
      y_range = parameters[["y_range"]],
      show_hover_info = parameters[["hover_info"]]
    )

  ## ... unrecognized state
  } else {
    plot <- plotly::plotly_empty()
  }










  # ## check if projection or trajectory should be shown
  # ## ... projection
  # if ( projection_to_display %in% availableProjections() ) {

  #   ## check if user requested to show expression in separate panels
  #   ## ... separate panels requested, two-dimensional projection selected, and
  #   ##     "gene" column present
  #   if (
  #     ncol(getProjection(projection_to_display)) == 2 &&
  #     parameters[["separate_panels"]] == TRUE &&
  #     "gene" %in% colnames(cells_df) == TRUE
  #   ) {

  #     ## prepare plot
  #     plot <- pltExpProj2DMultPan(
  #       df = cells_df,
  #       point_size = parameters[["point_size"]],
  #       point_opacity = parameters[["point_opacity"]],
  #       color_scale = parameters_color[["color_scale"]],
  #       color_range = parameters_color[["color_range"]],
  #       x_range = parameters[["x_range"]],
  #       y_range = parameters[["y_range"]]
  #     )

  #     ## convert ggplot to plotly
  #     plot <- plotly::ggplotly(plot)

  #   ## ... if conditions for multiple panels are not met
  #   } else {

  #     ## prepare hover info
  #     if ( parameters[["hover_info"]] == TRUE ) {

  #       hover_info <- buildHoverInfoForProjections(cells_df)

  #       ## add expression levels to hover info
  #       hover_info <- glue::glue(
  #         "{hover_info}
  #         <b>Expression level</b>: {formatC(cells_df$level, format = 'f', digits = 3)}"
  #       )

  #       ##
  #       parameter_hoverinfo <- "text"
  #       parameter_text <- ~hover_info

  #     ##
  #     } else {

  #       hover_info <- NULL
  #       parameter_hoverinfo <- "skip"
  #       parameter_text <- NULL
  #     }

  #     ## check if selection projection consists of 2 or 3 dimensions
  #     ## ... selection projection consists of 2 dimensions
  #     if ( ncol(getProjection(projection_to_display)) == 2 ) {

  #       ## prepare plot
  #       plot <- pltExpProj2DSglPan(
  #         df = cells_df,
  #         point_size = parameters[["point_size"]],
  #         point_opacity = parameters[["point_opacity"]],
  #         point_border = point_border,
  #         color_scale = parameters_color[["color_scale"]],
  #         color_range = parameters_color[["color_range"]],
  #         x_range = parameters[["x_range"]],
  #         y_range = parameters[["y_range"]],
  #         parameter_hoverinfo = parameter_hoverinfo,
  #         parameter_text = parameter_text,
  #         hover_info = hover_info
  #       )

  #     ## ... selection projection consists of 3 dimensions
  #     } else if ( ncol(getProjection(projection_to_display)) == 3 ) {

  #       ## prepare plot
  #       plot <- pltExpProj3DSglPan(
  #         df = cells_df,
  #         point_size = parameters[["point_size"]],
  #         point_opacity = parameters[["point_opacity"]],
  #         point_border = point_border,
  #         color_scale = parameters_color[["color_scale"]],
  #         color_range = parameters_color[["color_range"]],
  #         x_range = parameters[["x_range"]],
  #         y_range = parameters[["y_range"]],
  #         parameter_hoverinfo = parameter_hoverinfo,
  #         parameter_text = parameter_text,
  #         hover_info = hover_info
  #       )
  #     }
  #   }

  # ## ... trajectory
  # } else {

  #   ## split selection into method and name
  #   selection <- strsplit(projection_to_display, split = ' // ')[[1]]

  #   req(
  #     selection[1] %in% getMethodsForTrajectories(),
  #     selection[2] %in% getNamesOfTrajectories(selection[1])
  #   )

  #   ## collect trajectory data
  #   trajectory_data <- getTrajectory(
  #     selection[1],
  #     selection[2]
  #   )

  #   ## prepare hover info
  #   if ( parameters[["hover_info"]] == TRUE ) {

  #     hover_info <- buildHoverInfoForProjections(cells_df)

  #     ## add expression levels to hover info
  #     hover_info <- glue::glue(
  #       "{hover_info}
  #       <b>State</b>: {cells_df$state}
  #       <b>Pseudotime</b>: {formatC(cells_df$pseudotime, format = 'f', digits = 2)}
  #       <b>Expression level</b>: {formatC(cells_df$level, format = 'f', digits = 3)}"
  #     )

  #     ##
  #     parameter_hoverinfo <- "text"
  #     parameter_text <- ~hover_info

  #   ##
  #   } else {

  #     hover_info <- NULL
  #     parameter_hoverinfo <- "skip"
  #     parameter_text <- NULL
  #   }

  #   ##
  #   plot <- pltExpTrj2DSglPan(
  #     df = cells_df,
  #     trajectory_edges = trajectory_data[["edges"]],
  #     point_size = parameters[["point_size"]],
  #     point_opacity = parameters[["point_opacity"]],
  #     point_border = point_border,
  #     color_scale = parameters_color[["color_scale"]],
  #     color_range = parameters_color[["color_range"]],
  #     x_range = parameters[["x_range"]],
  #     y_range = parameters[["y_range"]],
  #     parameter_hoverinfo = parameter_hoverinfo,
  #     parameter_text = parameter_text,
  #     hover_info = hover_info
  #   )
  # }

  ## return plot either with WebGL or without, depending on setting
  if ( parameters[["webgl"]] == TRUE ) {
    plot %>% plotly::toWebGL()
  } else {
    plot
  }
})

##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##

output[["expression_number_of_selected_cells"]] <- renderText({

  ## check selection
  ## ... selection has not been made or there is no cell in it
  if ( is.null(expression_projection_selected_cells()) ) {

    ## manually set counter to 0
    number_of_selected_cells <- 0

  ## ... selection has been made and at least 1 cell is in it
  } else {

    ## get number of selected cells
    number_of_selected_cells <- expression_projection_selected_cells() %>%
      nrow() %>%
      formatC(format = "f", big.mark = ",", digits = 0)
  }

  ## prepare string to show
  paste0("<b>Number of selected cells</b>: ", number_of_selected_cells)
})

##----------------------------------------------------------------------------##
## Text showing which genes are present and missing.
##----------------------------------------------------------------------------##

output[["expression_genes_displayed"]] <- renderText({

  ## don't proceed without these inputs
  req(
    expression_genes_to_plot()
  )

  ## prepare text output from reactive data
  paste0(
    "<b>Showing expression for ",
    length(expression_genes_to_plot()[["genes_to_display_present"]]), " gene(s):</b><br>",
    paste0(expression_genes_to_plot()[["genes_to_display_present"]], collapse = ", "),
    "<br><br><b>",
    length(expression_genes_to_plot()[["genes_to_display_missing"]]),
    " gene(s) are not in data set: </b><br>",
    paste0(expression_genes_to_plot()[["genes_to_display_missing"]], collapse = ", ")
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["expression_projection_info"]], {
  showModal(
    modalDialog(
      expression_projection_info$text,
      title = expression_projection_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

expression_projection_info <- list(
  title = "Dimensional reduction",
  text = HTML("
    Interactive projection of cells into two- or three-dimensional space based on their expression profile.<br>
    <ul>
      <li>Both tSNE and UMAP are frequently used algorithms for dimensional reduction in single cell transcriptomics. While they generally allow to make similar conclusions, some differences exist between the two (please refer to Google and/or literature, such as Becht E. et al., Dimensionality reduction for visualizing single-cell data using UMAP. Nature Biotechnology, 2018, 37, 38-44).</li>
      <li>Cell color reflects the log-normalised expression of entered genes. If more than 1 gene is entered or a gene set is selected, the color reflects the average expression of all genes. Genes must be in separate lines or separated by a space, comma, or semicolon. Reported below the projection are the genes that are present and absent in this data set. Absent genes could either have been annotated with a different name or were not expressed in any of the cells. Matching of gene names is case-insensitive, that means Myc/MYC/myc are treated equally.</li>
      <li>Cells can be plotted either randomly (to give a more unbiased perspective) or in the order of expression (with highest expression plotted last), sometimes resulting in a more appealing figure.</li>
      <li>The last two slider elements on the left can be used to resize the projection axes. This can be particularly useful when a projection contains a population of cell that is very far away from the rest and therefore creates a big empty space (which is not uncommon for UMAPs).</li>
    </ul>
    The plot is interactive (drag and zoom) but depending on the computer of the user and the number of cells displayed it can become very slow.<br>
    <h4>Experimental options</h4>
    Experimental options can be accessed from the gear icon next to the 'export to PDF' button.<br>
    <b>Show genes in separate panels</b><br>
    When selecting multiple genes as input (up to 8), this option will show the expression of each gene in a separate panel instead of calculating the mean expression across all genes. The option is labeled as 'experimental' because of its poor implementation:
    <ol>
      <li>Hovering over the cells shows only limited information.</li>
      <li>Cells cannot be shown with a (grey) border around them.</li>
      <li>All genes have the some color scale.</li>
      <li>Cells cannot be selected.</li>
      <li>The 'Expression by group' panel needs to be deactivated.</li>
      <li>It throws annoying but innocent warning messages in the log.</li>
    </ol>
    Yet, this feature might be useful in some situations.
    "
  )
)

##----------------------------------------------------------------------------##
## Export projection plot to PDF when pressing the "export to PDF" button.
##----------------------------------------------------------------------------##

observeEvent(input[["expression_projection_export"]], {

  ## don't proceed without these inputs
  req(
    expression_projection_inputs(),
    expression_projection_color_inputs(),
    expression_plot_data()
  )

  ##
  parameters <- expression_projection_inputs()
  parameters_color <- expression_projection_color_inputs()

  ##
  cells_df <- expression_plot_data()

  ## open dialog to select where plot should be saved and how the file should
  ## be named
  shinyFiles::shinyFileSave(
    input,
    id = "expression_projection_export",
    roots = available_storage_volumes,
    session = session,
    restrictions = system.file(package = "base")
  )

  ## retrieve info from dialog
  save_file_input <- shinyFiles::parseSavePath(available_storage_volumes, input[["expression_projection_export"]])

  ## only proceed if a path has been provided
  req(
    nrow(save_file_input) > 0
  )

  ## extract specified file path
  save_file_path <- as.character(save_file_input$datapath[1])

  ## check if projection or trajectory should be shown
  ## ... projection
  if ( parameters[["projection"]] %in% availableProjections() ) {

    ## check if selection projection consists of 2 or 3 dimensions
    ## ... selection projection consists of 3 dimensions
    if ( ncol(getProjection(parameters[["projection"]])) == 3 ) {

      ## give error message
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Sorry!",
        text = "It's currently not possible to create PDF plots from 3D dimensional reductions. Please use the PNG export button in the panel or a 2D dimensional reduction instead.",
        type = "error"
      )

    ## ... selection projection consists of 2 dimensions
    } else if ( ncol(getProjection(parameters[["projection"]])) == 2 ) {

      ## ... separate panels requested and "gene" column present
      if (
        input[["expression_projection_genes_in_separate_panels"]] == TRUE &&
        "gene" %in% colnames(cells_df) == TRUE
      ) {

        ## prepare plot
        plot <- pltExpProj2DMultPanExp(
          df = cells_df,
          point_size = parameters[["point_size"]],
          point_opacity = parameters[["point_opacity"]],
          point_border = parameters[["draw_border"]],
          color_scale = parameters_color[["color_scale"]],
          color_range = parameters_color[["color_range"]],
          x_range = parameters[["x_range"]],
          y_range = parameters[["y_range"]]
        )

      ## ...
      } else {

        ## prepare plot
        plot <- pltExpProj2DSglPanExp(
          df = cells_df,
          point_size = parameters[["point_size"]],
          point_opacity = parameters[["point_opacity"]],
          point_border = parameters[["draw_border"]],
          color_scale = parameters_color[["color_scale"]],
          color_range = parameters_color[["color_range"]],
          x_range = parameters[["x_range"]],
          y_range = parameters[["y_range"]]
        )
      }
    }

  ## ... trajectory
  } else {

    ## split selection into method and name
    selection <- strsplit(parameters[["projection"]], split = ' // ')[[1]]

    req(
      selection[1] %in% getMethodsForTrajectories(),
      selection[2] %in% getNamesOfTrajectories(selection[1])
    )

    ## collect trajectory data
    trajectory_data <- getTrajectory(
      selection[1],
      selection[2]
    )

    ## prepare plot
    plot <- pltExpTrj2DSglPanExp(
      df = cells_df,
      trajectory_edges = trajectory_data[["edges"]],
      point_size = parameters[["point_size"]],
      point_opacity = parameters[["point_opacity"]],
      point_border = parameters[["draw_border"]],
      color_scale = parameters_color[["color_scale"]],
      color_range = parameters_color[["color_range"]],
      x_range = parameters[["x_range"]],
      y_range = parameters[["y_range"]]
    )
  }

  ## plot must be a ggplot object, otherwise don't proceed
  req(
    is.ggplot(plot)
  )

  ## save plot
  pdf(NULL)
  ggsave(save_file_path, plot, height = 8, width = 11)

  ## check if file was succesfully saved
  ## ... successful
  if ( file.exists(save_file_path) ) {

    ## give positive message
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Success!",
      text = paste0("Plot saved successfully as: ", save_file_path),
      type = "success"
    )

  ## ... failed
  } else {

    ## give negative message
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error!",
      text = "Sorry, it seems something went wrong...",
      type = "error"
    )
  }
})
