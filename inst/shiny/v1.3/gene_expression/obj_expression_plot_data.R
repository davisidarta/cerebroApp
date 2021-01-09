##----------------------------------------------------------------------------##
## Reactive data that holds data to be plotted.
##----------------------------------------------------------------------------##

expression_plot_data <- reactive({

  ## don't proceed without these inputs
  req(
    expression_projection_inputs(),
    expression_genes_to_plot()
  )

  ## check if projection or trajectory should be shown
  ## ... projection
  if ( expression_projection_inputs()[["projection"]] %in% availableProjections() ) {

    ## build data frame with data
    cells_df <- cbind(
      getProjection(expression_projection_inputs()[["projection"]]),
      getMetaData()
    )

  ## ... trajectory
  } else {

    ## split selection into method and name
    selection <- strsplit(expression_projection_inputs()[["projection"]], split = ' // ')[[1]]

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

    ## merge meta data and trajectory info and remove cells without pseudotime
    cells_df <- cbind(trajectory_data[["meta"]], getMetaData()) %>%
      dplyr::filter(!is.na(pseudotime))
  }

  ## remove cells based on group filters
  for ( i in getGroups() ) {

    ## make sure that group exists in meta data (as column) and that selected
    ## groups are not NULL, then subset the data frame
    if ( i %in% colnames(cells_df) ) {
      cells_df <- cells_df[which(cells_df[[i]] %in% expression_projection_inputs()[["group_filters"]][[ i ]] ),]
    }
  }

  ## randomly remove cells (if necessary)
  cells_df <- randomlySubsetCells(cells_df, expression_projection_inputs()[["pct_cells"]])

  ## get expression values that will be plotted; depends on how many genes are
  ## available
  ## ... no genes are available
  if ( length(expression_genes_to_plot()$genes_to_display_present) == 0 ) {

    ## set expression level to 0
    cells_df$level <- 0

  ## ... at least 1 gene is available
  } else {

    ## check if user requested to show expression in separate panels
    ## ... separate panels requested, at least 2 genes but not more than 8
    ##     genes selected
    if (
      expression_projection_inputs()[["separate_panels"]] == TRUE &&
      expression_projection_inputs()[["projection"]] %in% availableProjections() &&
      ncol(getProjection(expression_projection_inputs()[["projection"]])) == 2 &&
      length(expression_genes_to_plot()$genes_to_display_present) >= 2 &&
      length(expression_genes_to_plot()$genes_to_display_present) <= 8
    ) {

      ## - get expression matrix
      ## - transpose matrix
      ## - convert to data frame with genes as columns and cells as rows
      ## - add projection coordinates (only first two columns because 3D is not
      ##   supported anyway)
      ## - bring data in longer format
      ## NOTE: I don't merge the expression value with cell meta data because
      ##       hover info doesn't work properly anyway so like this the data
      ##       frame stays smaller, especially with large data sets
      cells_df <- getExpressionMatrix(
          cells = cells_df$cell_barcode,
          genes = expression_genes_to_plot()$genes_to_display_present
        ) %>%
        Matrix::t() %>%
        as.data.frame() %>%
        cbind(cells_df[,1:2], .) %>%
        tidyr::pivot_longer(
          cols = tidyselect::all_of(expression_genes_to_plot()$genes_to_display_present),
          names_to = "gene",
          values_to = "level"
        )

    ## ... if proper conditions for separate panels are not met
    } else {

      ## calculate mean across all genes for each cell
      cells_df$level <- getMeanExpressionForCells(
        cells = cells_df$cell_barcode,
        genes = expression_genes_to_plot()$genes_to_display_present
      )
    }
  }

  ## set plotting order, depending on user input
  plot_order <- expression_projection_inputs()[["plot_order"]]

  ## ... if plotting order is random
  if ( plot_order == "Random" ) {

    ## randomize row order
    cells_df <- cells_df[ sample(1:nrow(cells_df), nrow(cells_df)) , ]

  ## ... if plotting order is from high to low
  } else if ( plot_order == "Highest expression on top" ) {

    ## sort rows by expression level from low to high
    cells_df <- cells_df[ order(cells_df$level, decreasing = FALSE) , ]
  }

  message(str(cells_df))

  return(cells_df)
})
