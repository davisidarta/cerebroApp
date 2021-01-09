##----------------------------------------------------------------------------##
## Collect color parameters for projection plot.
##----------------------------------------------------------------------------##

expression_projection_color_inputs <- reactive({

  ## require input UI elements
  req(
    input[["expression_projection_color_scale"]],
    input[["expression_projection_color_scale_range"]]
  )

  ## collect parameters
  parameters <- list(
    color_scale = input[["expression_projection_color_scale"]],
    color_range = input[["expression_projection_color_scale_range"]]
  )

  message(str(parameters))

  ## return parameters
  return(parameters)
})
