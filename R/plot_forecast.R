
#' @title Plot the forecast of an Echo State Network.
#' 
#' @description Autoplot the forecasts from an Echo State Network (univariate and multivariate models).
#' 
#' @param object An object of class "forecast". The result of a call to forecast_esn(...).
#' @param include Integer value. Number of actual values (and fitted values) to be included in the graph.
#' @param add_test A tsibble containing the test data (hold-out sample).
#' @param add_paths Logical value. If TRUE, simulations are plotted.
#' @param add_fitted Logical value. If TRUE, fitted values are plotted along the actuals.
#' @param add_legend Logical value. If TRUE, a legend is added.
#' @param legend_position Character value. Position of the legend ("right", "left", "top", "bottom").
#' @param title Title for the plot.
#' @param subtitle Subtitle for the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param color_fcst Line color of the point forecasts.
#' @param color_fitted Line color of the fitted values.
#' @param color_actual Line color of the actual values.
#' @param color_test Line color of test data.
#' @param color_paths Line color of the simulated future sample paths.
#' @param size_fcst Line width of point forecast.
#' @param size_fitted Line width of fitted values.
#' @param size_actual Lines width of actual values.
#' @param size_test Line width of test data.
#' @param size_paths Line width of simulated future sample paths.
#' 
#' @return p An object of class ggplot.

plot_forecast <- function(object,
                          include = NULL,
                          add_test = NULL,
                          add_fitted = FALSE,
                          add_paths = FALSE,
                          add_legend = TRUE,
                          legend_position = "right",
                          title = NULL,
                          subtitle = NULL,
                          caption = NULL,
                          xlab = "Time",
                          ylab = NULL,
                          color_fcst = "#0000AA",
                          color_fitted = "#0000AA",
                          color_actual = "black",
                          color_test = "#D55E00",
                          color_paths = "grey35",
                          size_fcst = 1,
                          size_fitted = 0.5,
                          size_actual = 0.5,
                          size_test = 1,
                          size_paths = 0.1) {
  
  # (1) PREPROCESSING ==========================================================
  
  # Check class of object
  if (class(object) != "forecast") {
    stop("Object should be of class forecast")
  }
  
  # Extract forecasts, prediction intervals, actual and fitted values
  forecast <- object$forecast
  series <- rbind(object$actual, object$fitted)
  n_ahead <- object$n_ahead
  
  if (add_paths == TRUE) {
    simulation <- object$simulation
  }
  
  # Prepare actuals and fitted values
  # Check for fitted values
  if (add_fitted == FALSE) {
    series <- series %>% 
      filter(.type != "fitted")
  }
  
  # Cut actual and fitted values to correct length
  if (!is.null(include)) {
    series <- series %>%
      group_by(.variable, .type) %>%
      slice((n() - include + 1):n()) %>%
      ungroup()
  }
  
  # Add test data
  if (is.null(add_test)) {
    test <- NULL
    color_test <- NULL
    size_test <- NULL
  } else {
    test <- add_test %>%
      dplyr::mutate(.type = "test") %>%
      update_tsibble(
        key = c(".variable", ".type"))
  }
  
  # Concatenate actual and fitted values and point forecasts and test data
  series <- rbind(series, forecast, test)
  
  # if (include == 0) {
  #   color_actual <- color_fitted <- NULL
  #   size_actual <- size_fitted <- NULL
  # }
  # 
  # if (add_fitted == FALSE) {
  #   color_fitted <- NULL
  #   size_fitted <- NULL
  # }
  
  # (2) VISUALIZATION ==========================================================
  
  # Initialize ggplot
  p <- ggplot2::ggplot()
  p <- p + ggplot2::scale_y_continuous()
  
  # Create prediction intervals
  if (add_paths == TRUE) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(
        x = date_time,
        y = .value,
        group = .sim),
      data = simulation,
      alpha = 0.1,
      color = color_paths,
      size = size_paths)
  }
  
  # Create lines (actual, fitted, forecast)
  p <- p + ggplot2::geom_line(
    ggplot2::aes(
      x = date_time,
      y = .value,
      colour = .type,
      size = .type),
    data = series)
  
  # Adjust lines colors
  p <- p + ggplot2::scale_colour_manual(
    name = "Series",
    values = c(color_actual,
               color_fitted,
               color_fcst,
               color_test))
  
  # Adjust lines widths
  p <- p + ggplot2::scale_size_manual(
    name = "Series",
    values = c(size_actual,
               size_fitted,
               size_fcst,
               size_test))
  
  # Plot each variable in a seperate panel
  p <- p + ggplot2::facet_wrap(
    ~.variable,
    scales = "free")
  
  # Adjust theme
  p <- p + theme_echos()
  #p <- p + theme_minimal()
  
  # Adjust title, subtitle, axis labels etc.
  p <- p + ggplot2::labs(title = title)
  p <- p + ggplot2::labs(subtitle = subtitle)
  p <- p + ggplot2::labs(x = xlab)
  p <- p + ggplot2::labs(y = ylab)
  p <- p + ggplot2::labs(caption = caption)
  
  # Adjust the legends
  if (add_legend == FALSE) {
    p <- p + ggplot2::theme(legend.position = "none")
  } else {
    # Fix the order of the legends
    p <- p + ggplot2::guides(
      colour = ggplot2::guide_legend(
        title = "Series",
        order = 1))
    
    p <- p + ggplot2::guides(
      size = ggplot2::guide_legend(
        title = "Series",
        order = 1))
    
    # Adjust font size, font face and position
    p <- p + ggplot2::theme(
      legend.position = legend_position)
  }
  return(p)
}