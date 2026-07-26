make_panel_data <- function() {
  data.frame(
    country = rep(c("A", "B"), each=4L),
    year    = rep(2020:2023, 2L),
    y       = c(10, 20, 30, 40, 50, 60, 70, 80),
    x       = c(1, 2, 3, 4, 5, 6, 7, 8)
  )
}


automatic_mean <- function(series, multiplier=1) {
  structure(
    list(
      series    = as.numeric(series),
      frequency = stats::frequency(series),
      mean      = mean(series) * multiplier
    ),
    class="automatic_mean"
  )
}

test_that("panelauto estimates one model for each panel series", {
  data   <- make_panel_data()
  
  result <- panelauto(
    data       = data,
    vars       = "y",
    dimensions = c("country", "year"),
    estimate   = automatic_mean
  )
  
  expect_type(result, "list")
  expect_named(result, c("data", "models"))
  expect_s3_class(result$data, "data.frame")
  expect_length(result$models, 2L)
  
  expect_named(
    result$models,
    c(
      "variable=y;country=A",
      "variable=y;country=B"
    )
  )
  
  expect_identical(result$data, data)
  
  expect_equal(
    result$models[[1L]]$model$series,
    c(10, 20, 30, 40)
  )
  
  expect_equal(
    result$models[[2L]]$model$series,
    c(50, 60, 70, 80)
  )
  
  expect_equal(result$models[[1L]]$model$mean, 25)
  expect_equal(result$models[[2L]]$model$mean, 65)
  
  expect_identical(
    result$models[[1L]]$group,
    list(country="A")
  )
})

test_that("panelauto orders each series by the final dimension", {
  data   <- data.frame(
    country = c("A", "A", "A", "B", "B", "B"),
    year    = c(2022, 2020, 2021, 2021, 2022, 2020),
    y       = c(30, 10, 20, 60, 70, 50)
  )
  
  result <- panelauto(
    data       = data,
    vars       = "y",
    dimensions = c("country", "year"),
    estimate   = automatic_mean
  )
  
  expect_equal(
    result$models[[1L]]$model$series,
    c(10, 20, 30)
  )
  
  expect_equal(
    result$models[[2L]]$model$series,
    c(50, 60, 70)
  )
  
  # The returned data retain their original order.
  expect_identical(result$data, data)
})

test_that("panelauto estimates multiple variables", {
  data   <- make_panel_data()
  
  result <- panelauto(
    data       = data,
    vars       = c("y", "x"),
    dimensions = c("country", "year"),
    estimate   = automatic_mean
  )
  
  expect_length(result$models, 4L)
  
  expect_named(
    result$models,
    c(
      "variable=y;country=A",
      "variable=x;country=A",
      "variable=y;country=B",
      "variable=x;country=B"
    )
  )
  
  expect_identical(
    unname(
      vapply(
        result$models,
        function(x) x$variable,
        character(1L)
      )
    ),
    c("y", "x", "y", "x")
  )
  
  expect_equal(result$models[[1L]]$model$mean, 25)
  expect_equal(result$models[[2L]]$model$mean, 2.5)
  expect_equal(result$models[[3L]]$model$mean, 65)
  expect_equal(result$models[[4L]]$model$mean, 6.5)
})

test_that("panelauto handles an ungrouped time series", {
  data   <- data.frame(
    year = 2020:2023,
    y    = c(10, 20, 30, 40)
  )
  
  result <- panelauto(
    data       = data,
    vars       = "y",
    dimensions = "year",
    estimate   = automatic_mean,
    frequency  = 4
  )
  
  expect_length(result$models, 1L)
  expect_named(result$models, "variable=y")
  expect_identical(result$models[[1L]]$group, list())
  
  expect_equal(
    result$models[[1L]]$model$series,
    c(10, 20, 30, 40)
  )
  
  expect_equal(
    result$models[[1L]]$model$frequency,
    4
  )
})

test_that("panelauto passes additional arguments to estimate", {
  data   <- make_panel_data()
  
  result <- panelauto(
    data       = data,
    vars       = "y",
    dimensions = c("country", "year"),
    estimate   = automatic_mean,
    multiplier = 2
  )
  
  expect_equal(result$models[[1L]]$model$mean, 50)
  expect_equal(result$models[[2L]]$model$mean, 130)
})

test_that("panelauto applies pre-estimation before estimation", {
  data   <- make_panel_data()
  
  result <- panelauto(
    data       = data,
    vars       = "y",
    dimensions = c("country", "year"),
    estimate   = automatic_mean,
    preestimation = function(data) {
      data$y_centered <- data$y - mean(data$y)
      data$y          <- data$y_centered
      data
    }
  )
  
  expect_true("y_centered" %in% names(result$data))
  
  expect_equal(
    result$data$y_centered,
    c(-15, -5, 5, 15, -15, -5, 5, 15)
  )
  
  expect_equal(result$models[[1L]]$model$mean, 0)
  expect_equal(result$models[[2L]]$model$mean, 0)
})

test_that("panelauto applies post-estimation and adds results to data", {
  data   <- make_panel_data()
  
  result <- panelauto(
    data       = data,
    vars       = c("y", "x"),
    dimensions = c("country", "year"),
    estimate   = automatic_mean,
    postestimation = function(data, model, variable) {
      fitted_name   <- paste0(variable, "_fitted")
      residual_name <- paste0(variable, "_residual")
      
      data[[fitted_name]]   <- rep(model$mean, nrow(data))
      data[[residual_name]] <- data[[variable]] - model$mean
      
      list(
        data  = data,
        model = model
      )
    }
  )
  
  expect_true(
    all(
      c(
        "y_fitted",
        "y_residual",
        "x_fitted",
        "x_residual"
      ) %in% names(result$data)
    )
  )
  
  expect_equal(
    result$data$y_fitted,
    c(rep(25, 4L), rep(65, 4L))
  )
  
  expect_equal(
    result$data$x_fitted,
    c(rep(2.5, 4L), rep(6.5, 4L))
  )
  
  expect_equal(
    result$data$y_residual,
    c(-15, -5, 5, 15, -15, -5, 5, 15)
  )
  
  expect_equal(
    result$data$x_residual,
    c(-1.5, -0.5, 0.5, 1.5, -1.5, -0.5, 0.5, 1.5)
  )
})

test_that("panelauto rejects invalid data and specifications", {
  data <- make_panel_data()
  
  expect_error(
    panelauto(
      data       = matrix(1:4, nrow=2L),
      vars       = "y",
      dimensions = "year",
      estimate   = automatic_mean
    ),
    "data.*data frame"
  )
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "unknown",
      dimensions = c("country", "year"),
      estimate   = automatic_mean
    ),
    "Variables not found"
  )
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "year",
      dimensions = c("country", "year"),
      estimate   = automatic_mean
    ),
    "cannot also be dimension"
  )
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "y",
      dimensions = c("country", "unknown"),
      estimate   = automatic_mean
    ),
    "Dimension variables not found"
  )
  
  character_data   <- data
  character_data$y <- as.character(character_data$y)
  
  expect_error(
    panelauto(
      data       = character_data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = automatic_mean
    ),
    "must be numeric"
  )
  
  duplicated_data <- rbind(data, data[1L, ])
  
  expect_error(
    panelauto(
      data       = duplicated_data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = automatic_mean
    ),
    "unique observation"
  )
})

test_that("panelauto validates estimation arguments", {
  data <- make_panel_data()
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year")
    ),
    "estimate.*estimation function"
  )
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = "not a function"
    ),
    "estimate.*estimation function"
  )
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = automatic_mean,
      frequency  = 0
    ),
    "frequency.*positive"
  )
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = automatic_mean,
      preestimation = "not a function"
    ),
    "preestimation.*NULL or a function"
  )
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = automatic_mean,
      postestimation = "not a function"
    ),
    "postestimation.*NULL or a function"
  )
})

test_that("panelauto validates pre-estimation output", {
  data <- make_panel_data()
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = automatic_mean,
      preestimation = function(data) {
        data$y
      }
    ),
    "preestimation.*data frame"
  )
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = automatic_mean,
      preestimation = function(data) {
        data[-1L, ]
      }
    ),
    "preserve the number"
  )
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = automatic_mean,
      preestimation = function(data) {
        data$year <- rev(data$year)
        data
      }
    ),
    "must not modify.*dimension"
  )
})

test_that("panelauto validates post-estimation output", {
  data <- make_panel_data()
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = automatic_mean,
      postestimation = function(data, model) {
        data
      }
    ),
    "postestimation.*named list.*data.*model"
  )
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = automatic_mean,
      postestimation = function(data, model) {
        list(data=data)
      }
    ),
    "postestimation.*named list.*data.*model"
  )
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = automatic_mean,
      postestimation = function(data, model) {
        list(
          data  = data$y,
          model = model
        )
      }
    ),
    "data.*postestimation.*data frame"
  )
  
  expect_error(
    panelauto(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = automatic_mean,
      postestimation = function(data, model) {
        data$year <- rev(data$year)
        list(
          data  = data,
          model = model
        )
      }
    ),
    "must not modify.*dimension"
  )
})
