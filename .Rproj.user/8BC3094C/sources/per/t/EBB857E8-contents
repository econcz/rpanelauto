#' Perform Automatic Estimation on Time Series in Multidimensional Panels
#'
#' Applies a user-supplied automatic estimation function independently to the
#' selected time-series variables in a data frame. Estimation is performed
#' separately for every combination of the preceding dimensions, while the
#' final dimension determines the ordering of observations within each time
#' series.
#'
#' When only one dimension is supplied, each selected variable is treated as a
#' single ungrouped time series. Optional pre-estimation and post-estimation
#' functions can be evaluated before and after each automatic estimation.
#'
#' @param data data frame  
#'   Input data containing the dimension variables and the time-series
#'   variables to be modelled.
#'
#' @param vars character vector  
#'   Names of the numeric variables for which models are to be estimated.
#'   A separate model is estimated for every selected variable and every
#'   combination of the grouping dimensions.
#'
#' @param dimensions character vector  
#'   Names of the variables that uniquely identify observations. The last
#'   variable specifies the dimension along which observations are ordered,
#'   while all preceding variables define independent groups. When only one
#'   dimension is supplied, the selected variables are treated as ungrouped
#'   time series.
#'
#' @param estimate function  
#'   Automatic estimation function applied independently to each time series.
#'   The function must accept a \code{\link[stats]{ts}} object as its first
#'   argument and return a fitted model object. Additional arguments required
#'   by the estimation function can be supplied through \code{...}. The
#'   function may be provided by another package or defined by the user.
#'
#' @param frequency numeric scalar, default = \code{1}  
#'   Frequency used when converting each ordered variable to a
#'   \code{\link[stats]{ts}} object. For example, use \code{1} for annual,
#'   \code{4} for quarterly, and \code{12} for monthly observations.
#'
#' @param preestimation function or \code{NULL}, default = \code{NULL}  
#'   Function evaluated after each panel group has been ordered and before
#'   estimation. The function must accept the ordered group data frame as its
#'   first argument and return a data frame with the same number of
#'   observations and unchanged dimension variables. The returned data frame
#'   is subsequently used to estimate all selected variables in that group.
#'
#' @param postestimation function or \code{NULL}, default = \code{NULL}  
#'   Function evaluated after each model has been estimated. The function must
#'   accept the ordered group data frame as its first argument and the fitted
#'   model as its second argument. It may additionally accept a named argument
#'   \code{variable}, identifying the variable associated with the current
#'   model. It must return the final result as a named list containing
#'   components \code{data} and \code{model}. The function may add fitted
#'   values, residuals, forecasts, standard errors, or other post-estimation
#'   quantities to the group data frame.
#'
#' @param ... Optional.  
#'   Additional arguments passed unchanged to \code{estimate}.
#'
#' @return
#' A named list containing at least the following components:  
#' \describe{
#'   \item{\code{data}}{
#'     The complete data frame after application of the optional
#'     \code{preestimation} and \code{postestimation} functions. The original
#'     row order is preserved.
#'   }
#'   \item{\code{models}}{
#'     A named list containing one model record for every selected variable and
#'     every combination of the grouping dimensions. Each model record contains
#'     the following components:
#'     \describe{
#'       \item{\code{variable}}{
#'         Name of the modelled variable.
#'       }
#'       \item{\code{group}}{
#'         Named list identifying the values of the grouping dimensions. This
#'         component is an empty list when no grouping dimensions are supplied.
#'       }
#'       \item{\code{model}}{
#'         The fitted model returned by \code{estimate}, or the model component
#'         returned by \code{postestimation}.
#'       }
#'     }
#'   }
#' }
#'
#' @details
#' The final variable in \code{dimensions} determines the ordering of
#' observations within each time series. All preceding dimension variables
#' define independent groups.
#'
#' For example, with
#'
#' \preformatted{
#' dimensions = c("country", "sector", "year")
#' }
#'
#' observations are ordered by \code{year}, and separate models are estimated
#' for every variable-country-sector combination.
#'
#' With
#'
#' \preformatted{
#' dimensions = "year"
#' }
#'
#' each variable listed in \code{vars} is treated as one ungrouped time series.
#'
#' The dimension variables themselves are not modified, and the original row
#' order of \code{data} is preserved.
#'
#' Missing values in variables listed in \code{vars} are passed unchanged to
#' the estimation function. Their admissibility and treatment therefore depend
#' on the selected estimator.
#'
#' Missing values in dimension variables are not permitted. Each combination
#' of the dimension variables must uniquely identify one observation.
#'
#' The order of operations within each panel group is:
#'
#' \enumerate{
#'   \item Order observations by the final dimension.
#'   \item Apply \code{preestimation} to the ordered group data.
#'   \item Convert each selected variable to a \code{ts} object.
#'   \item Apply \code{estimate} independently to each time series.
#'   \item Apply \code{postestimation} to the group data and fitted model.
#'   \item Store the fitted model and copy the transformed data back to the
#'         original row positions.
#' }
#'
#' The \code{preestimation} and \code{postestimation} functions must preserve
#' the number of observations and the values of all dimension variables within
#' each group.
#'
#' When several variables are modelled, \code{postestimation} should generally
#' use variable-specific output names to avoid overwriting quantities created
#' for another model. When the function declares a \code{variable} argument,
#' \code{panelauto} supplies the name of the current variable automatically.
#'
#' Any estimation function can be used, provided that it accepts the time
#' series as its first argument. Functions with different interfaces can be
#' supplied through a user-defined wrapper.
#'
#' @seealso
#' \code{\link[stats]{arima}},
#' \code{\link[stats]{ts}},
#' \code{\link[forecast]{auto.arima}},
#' \code{\link[forecast]{ets}}
#'
#' @examples
#'   ## Example: user-defined automatic autoregressive estimation
#'
#'   set.seed(123456789)
#'
#'   data <- data.frame(
#'       country = rep(c("A", "B"), each = 20L),
#'       year    = rep(2001:2020, 2L),
#'       y       = c(
#'           cumsum(stats::rnorm(20L)),
#'           cumsum(stats::rnorm(20L))
#'       )
#'   )
#'
#'   automatic_ar <- function(series, orders = 1:3) {
#'       models <- lapply(
#'           orders,
#'           function(p) {
#'               stats::arima(
#'                   series,
#'                   order = c(p, 0L, 0L)
#'               )
#'           }
#'       )
#'
#'       aic <- vapply(
#'           models,
#'           stats::AIC,
#'           numeric(1L)
#'       )
#'
#'       models[[which.min(aic)]]
#'   }
#'
#'   result <- panelauto(
#'       data       = data,
#'       vars       = "y",
#'       dimensions = c("country", "year"),
#'       estimate   = automatic_ar,
#'       orders     = 1:2
#'   )
#'
#'   print(result$data)
#'   print(result$models[[1L]]$model)
#'
#'   ## Example: automatic ARIMA estimation
#'
#'   if (requireNamespace("forecast", quietly = TRUE)) {
#'       result <- panelauto(
#'           data       = data,
#'           vars       = "y",
#'           dimensions = c("country", "year"),
#'           estimate   = forecast::auto.arima,
#'           seasonal   = FALSE
#'       )
#'
#'       print(result$models[[1L]]$model)
#'   }
#'
#'   ## Pre-estimation, estimation, and fitted values
#'
#'   result <- panelauto(
#'       data       = data,
#'       vars       = "y",
#'       dimensions = c("country", "year"),
#'       preestimation = function(data) {
#'           data$y <- data$y - mean(data$y)
#'           data
#'       },
#'       estimate = automatic_ar,
#'       orders   = 1:2,
#'       postestimation = function(data, model, variable) {
#'           fitted_name   <- paste0(variable, "_fitted")
#'           residual_name <- paste0(variable, "_residual")
#'
#'           data[[residual_name]] <- as.numeric(stats::residuals(model))
#'           data[[fitted_name]]   <- data[[variable]] - 
#'                                    as.numeric(stats::residuals(model))
#'
#'           list(
#'               data  = data,
#'               model = model
#'           )
#'       }
#'   )
#'
#'   print(result$data)
#'
#' @export
panelauto <- function(data, vars, dimensions, estimate, frequency=1,
                      preestimation=NULL, postestimation=NULL, ...) {
  # 'data' checks
  if (!is.data.frame(data))
    stop("'data' must be a data frame.",                        call. = FALSE)
  if (nrow(data)                                                      ==   0L)
    stop("'data' must contain at least one observation.",       call. = FALSE)
  # 'vars' checks
  if (missing(vars)            || !is.character(vars)                         ||
      length(vars)       == 0L || anyNA(vars)                                 ||
      any(vars           == ""))
    stop("'vars' must be a non-empty character vector of ",
         "variable names.",                                     call. = FALSE)
  if (anyDuplicated(vars))
    stop("'vars' must not contain duplicated variable names.",  call. = FALSE)
  if (length(missing_vars         <- setdiff(vars, names(data)))        >  0L)
    stop("Variables not found in 'data': ",
         paste(missing_vars,       collapse=", "),        ".",  call. = FALSE)
  numeric_vars                    <- vapply(data[vars], is.numeric,
                                            logical(1L))
  if (any(!numeric_vars))
    stop("Variables in 'vars' must be numeric: ",
         paste(vars[!numeric_vars], collapse=", "),        ".", call. = FALSE)
  # 'dimensions' checks
  if (missing(dimensions)      || !is.character(dimensions)                   ||
      length(dimensions) == 0L || anyNA(dimensions)                           ||
      any(dimensions     == ""))
    stop("'dimensions' must be a non-empty character vector ",
         "of variable names.",                                  call. = FALSE)
  if (anyDuplicated(dimensions))
    stop("'dimensions' must not contain duplicated variable ",
         "names.",                                              call. = FALSE)
  if (length(missing_dimensions   <- setdiff(dimensions, names(data)))  >  0L)
    stop("Dimension variables not found in 'data': ",
         paste(missing_dimensions, collapse=", "),        ".",  call. = FALSE)
  if (length(overlapping_vars     <- intersect(vars, dimensions))       >  0L)
    stop("Variables in 'vars' cannot also be dimension ",
         "variables: ",
         paste(overlapping_vars,   collapse=", "),        ".",  call. = FALSE)
  # observations checks
  if (anyNA(data[dimensions]))
    stop("Missing values are not permitted in dimension ",
         "variables.",                                          call. = FALSE)
  if (anyDuplicated(data[dimensions]))
    stop("Each combination of dimension variables must ",
         "identify a unique observation.",                      call. = FALSE)
  # arguments checks
  if (missing(estimate)        || !is.function(estimate))
    stop("'estimate' must be an estimation function.",          call. = FALSE)
  if (!is.numeric(frequency)   || length(frequency)                   !=   1L ||
      is.na(frequency)         || !is.finite(frequency)                       ||
      frequency <= 0)
    stop("'frequency' must be a positive finite numeric ",
         "scalar.",                                             call. = FALSE)
  if (!is.null(preestimation)  && !is.function(preestimation))
    stop("'preestimation' must be NULL or a function.",         call. = FALSE)
  if (!is.null(postestimation) && !is.function(postestimation))
    stop("'postestimation' must be NULL or a function.",        call. = FALSE)
  # parse dimensions
  time_dimension                  <- utils::tail(dimensions,  1L)
  group_dimensions                <- utils::head(dimensions, -1L)
  groups                          <- if (length(group_dimensions) == 0L)
    list(seq_len(nrow(data)))          else
      split(seq_len(nrow(data)),
            do.call(interaction,
                    c(unname(data[group_dimensions]),
                      list(drop=TRUE, lex.order=TRUE))),
            drop=TRUE)
  
  # parse estimation arguments
  estimate_args                   <- list(...)
  # initialize results
  result_data                     <- data
  models                          <- list()
  model_names                     <- character()
  model_index                     <- 0L
  # loop through panel groups
  for (group_rows in groups)      {
    rows                          <- group_rows[order(
      data[[time_dimension]][group_rows],
      method="radix")]
    group_data                    <- result_data[rows, ,    drop=FALSE]
    original_dimensions           <- data[rows, dimensions, drop=FALSE]
    # identify the group
    group_values                  <- if (length(group_dimensions) == 0L)
      list()                              else
        as.list(data[rows[1L], group_dimensions,
                     drop=FALSE])
    # apply the preestimation function (if provided)
    if (!is.null(preestimation))  {
      group_data                  <- preestimation(group_data)
      if (!is.data.frame(group_data))
        stop("'preestimation' must return a data frame.",       call. = FALSE)
      if (nrow(group_data) != length(rows))
        stop("'preestimation' must preserve the number of ",
             "observations within each time series.",           call. = FALSE)
      if (!all(dimensions %in% names(group_data)))
        stop("'preestimation' must preserve all dimension ",
             "variables.",                                      call. = FALSE)
      if (!identical(group_data[dimensions], original_dimensions))
        stop("'preestimation' must not modify the dimension ",
             "variables.",                                      call. = FALSE)
    }
    # estimate the models
    for (variable in vars)        {
      if (!variable %in% names(group_data))
        stop("'preestimation' removed the modelled variable '",
             variable, "'.",                                    call. = FALSE)
      if (!is.numeric(group_data[[variable]]))
        stop("The modelled variable '", variable,
             "' must remain numeric after 'preestimation'.",    call. = FALSE)
      series                          <- stats::ts(group_data[[variable]],
                                                   frequency=frequency)
      result_model                    <- do.call(estimate,
                                                 c(list(series), estimate_args))
      model_data                      <- group_data
      # apply the postestimation function (if provided)
      if (!is.null(postestimation)) {
        post_formals                <- names(formals(postestimation))
        accepts_variable            <- !is.null(post_formals)                 &&
          ("variable" %in% post_formals          ||
             "..."      %in% post_formals)
        result                      <- if (accepts_variable)
          postestimation(model_data, result_model,
                         variable=variable) else
                           postestimation(model_data, result_model)
        if (!is.list(result)       || is.data.frame(result))
          stop("'postestimation' must return a named list ",
               "containing 'data' and 'model'.",                call. = FALSE)
        if (is.null(names(result)) || anyDuplicated(names(result)))
          stop("'postestimation' must return a list with unique ",
               "component names.",                              call. = FALSE)
        if (!all(c("data", "model") %in% names(result)))
          stop("'postestimation' must return a named list ",
               "containing 'data' and 'model'.",                call. = FALSE)
        if (!is.data.frame(result$data))
          stop("The 'data' component returned by ",
               "'postestimation' must be a data frame.",        call. = FALSE)
        if (nrow(result$data)      != length(rows))
          stop("The 'data' component returned by ",
               "'postestimation' must preserve the number of ",
               "observations within each time series.",         call. = FALSE)
        if (!all(dimensions %in% names(result$data)))
          stop("'postestimation' must preserve all dimension ",
               "variables.",                                    call. = FALSE)
        if (!identical(result$data[dimensions], original_dimensions))
          stop("'postestimation' must not modify the dimension ",
               "variables.",                                    call. = FALSE)
        model_data                  <- result$data
        result_model                <- result$model
      }
      # initialize new variables
      new_variables                 <- setdiff(names(model_data ),
                                               names(result_data))
      for (new_variable in new_variables) {
        result_data[[new_variable]] <- model_data[[new_variable]][
          rep(NA_integer_, nrow(result_data))]
      }
      # copy transformed data to the original row positions
      result_data[rows, names(model_data)] <- model_data
      # store the model
      model_index                          <- model_index           +  1L
      models[[model_index]]                <- list(variable=variable,
                                                   group=group_values,
                                                   model=result_model)
      group_label                   <- if (length(group_dimensions) == 0L)
        character()                       else
          paste0(group_dimensions, "=",
                 vapply(group_values, as.character,
                        character(1L)))
      model_names[[model_index]]    <- paste(c(paste0("variable=", variable),
                                               group_label),
                                             collapse=";")
      # preserve additions for the next model in the same group
      group_data                    <- model_data
    }
  }
  # name models
  names(models)                     <- make.unique(model_names)
  
  # return the result
  list(data=result_data, models=models)
}
