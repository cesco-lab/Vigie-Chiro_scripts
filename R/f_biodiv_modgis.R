#' @title f_biodiv_modgis
#' @description Run the Random Forest model
#'
#' @param data [data frame] The data
#' @param metric [string] Column name of the variable to be modeled
#' @param x [string] Column name of the longitude
#' @param y [string] Column name of the latitude
#' @param num_coord [numeric] Number of coordinates to project
#' @param log_transform [boolean] If TRUE, log-transform response
#' @param strata [string] Vector of strata
#' @param plot_var_imp [boolean] If TRUE, variables importances are plotted
#'
#' @export
#'
#' @importFrom randomForest randomForest
#' @importFrom spdep Rotation
#' @importFrom data.table yday
#'
#' @return a Random Forest object
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @examples
#'
#' # See vignette()
#'



f_biodiv_modgis <- function(
  data, metric, x = "longitude", y = "latitude", date = NULL, num_coord = 40,
  log_transform = TRUE, strata = c("id_site", "localite"), plot_var_imp = TRUE
) {



  # Function checks     -------------

  if (missing(data)) { stop("No data provided.") }
  if (missing(metric)) { stop("No metric colname provided.") }

  if (length(metric) != 1) { stop("Argument metric must be length of 1.") }
  if (!is.character(metric)) { stop("Argument metric must be a character.") }
  if (sum(which(colnames(data) == metric)) == 0) { stop("Missing metric column.") }

  if (!is.character(strata)) { stop("Argument strata must be a character.") }
  if (sum(which(colnames(data) %in% strata)) < length(strata)) { stop("Missing strata column(s).") }

  if (!is.logical(log_transform)) { stop("log_transform must be boolean.") }
  if (!is.logical(plot_var_imp)) { stop("plot_var_imp must be boolean.") }

  if (length(x) != 1) { stop("x must be length of 1.") }
  if (!is.character(x)) { stop("Argument x must be a character.") }
  if (sum(which(colnames(data) == x)) == 0) { stop("Missing x column.") }

  if (length(y) != 1) { stop("y must be length of 1.") }
  if (!is.character(y)) { stop("Argument y must be a character.") }
  if (sum(which(colnames(data) == y)) == 0) { stop("Missing y column.") }

  if (length(num_coord) != 1) { stop("num_coord must be length of 1.") }
  if (!is.numeric(num_coord)) { stop("Argument num_coord must be numeric.") }
  if (num_coord <= 0) { stop("Argument num_coord must be > 0.") }

  if (var(data[ , metric]) == 0) { stop("No variation in metric.") }

  if (!is.null(date)) {

    if (length(date) != 1) { stop("date must be length of 1.") }
    if (!is.character(date)) { stop("Argument date must be a character.") }
    if (sum(which(colnames(data) == date)) == 0) { stop("Missing date column.") }
  }


  # Log-transform metric     -------------

  if (log_transform) {

    data[ , metric] <- log10(data[ , metric] + 1)
  }


  # Coordinates rotation     -------------

  coords <- as.matrix(cbind(data[ , x], data[ , y]))

  for (a in 0 : (num_coord - 1)) {

    coords_i <- spdep::Rotation(coords, angle = pi * a / num_coord)
    data     <- cbind(data, coords_i[ , 1])

    names(data)[ncol(data)] <- paste0("SpCoord", a)

  }


  # Create date predictors     -------------

  if (!is.null(date)) {

    julday <- data.table::yday(
      as.Date(
        substr(data[ , date], 1, 10)
      )
    )

    trigoday <- julday / 365 * pi * 2

    data[ , "SpFDateC"] <- cos(trigoday)
    data[ , "SpFDateS"] <- sin(trigoday)

  } else {

    data[ , "SpFDateC"] <- 0
    data[ , "SpFDateS"] <- 0
  }


  # Extract predictors     -------------

  test_pred   <- substr(names(data), 1, 2) == "Sp"
  pred_names  <- names(data)[test_pred]
  predictors  <- data[ , pred_names]


  # Run model     -------------

  if (is.null(strata)) {

    mod_rf <- randomForest::randomForest(
      x          = predictors,
      y          = data[ , metric],
      replace    = TRUE,
      importance = TRUE
    )

  } else {

    strata_char <- apply(
      data[ , strata],
      MARGIN = 1,
      FUN    = function(x) {
        paste(x[1], x[2], sep = "_")
      }
    )

    mod_rf <- randomForest::randomForest(
      x          = predictors,
      y          = data[ , metric],
      replace    = TRUE,
      importance = TRUE,
      strata     = strata_char
    )

  }


  # Plot variable importances (if required)     -------------

  if (plot_var_imp) {

    randomForest::varImpPlot(mod_rf, cex = 0.5)
  }


  # Print PseudoR2     -------------

  print(paste0("PseudoR2: ", round(mod_rf$rsq[mod_rf$ntree], 2))

  return(mod_rf)
}
