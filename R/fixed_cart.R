#' fixed_cart
#'
#' @param formula formula
#' @param data data frame to train with
#' @param MD max depth of tree
#' @param NB max number of branch of tree
#' @param MS minimum data per split
#'
#' @returns
#' a cart model with fixed parameters
#' @export
#'
#' @examples
#' # no example
fixed_cart <- function(formula,
                       data,
                       MD,  # Maximum tree depth
                       NB,  # Maximum and fixed number of branches (nsplit)
                       MS)  # Minimum split
{
  # ------------------------------------------------------------
  # 1. Fit an initial tree to obtain the CP table
  # ------------------------------------------------------------
  # We set cp = 0 to fully grow the tree (up to maxdepth)
  initial_model <- rpart::rpart(
    formula = formula,
    data    = data,
    control = rpart::rpart.control(
      maxdepth  = MD,
      xval      = 10,
      minbucket = 5,
      minsplit  = MS,
      cp        = 0
    ),
    model = TRUE
  )

  # Extract the complexity parameter table (cp table)
  cp_table <- as.data.frame(initial_model$cptable)

  # ------------------------------------------------------------
  # 2. Determine the appropriate number of branches (nsplit)
  # ------------------------------------------------------------
  # Identify all unique splits that do not exceed NB
  valid_splits <- unique(cp_table$nsplit)
  valid_splits <- valid_splits[valid_splits <= NB]

  # Use the maximum valid number of splits
  NBM <- max(valid_splits)

  # Extract the corresponding CP value
  cp <- cp_table$CP[cp_table$nsplit == NBM]

  # ------------------------------------------------------------
  # 3. Refit the classification tree using the selected CP
  # ------------------------------------------------------------
  # Multiply CP by a small factor (1.05) to ensure stable pruning
  final_model <- rpart::rpart(
    formula = formula,
    data    = data,
    control = rpart::rpart.control(
      maxdepth  = MD,
      xval      = 10,
      minbucket = 5,
      minsplit  = MS,
      cp        = cp * 1.05
    ),
    model = TRUE
  )

  # ------------------------------------------------------------
  # 4. Return the final pruned tree
  # ------------------------------------------------------------
  return(final_model)
}
