#' bootstrap_cart
#'
#' @param formula formula of cart model
#' @param data data frame to train with
#' @param nbagg number of iterations
#' @param MD max depth of tree
#' @param NB max number of branch of tree
#' @param MS minimum data per split
#'
#' @returns
#' a bootstrap list with tain/test data and model
#' @export
#'
#' @examples
#' no example
bootstrap_cart <- function(formula,
                           data,
                           nbagg,
                           MD,
                           NB,
                           MS) {

  # ------------------------------------------------------------
  # 1. Add a bootstrap ID column
  # ------------------------------------------------------------
  data$bag_id <- seq_len(nrow(data))

  # ------------------------------------------------------------
  # 2. Loop over nbagg bootstrap iterations
  # ------------------------------------------------------------
  final_results <- vector("list", nbagg)

  for (i in seq_len(nbagg)) {

    set.seed(i)

    # ------------------------------------------------------------
    # 2.1 Bootstrap sample (train)
    # ------------------------------------------------------------
    id_train <- sample(seq_len(nrow(data)),
                        size = nrow(data),
                        replace = TRUE)
    train <- data[id_train, , drop = FALSE]

    # ------------------------------------------------------------
    # 2.2 Out-of-bag rows (test)
    # ------------------------------------------------------------
    # Keep all rows whose bag_id does NOT appear in the bootstrap sample
    oob_id <- data$bag_id[!data$bag_id %in% train$bag_id]
    test <- data[data$bag_id %in% oob_id, , drop = FALSE]

    # Remove the bag_id column
    train$bag_id <- NULL
    if (nrow(test) > 0) test$bag_id <- NULL

    # ------------------------------------------------------------
    # 2.3 Fit the fixed CART model
    # ------------------------------------------------------------
    mod <- fixed_cart(
      formula = formula,
      data    = train,
      MD      = MD,
      NB      = NB,
      MS      = MS
    )

    # ------------------------------------------------------------
    # 2.4 Store results
    # ------------------------------------------------------------
    final_results[[i]] <- list(
      mod   = mod,
      train = train,
      test  = test
    )
  }

  return(final_results)
}
