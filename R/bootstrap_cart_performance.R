#' bootstrap_cart_performance
#'
#' @param bootstrap_list a bootstrap list with tain/test data and model
#' @param class variable name to test a.k.a. column
#'
#' @returns
#' bootstrap performance table
#' @export
#'
#' @examples
#' no example
bootstrap_cart_performance <- function(bootstrap_list, class) {

  # ------------------------------------------------------------
  # 1. Wrapper pour prédictions
  # ------------------------------------------------------------
  pred_wrapper <- function(object, newdata) {
    predict(object, newdata, type = "class")
  }

  # ------------------------------------------------------------
  # 2. Initialisation du résultat final
  # ------------------------------------------------------------
  n_iter <- length(bootstrap_list)
  final_results <- vector("list", n_iter)

  # ------------------------------------------------------------
  # 3. Boucle sur les itérations de bootstrap
  # ------------------------------------------------------------
  for (i in seq_len(n_iter)) {

    mod  <- bootstrap_list[[i]][["mod"]]
    test <- bootstrap_list[[i]][["test"]]
    train <- bootstrap_list[[i]][["train"]]

    # ------------------------------------------------------------
    # 3.1 Prédictions CART
    # ------------------------------------------------------------
    pred <- predict(mod, test, type = "class")
    ref  <- test[[class]]

    # ------------------------------------------------------------
    # 3.2 Matrice de confusion
    # ------------------------------------------------------------
    scores <- confusionMatrix(data = pred, reference = ref)

    # Extraire byClass (acc, sens, spec)
    byclass <- scores$byClass[, c(1, 2, 11), drop = FALSE]

    # Convertir rownames en nom de classe (sans tidyverse)
    classes <- sub("Class: ", "", rownames(byclass))

    # Construire un tableau large (pivot_wider en base R)
    perf_mat <- t(byclass)
    colnames(perf_mat) <- classes

    # Ajouter kappa et accuracy
    perf <- as.data.frame(perf_mat)
    perf$kappa    <- scores$overall[["Kappa"]]
    perf$Accuracy <- scores$overall[["Accuracy"]]

    # ------------------------------------------------------------
    # 3.3 Importance des variables via {vip}
    # ------------------------------------------------------------
    vi_res <- vi(
      mod,
      method = "permute",
      train  = test,
      target = class,
      metric = "bal_accuracy",
      pred_wrapper = pred_wrapper,
      smaller_is_better = FALSE
    )

    # Transformation wide (pivot_wider equivalent)
    vars <- vi_res$Variable
    imps <- vi_res$Importance

    imp_df <- as.data.frame(t(imps))
    colnames(imp_df) <- paste0(vars, "_importance")

    # ------------------------------------------------------------
    # 3.4 Variables utilisées dans l'arbre
    # ------------------------------------------------------------
    first_var <- mod$frame[1, 1]
    var_used  <- unique(mod$frame[, 1])

    # ------------------------------------------------------------
    # 3.5 Fusion finale des résultats
    # ------------------------------------------------------------
    res <- cbind(perf, imp_df)

    res$first_var <- first_var
    res$var_used  <- list(var_used)
    res$iteration <- i
    res$n_train   <- nrow(train)
    res$n_test    <- nrow(test)

    final_results[[i]] <- res
  }

  # ------------------------------------------------------------
  # 4. Combiner toutes les lignes
  # ------------------------------------------------------------
  final_results <- do.call(rbind, final_results)

  return(final_results)
}
