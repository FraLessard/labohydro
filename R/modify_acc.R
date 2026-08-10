#' modify_acc
#'
#' @param facc facc
#' @param m_slope m_slope
#'
#' @returns
#' modified flow accumulation
#' @export
#'
#' @examples
#' # no example
modify_acc <- function(facc,
                       m_slope){

  n_changes <- 1
  iteration <- 0

  # Tant que la superficie a modifier n'est pas nulle, les iterations continuent
  while(n_changes > 0){

    iteration <- iteration + 1

    # terra::plot(facc) # Juste necessaire pour la validation que tout es ok

    # Permet d'aller chercher pour chaque cellule l'accumulation de flux max voisine
    facc_max <- terra::focal(facc, w = 3, fun = "max", na.rm = TRUE, na.policy = "omit", expand = TRUE)

    # Multiplie l'accumulation de flux max voisine par la pente modifiee
    facc_max_mod <- facc_max*m_slope

    # Selectionne l'accumulation de flux max entre celle originale et celle multipliee par la pente modifiee
    facc_mod <- c(facc, facc_max_mod)
    facc_mod <- terra::app(facc_mod, "max")

    # Permet d'identifier les cellules ayant ete modifiee par la pente modifiee
    facc_diff <- !sum(facc == facc_mod)

    n_changes <- terra::global(facc_diff, fun = "sum", na.rm = TRUE)$sum

    # Pour remplacer l'accumulation de flux modifiée initiale par celle ayant l'accumulation de flux maximale
    facc <- facc_mod

    cat("\npass ", iteration, " (", n_changes, " > 0)", sep = "")

  }

  return(facc)

}
