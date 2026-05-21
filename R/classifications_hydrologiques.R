#' classification_hydrologique_labo_hydro_3classes_ecofor
#'
#' @param x DEP_SUR column of ecoforestry
#' @param y CO_TER column of ecoforestry
#'
#' @returns
#' hydrological classification
#' @export
#'
#' @examples
#' no example
classification_hydrologique_labo_hydro_3classes_ecofor <- function(x, # Depots de surface de la cartographie écoforestière
                                                                   y){ # Code de terrain de la cartographie écoforestière

  ch <- ifelse (x %in% c("1A","1AA","1AAM","1AAR","1AAY","1AB","1AD","1ADY","1AM","1AR","1ASY",
                         "1AY","1AYR","1M","1Y","M1","M1A","M1AA","M6S","M7T","M8A","M8AP","M8C","M8PY","R",
                         "R1","R1A","R1AA","R1BD","R2A","R2AK","R2BE","R3AN","R4","R4GA","R4GS","R5A","R5S",
                         "R6","R6S","R7","R7T","R8A","R8AP","R8C","R8E","R8P","R9S","RS"), "Dépôts glaciaires","")

  ch <- ifelse (y %in% c("EAU","DH","INO","AL")
                | x %in% c("4A","4AR","4AY","4GA","4GAM","4GAR","4GAY","5AM","5AR","5AY",
                           "7","7AN","7E","7L","7R","7T","7TM","7TY"), "Dépôts peu infiltrants", ch)

  ch <- ifelse (y %in% c("A","AER","ANT","CAR","GR","HAB","MI","PAI","US","VIL","RO")
                | x %in% c("1B","1BC","1BD","1BDY","1BF","1BG","1BI","1BIM","1BIY","1BN","1BP",
                           "1BPY","1BR","1BT","1P","2","2A","2AE","2AK","2AM","2AR","2AT","2AY","2B","2BD",
                           "2BDY","2BE","2BEM","2BER","2BEY","2BP","2BR","3","3A","3AC","3AE","3AN","3ANY",
                           "3D","3DD","3DE","4","4GD","4GS","4GSM","4GSY","4P","5G","5GR","5GSR","5L","5R",
                           "5S","5SM","5SR","5SY","5Y","6","6A","6AM","6AY","6R","6S","6SM","6SR","6SY","8",
                           "8A","8AC","8AL","8ALM","8ALY","8AM","8AP","8APM","8APY","8AR","8AS","8ASY","8AY",
                           "8AYP","8C","8CM","8CY","8E","8F","8G","8M","8P","8PM","8PY","8Y","9","9A","9R",
                           "9S","9SM","9SY","5A"), "Dépôts infiltrants", ch)

  ch <- ifelse(ch == "", "Dépôts glaciaires", ch)

  return(ch)
}

#' classification_hydrologique_labo_hydro_3classes
#'
#' @param x a column of surface deposits from MELCCFP raster
#'
#' @returns
#' hydrological classification
#' @export
#'
#' @examples
#' no example
classification_hydrologique_labo_hydro_3classes <- function(x){ # Depots de surface du MELCCFP

  classification_hydrologique <- c("0" = "Dépôts glaciaires",
                                   "0A" = "Dépôts glaciaires",
                                   "0B" = "Dépôts glaciaires",
                                   "0R" = "Dépôts glaciaires",
                                   "0S" = "Dépôts glaciaires",
                                   "0T" = "Dépôts glaciaires",
                                   "1AE" = "Dépôts glaciaires",
                                   "1AEV" = "Dépôts glaciaires",
                                   "1AEVB" = "Dépôts glaciaires",
                                   "1AM" = "Dépôts glaciaires",
                                   "1AMR" = "Dépôts glaciaires",
                                   "1B" = "Dépôts infiltrants",
                                   "1C" = "Dépôts infiltrants",
                                   "1D" = "Dépôts infiltrants",
                                   "1G" = "Dépôts infiltrants",
                                   "1H" = "Dépôts infiltrants",
                                   "1M" = "Dépôts infiltrants",
                                   "1R" = "Dépôts infiltrants",
                                   "2" = "Dépôts infiltrants",
                                   "2A" = "Dépôts infiltrants",
                                   "2AK" = "Dépôts infiltrants",
                                   "2AT" = "Dépôts infiltrants",
                                   "2B" = "Dépôts infiltrants",
                                   "2BD" = "Dépôts infiltrants",
                                   "2BE" = "Dépôts infiltrants",
                                   "3D" = "Dépôts infiltrants",
                                   "3DB" = "Dépôts infiltrants",
                                   "3F" = "Dépôts infiltrants",
                                   "3FA" = "Dépôts infiltrants",
                                   "3FB" = "Dépôts infiltrants",
                                   "3M" = "Dépôts infiltrants",
                                   "4A" = "Dépôts peu infiltrants",
                                   "4S" = "Dépôts infiltrants",
                                   "5A" = "Dépôts peu infiltrants",
                                   "5S" = "Dépôts infiltrants",
                                   "6A" = "Dépôts infiltrants",
                                   "6BH" = "Dépôts infiltrants",
                                   "6C" = "Dépôts infiltrants",
                                   "6CB" = "Dépôts infiltrants",
                                   "6CH" = "Dépôts infiltrants",
                                   "6D" = "Dépôts infiltrants",
                                   "6DB" = "Dépôts infiltrants",
                                   "6DH" = "Dépôts infiltrants",
                                   "7" = "Dépôts peu infiltrants",
                                   "7F" = "Dépôts peu infiltrants",
                                   "8A" = "Dépôts infiltrants",
                                   "8AAE" = "Dépôts infiltrants",
                                   "8AAM" = "Dépôts infiltrants",
                                   "8B" = "Dépôts infiltrants",
                                   "8C" = "Dépôts infiltrants",
                                   "8CAE" = "Dépôts infiltrants",
                                   "8CAM" = "Dépôts infiltrants",
                                   "8D" = "Dépôts infiltrants",
                                   "9D" = "Dépôts infiltrants",
                                   "0R_5A" = "Dépôts glaciaires",
                                   "1AE_7" = "Dépôts glaciaires",
                                   "1AM_0R" = "Dépôts glaciaires",
                                   "1AM_7" = "Dépôts glaciaires",
                                   "1I" = "Dépôts infiltrants",
                                   "1I_2" = "Dépôts infiltrants",
                                   "1M_1H" = "Dépôts infiltrants",
                                   "1M_2" = "Dépôts infiltrants",
                                   "1M_2BD" = "Dépôts infiltrants",
                                   "2_9" = "Dépôts infiltrants",
                                   "2BD_9" = "Dépôts infiltrants",
                                   "3C" = "Dépôts infiltrants",
                                   "3F_9" = "Dépôts infiltrants",
                                   "4D" = "Dépôts infiltrants",
                                   "5A_1G" = "Dépôts peu infiltrants",
                                   "5S_0R" = "Dépôts infiltrants",
                                   "5S_1G" = "Dépôts infiltrants",
                                   "5S_9" = "Dépôts infiltrants",
                                   "6_7" = "Dépôts infiltrants",
                                   "6_9" = "Dépôts infiltrants",
                                   "8DPA" = "Dépôts infiltrants",
                                   "7_0R" = "Dépôts peu infiltrants",
                                   "7_6" = "Dépôts peu infiltrants",
                                   "7_9" = "Dépôts infiltrants",
                                   "9_7" = "Dépôts infiltrants",
                                   "MS" = "ND",
                                   "1AT" = "Dépôts glaciaires",
                                   "1ATR" = "Dépôts glaciaires",
                                   "7T" = "Dépôts peu infiltrants",
                                   "9T" = "Dépôts infiltrants",
                                   "ANT" = "Dépôts infiltrants",
                                   "ND" = "ND")

  # depots <- terra::extract(y, x, fun=max, na.rm=TRUE)[,2]
  # x$CH_LH_3CL <- classification_hydrologique[match(depots, names(classification_hydrologique))]

  x <- classification_hydrologique[match(x, names(classification_hydrologique))]

  return(x)

}

#' classification_hydrologique_labo_hydro_4classes
#'
#' @param x a column of surface deposits from MELCCFP raster
#'
#' @returns
#' hydrological classification
#' @export
#'
#' @examples
#' no example
classification_hydrologique_labo_hydro_4classes <- function(x){ # Depots de surface du MELCCFP

  classification_hydrologique <- c("0" = "Tills et affleurements",
                                   "0A" = "Tills et affleurements",
                                   "0B" = "Tills et affleurements",
                                   "0R" = "Tills et affleurements",
                                   "0S" = "Tills et affleurements",
                                   "0T" = "Tills et affleurements",
                                   "1AE" = "Tills et affleurements",
                                   "1AEV" = "Dépôts triés",
                                   "1AEVB" = "Dépôts triés",
                                   "1AM" = "Tills et affleurements",
                                   "1AMR" = "Dépôts triés",
                                   "1B" = "Tills et affleurements",
                                   "1C" = "Dépôts triés",
                                   "1D" = "Dépôts triés",
                                   "1G" = "Dépôts triés",
                                   "1H" = "Dépôts triés",
                                   "1M" = "Dépôts triés",
                                   "1R" = "Dépôts triés",
                                   "2" = "Dépôts triés",
                                   "2A" = "Dépôts triés",
                                   "2AK" = "Dépôts triés",
                                   "2AT" = "Dépôts triés",
                                   "2B" = "Dépôts triés",
                                   "2BD" = "Dépôts triés",
                                   "2BE" = "Dépôts triés",
                                   "3D" = "Dépôts triés",
                                   "3DB" = "Dépôts triés",
                                   "3F" = "Dépôts triés",
                                   "3FA" = "Dépôts triés",
                                   "3FB" = "Dépôts triés",
                                   "3M" = "Dépôts triés",
                                   "4A" = "Dépôts triés",
                                   "4S" = "Dépôts triés",
                                   "5A" = "Dépôts triés",
                                   "5S" = "Dépôts triés",
                                   "6A" = "Dépôts triés",
                                   "6BH" = "Dépôts triés",
                                   "6C" = "Dépôts triés",
                                   "6CB" = "Dépôts triés",
                                   "6CH" = "Dépôts triés",
                                   "6D" = "Dépôts triés",
                                   "6DB" = "Dépôts triés",
                                   "6DH" = "Dépôts triés",
                                   "7" = "Dépôts organiques",
                                   "7F" = "Dépôts organiques",
                                   "8A" = "Dépôts triés",
                                   "8AAE" = "Dépôts d'altération",
                                   "8AAM" = "Dépôts d'altération",
                                   "8B" = "Dépôts d'altération",
                                   "8C" = "Dépôts d'altération",
                                   "8CAE" = "Dépôts d'altération",
                                   "8CAM" = "Dépôts d'altération",
                                   "8D" = "Dépôts triés",
                                   "9D" = "Dépôts triés",
                                   "0R_5A" = "Tills et affleurements",
                                   "1AE_7" = "Tills et affleurements",
                                   "1AM_0R" = "Tills et affleurements",
                                   "1AM_7" = "Tills et affleurements",
                                   "1I" = "Dépôts triés",
                                   "1I_2" = "Dépôts triés",
                                   "1M_1H" = "Dépôts triés",
                                   "1M_2" = "Dépôts triés",
                                   "1M_2BD" = "Dépôts triés",
                                   "2_9" = "Dépôts triés",
                                   "2BD_9" = "Dépôts triés",
                                   "3C" = "Dépôts triés",
                                   "3F_9" = "Dépôts triés",
                                   "4D" = "Dépôts triés",
                                   "5A_1G" = "Dépôts triés",
                                   "5S_0R" = "Dépôts triés",
                                   "5S_1G" = "Dépôts triés",
                                   "5S_9" = "Dépôts triés",
                                   "6_7" = "Dépôts triés",
                                   "6_9" = "Dépôts triés",
                                   "8DPA" = "Dépôts triés",
                                   "7_0R" = "Dépôts organiques",
                                   "7_6" = "Dépôts organiques",
                                   "7_9" = "Dépôts organiques",
                                   "9_7" = "Dépôts triés",
                                   "MS" = "ND",
                                   "1AT" = "Dépôts triés",
                                   "1ATR" = "Dépôts triés",
                                   "7T" = "Dépôts organiques",
                                   "9T" = "Dépôts triés",
                                   "ANT" = "ND",
                                   "ND" = "ND")

  # depots <- terra::extract(y, x, fun=max, na.rm=TRUE)[,2]
  # x$CH_LH_4CL <- classification_hydrologique[match(depots, names(classification_hydrologique))]

  x <- classification_hydrologique[match(x, names(classification_hydrologique))]

  return(x)

}

#' classification_hydrologique_radf
#'
#' @param x a column of surface deposits from MELCCFP raster
#'
#' @returns
#' hydrological classification
#' @export
#'
#' @examples
#' no example
classification_hydrologique_radf <- function(x){ # Depots de surface du MELCCFP

  classification_hydrologique <- c("0" = "CD", # R
                                   "0A" = "CD", # R1AA
                                   "0B" = "n.a.", # R7T
                                   "0R" = "CD", # R
                                   "0S" = "C", # R1
                                   "0T" = "C", # R1
                                   "1AE" = "B", # 1A
                                   "1AEV" = "B", # 1AD
                                   "1AEVB" = "AB", # 1AB
                                   "1AM" = "C", # 1AM
                                   "1AMR" = "C", # 1AM
                                   "1B" = "B", # 1B
                                   "1C" = "B", # 1BD
                                   "1D" = "B", # 1BD
                                   "1G" = "AB", # 1BG
                                   "1H" = "AB", # 1BP
                                   "1M" = "AB", # 1BF
                                   "1R" = "B", # 1BC
                                   "2" = "AB", # 2
                                   "2A" = "AB", # 2A
                                   "2AK" = "AB", # 2AE
                                   "2AT" = "AB", # 2AK - 2AT
                                   "2B" = "AB", # 2B
                                   "2BD" = "AB", # 2BD
                                   "2BE" = "AB", # 2BE
                                   "3D" = "BC", # 3D
                                   "3DB" = "BC", # 3DD
                                   "3F" = "B", # 3A
                                   "3FA" = "AB", # 3AC
                                   "3FB" = "B", # 3AN
                                   "3M" = "B", # 3 - 3M ?
                                   "4A" = "C", # 4GA
                                   "4S" = "AB", # 4GS
                                   "5A" = "CD", # 5A
                                   "5S" = "AB", # 5S
                                   "6A" = "AB", # 6 ?
                                   "6BH" = "B", # 4P
                                   "6C" = "AB", # 6A ?
                                   "6CB" = "AB", # 6A ?
                                   "6CH" = "AB", # 6A ?
                                   "6D" = "B", # 5G - 6G - 6S ?
                                   "6DB" = "B", # 5G - 6G - 6S ?
                                   "6DH" = "B", # 5G - 6G - 6S ?
                                   "7" = "n.a.", # 7
                                   "7F" = "n.a.", # 7T
                                   "8A" = "B", # 8A
                                   "8AAE" = "AB", # 8AP ?
                                   "8AAM" = "B", # 8* ?
                                   "8B" = "AB", # 8E
                                   "8C" = "B", # 8C
                                   "8CAE" = "AB", # 8CY
                                   "8CAM" = "AB", # 8CM
                                   "8D" = "AB", # 8P
                                   "9D" = "AB", # 9
                                   "0R_5A" = "CD", # R
                                   "1AE_7" = "n.a.", # 7
                                   "1AM_0R" = "C", # 1*Y
                                   "1AM_7" = "n.a.", # 7
                                   "1I" = "B", # 1*M ?
                                   "1I_2" = "B", # 1*M ?
                                   "1M_1H" = "B", # 1*M ?
                                   "1M_2" = "B", # 1*M ?
                                   "1M_2BD" = "B", # 1*M ?
                                   "2_9" = "AB", # 2 ?
                                   "2BD_9" = "AB", # 2 ?
                                   "3C" = "AB", # 3AC ?
                                   "3F_9" = "B", # 3A
                                   "4D" = "BC", # 4 ?
                                   "5A_1G" = "CD", # 5A
                                   "5S_0R" = "AB", # 5S
                                   "5S_1G" = "AB", # 5S
                                   "5S_9" = "AB", # 5S
                                   "6_7" = "n.a.", # 7
                                   "6_9" = "AB", # 6 ?
                                   "8DPA" = "BC", # 8G ?
                                   "7_0R" = "n.a.", # 7
                                   "7_6" = "n.a.", # 7
                                   "7_9" = "AB", # 6 ?
                                   "9_7" = "AB", # 9 ?
                                   "MS" = "n.a.",
                                   "1AT" = "B", # 1AD ?
                                   "1ATR" = "B", # 1AD ?
                                   "7T" = "n.a.", # 7T
                                   "9T" = "AB", # 9
                                   "ANT" = "n.a.",
                                   "ND" = "n.a.")

  # depots <- terra::extract(y, x, fun=max, na.rm=TRUE)[,2]
  # x$CH_RADF <- classification_hydrologique[match(depots, names(classification_hydrologique))]

  x <- classification_hydrologique[match(x, names(classification_hydrologique))]

  return(x)

}

#' classification_hydrologique_mailhot_2018
#'
#' @param x a column of surface deposits from MELCCFP raster
#'
#' @returns
#' hydrological classification
#' @export
#'
#' @examples
#' no example
classification_hydrologique_mailhot_2018 <- function(x){ # Depots de surface du MELCCFP

  classification_hydrologique <- c("0" = "ROC", # R
                                   "0A" = "ROC", # R1AA
                                   "0B" = "ROC", # R7T
                                   "0R" = "ROC", # R
                                   "0S" = "ROC", # R1
                                   "0T" = "ROC", # R1
                                   "1AE" = "B", # 1A
                                   "1AEV" = "B", # 1AD
                                   "1AEVB" = "AB", # 1AB
                                   "1AM" = "B", # 1AM
                                   "1AMR" = "B", # 1AM
                                   "1B" = "B", # 1B
                                   "1C" = "B", # 1BD
                                   "1D" = "B", # 1BD
                                   "1G" = "B", # 1BG
                                   "1H" = "B", # 1BP
                                   "1M" = "B", # 1BF
                                   "1R" = "B", # 1BC
                                   "2" = "B", # 2
                                   "2A" = "B", # 2A
                                   "2AK" = "B", # 2AE
                                   "2AT" = "B", # 2AK - 2AT
                                   "2B" = "B", # 2B
                                   "2BD" = "B", # 2BD
                                   "2BE" = "B", # 2BE
                                   "3D" = "C", # 3D
                                   "3DB" = "C", # 3DD
                                   "3F" = "B", # 3A
                                   "3FA" = "C", # 3AC
                                   "3FB" = "C", # 3AN
                                   "3M" = "C", # 3 - 3M ?
                                   "4A" = "C", # 4GA
                                   "4S" = "B", # 4GS
                                   "5A" = "C", # 5A
                                   "5S" = "B", # 5S
                                   "6A" = "B", # 6 ?
                                   "6BH" = "B", # 4P
                                   "6C" = "B", # 6A ?
                                   "6CB" = "B", # 6A ?
                                   "6CH" = "B", # 6A ?
                                   "6D" = "B", # 5G - 6G - 6S ?
                                   "6DB" = "B", # 5G - 6G - 6S ?
                                   "6DH" = "B", # 5G - 6G - 6S ?
                                   "7" = "ORG", # 7
                                   "7F" = "ORG", # 7T
                                   "8A" = "B", # 8A
                                   "8AAE" = "A", # 8AP ?
                                   "8AAM" = "B", # 8* ?
                                   "8B" = "A", # 8E
                                   "8C" = "B", # 8C
                                   "8CAE" = "B", # 8CY
                                   "8CAM" = "B", # 8CM
                                   "8D" = "B", # 8P
                                   "9D" = "B", # 9
                                   "0R_5A" = "ROC", # R
                                   "1AE_7" = "ORG", # 7
                                   "1AM_0R" = "B", # 1*Y
                                   "1AM_7" = "ORG", # 7
                                   "1I" = "B", # 1*M ?
                                   "1I_2" = "B", # 1*M ?
                                   "1M_1H" = "B", # 1*M ?
                                   "1M_2" = "B", # 1*M ?
                                   "1M_2BD" = "B", # 1*M ?
                                   "2_9" = "B", # 2 ?
                                   "2BD_9" = "B", # 2 ?
                                   "3C" = "C", # 3AC ?
                                   "3F_9" = "B", # 3A
                                   "4D" = "B", # 4 ?
                                   "5A_1G" = "C", # 5A
                                   "5S_0R" = "B", # 5S
                                   "5S_1G" = "B", # 5S
                                   "5S_9" = "B", # 5S
                                   "6_7" = "ORG", # 7
                                   "6_9" = "C", # 6 ?
                                   "8DPA" = "B", # 8G ?
                                   "7_0R" = "ORG", # 7
                                   "7_6" = "ORG", # 7
                                   "7_9" = "C", # 6 ?
                                   "9_7" = "B", # 9 ?
                                   "MS" = "n.a.",
                                   "1AT" = "C", # 1AD ?
                                   "1ATR" = "C", # 1AD ?
                                   "7T" = "ORG", # 7T
                                   "9T" = "B", # 9
                                   "ANT" = "n.a.",
                                   "ND" = "n.a.")

  # depots <- terra::extract(y, x, fun=max, na.rm=TRUE)[,2]
  # x$CH_MAILHOT2018 <- classification_hydrologique[match(depots, names(classification_hydrologique))]

  x <- classification_hydrologique[match(x, names(classification_hydrologique))]

  return(x)

}


#' classification_hydrologique_simplify_v1
#'
#' @param x a column of surface deposits from MELCCFP raster
#'
#' @returns
#' hydrological classification
#' @export
#'
#' @examples
#' no example
classification_hydrologique_simplify_v1 <- function(x){ # Depots de surface du MELCCFP

  classification_hydrologique <- c("0" = "Affleurement rocheux",
                                   "0A" = "Affleurement rocheux",
                                   "0B" = "Affleurement rocheux",
                                   "0R" = "Affleurement rocheux",
                                   "0S" = "Affleurement rocheux",
                                   "0T" = "Affleurement rocheux",
                                   "1AE" = "Dépôt glaciaire",
                                   "1AEV" = "Dépôt glaciaire",
                                   "1AEVB" = "Dépôt glaciaire",
                                   "1AM" = "Dépôt glaciaire",
                                   "1AMR" = "Dépôt glaciaire",
                                   "1B" = "Dépôt glaciaire",
                                   "1C" = "Dépôt glaciaire",
                                   "1D" = "Dépôt glaciaire",
                                   "1G" = "Dépôt glaciaire",
                                   "1H" = "Dépôt glaciaire",
                                   "1M" = "Dépôt glaciaire",
                                   "1R" = "Dépôt glaciaire",
                                   "2" = "Dépôt fluvio-glaciaire",
                                   "2A" = "Dépôt fluvio-glaciaire",
                                   "2AK" = "Dépôt fluvio-glaciaire",
                                   "2AT" = "Dépôt fluvio-glaciaire",
                                   "2B" = "Dépôt fluvio-glaciaire",
                                   "2BD" = "Dépôt fluvio-glaciaire",
                                   "2BE" = "Dépôt fluvio-glaciaire",
                                   "3D" = "Dépôt fluviatile",
                                   "3DB" = "Dépôt fluviatile",
                                   "3F" = "Dépôt fluviatile",
                                   "3FA" = "Dépôt fluviatile",
                                   "3FB" = "Dépôt fluviatile",
                                   "3M" = "Dépôt fluviatile",
                                   "4A" = "Dépôt lacustre",
                                   "4S" = "Dépôt lacustre",
                                   "5A" = "Dépôt marin",
                                   "5S" = "Dépôt marin",
                                   "6A" = "Dépôt littoral marin",
                                   "6BH" = "Dépôt littoral marin",
                                   "6C" = "Dépôt littoral marin",
                                   "6CB" = "Dépôt littoral marin",
                                   "6CH" = "Dépôt littoral marin",
                                   "6D" = "Dépôt littoral marin",
                                   "6DB" = "Dépôt littoral marin",
                                   "6DH" = "Dépôt littoral marin",
                                   "7" = "Dépôt organique",
                                   "7F" = "Dépôt organique",
                                   "8A" = "Dépôt de pente et d'altération",
                                   "8AAE" = "Dépôt de pente et d'altération",
                                   "8AAM" = "Dépôt de pente et d'altération",
                                   "8B" = "Dépôt de pente et d'altération",
                                   "8C" = "Dépôt de pente et d'altération",
                                   "8CAE" = "Dépôt de pente et d'altération",
                                   "8CAM" = "Dépôt de pente et d'altération",
                                   "8D" = "Dépôt de pente et d'altération",
                                   "9D" = "Dépôt éolien",
                                   "0R_5A" = "Affleurement rocheux",
                                   "1AE_7" = "Dépôt glaciaire",
                                   "1AM_0R" = "Dépôt glaciaire",
                                   "1AM_7" = "Dépôt glaciaire",
                                   "1I" = "Dépôt glaciaire",
                                   "1I_2" = "Dépôt glaciaire",
                                   "1M_1H" = "Dépôt glaciaire",
                                   "1M_2" = "Dépôt glaciaire",
                                   "1M_2BD" = "Dépôt glaciaire",
                                   "2_9" = "Dépôt fluvio-glaciaire",
                                   "2BD_9" = "Dépôt fluvio-glaciaire",
                                   "3C" = "Dépôt fluviatile",
                                   "3F_9" = "Dépôt fluviatile",
                                   "4D" = "Dépôt lacustre",
                                   "5A_1G" = "Dépôt marin",
                                   "5S_0R" = "Dépôt marin",
                                   "5S_1G" = "Dépôt marin",
                                   "5S_9" = "Dépôt marin",
                                   "6_7" = "Dépôt littoral marin",
                                   "6_9" = "Dépôt littoral marin",
                                   "8DPA" = "Dépôt de pente et d'altération",
                                   "7_0R" = "Dépôt organique",
                                   "7_6" = "Dépôt organique",
                                   "7_9" = "Dépôt littoral marin",
                                   "9_7" = "Dépôt éolien",
                                   "MS" = "ND",
                                   "1AT" = "Dépôt glaciaire",
                                   "1ATR" = "Dépôt glaciaire",
                                   "7T" = "Dépôt organique",
                                   "9T" = "Dépôt éolien",
                                   "ANT" = "ND",
                                   "ND" = "ND")

  # depots <- terra::extract(y, x, fun=max, na.rm=TRUE)[,2]
  # x$CH_SIMPLIFY <- classification_hydrologique[match(depots, names(classification_hydrologique))]

  x <- classification_hydrologique[match(x, names(classification_hydrologique))]

  return(x)

}


#' classification_hydrologique_simplify_v2
#'
#' @param x a column of surface deposits from MELCCFP raster
#'
#' @returns
#' hydrological classification
#' @export
#'
#' @examples
#' no example
classification_hydrologique_simplify_v2 <- function(x){ # Depots de surface du MELCCFP

  classification_hydrologique <- c("0" = "Affleurement rocheux",
                                   "0A" = "Affleurement rocheux",
                                   "0B" = "Affleurement rocheux",
                                   "0R" = "Affleurement rocheux",
                                   "0S" = "Affleurement rocheux",
                                   "0T" = "Affleurement rocheux",
                                   "1AE" = "Till épais",
                                   "1AEV" = "Till épais",
                                   "1AEVB" = "Till épais",
                                   "1AM" = "Till mince",
                                   "1AMR" = "Till mince",
                                   "1B" = "Moraine",
                                   "1C" = "Moraine",
                                   "1D" = "Moraine",
                                   "1G" = "Moraine",
                                   "1H" = "Moraine",
                                   "1M" = "Moraine",
                                   "1R" = "Moraine",
                                   "2" = "Dépôt fluvio-glaciaire",
                                   "2A" = "Dépôt fluvio-glaciaire",
                                   "2AK" = "Dépôt fluvio-glaciaire",
                                   "2AT" = "Dépôt fluvio-glaciaire",
                                   "2B" = "Dépôt fluvio-glaciaire",
                                   "2BD" = "Dépôt fluvio-glaciaire",
                                   "2BE" = "Dépôt fluvio-glaciaire",
                                   "3D" = "Dépôt fluviatile",
                                   "3DB" = "Dépôt fluviatile",
                                   "3F" = "Dépôt fluviatile",
                                   "3FA" = "Dépôt fluviatile",
                                   "3FB" = "Dépôt fluviatile",
                                   "3M" = "Dépôt fluviatile",
                                   "4A" = "Dépôt lacustre calme",
                                   "4S" = "Dépôt lacustre agité",
                                   "5A" = "Dépôt marin calme",
                                   "5S" = "Dépôt marin agité",
                                   "6A" = "Dépôt littoral marin actuel",
                                   "6BH" = "Dépôt littoral marin subactuelle",
                                   "6C" = "Dépôt littoral marin subactuelle",
                                   "6CB" = "Dépôt littoral marin actuel",
                                   "6CH" = "Dépôt littoral marin actuel",
                                   "6D" = "Dépôt littoral marin subactuelle",
                                   "6DB" = "Dépôt littoral marin subactuelle",
                                   "6DH" = "Dépôt littoral marin subactuelle",
                                   "7" = "Dépôt organique",
                                   "7F" = "Dépôt organique",
                                   "8A" = "Dépôt de pente et d'altération",
                                   "8AAE" = "Dépôt de pente et d'altération épais",
                                   "8AAM" = "Dépôt de pente et d'altération mince",
                                   "8B" = "Dépôt de pente et d'altération colluvion",
                                   "8C" = "Dépôt de pente et d'altération colluvion",
                                   "8CAE" = "Dépôt de pente et d'altération colluvion",
                                   "8CAM" = "Dépôt de pente et d'altération colluvion",
                                   "8D" = "Dépôt de pente et d'altération colluvion",
                                   "9D" = "Dépôt éolien",
                                   "0R_5A" = "Affleurement rocheux",
                                   "1AE_7" = "Till épais",
                                   "1AM_0R" = "Till mince",
                                   "1AM_7" = "Till mince",
                                   "1I" = "Moraine",
                                   "1I_2" = "Moraine",
                                   "1M_1H" = "Moraine",
                                   "1M_2" = "Moraine",
                                   "1M_2BD" = "Moraine",
                                   "2_9" = "Dépôt fluvio-glaciaire",
                                   "2BD_9" = "Dépôt fluvio-glaciaire",
                                   "3C" = "Dépôt fluviatile",
                                   "3F_9" = "Dépôt fluviatile",
                                   "4D" = "Dépôt lacustre",
                                   "5A_1G" = "Dépôt marin calme",
                                   "5S_0R" = "Dépôt marin agité",
                                   "5S_1G" = "Dépôt marin agité",
                                   "5S_9" = "Dépôt marin agité",
                                   "6_7" = "Dépôt littoral marin",
                                   "6_9" = "Dépôt littoral marin",
                                   "8DPA" = "Dépôt de pente et d'altération colluvion",
                                   "7_0R" = "Dépôt organique",
                                   "7_6" = "Dépôt organique",
                                   "7_9" = "Dépôt littoral marin",
                                   "9_7" = "Dépôt éolien",
                                   "MS" = "ND",
                                   "1AT" = "Till épais",
                                   "1ATR" = "Till épais",
                                   "7T" = "Dépôt organique",
                                   "9T" = "Dépôt éolien",
                                   "ANT" = "ND",
                                   "ND" = "ND")

  # depots <- terra::extract(y, x, fun=max, na.rm=TRUE)[,2]
  # x$CH_SIMPLIFY <- classification_hydrologique[match(depots, names(classification_hydrologique))]

  x <- classification_hydrologique[match(x, names(classification_hydrologique))]

  return(x)

}
