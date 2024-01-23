#' @export
parse_config_subsets <- function(config) {

  # Remove whitespace
  sub <- stringr::str_remove_all(config$Subsets$sub, " ")
  ses <- stringr::str_remove_all(config$Subsets$ses, " ")
  rec <- stringr::str_remove_all(config$Subsets$rec, " ")
  TracerName <- stringr::str_remove_all(config$Subsets$TracerName, " ")
  seg <- stringr::str_remove_all(config$Region$seg, " ")
  pvc <- stringr::str_remove_all(config$Region$pvc, " ")
  deriv <- stringr::str_remove_all(config$Region$deriv, " ")
  

  sub <-  str_split(sub, ";")[[1]]
  ses <-  str_split(ses, ";")[[1]]
  rec <-  str_split(rec, ";")[[1]]
  TracerName <- str_split(TracerName, ";")[[1]]
  seg <- str_split(seg, ";")[[1]]
  pvc <- str_split(pvc, ";")[[1]]
  deriv <- str_split(deriv, ";")[[1]]


  all_tibble <- tibble::as_tibble(
    expand.grid(sub = sub,
              ses = ses,
              rec = rec,
              TracerName = TracerName,
              seg = seg,
              pvc = pvc,
              deriv = deriv)) %>%
    dplyr::mutate(across(where(is.factor), as.character))

  clean_tibble <- all_tibble[,!apply(all_tibble, 2, function(x) (all(x=="")))]

}

#' @export
all_identifiers_to_character <- function(bidsdata) {

  cnames <- colnames(bidsdata)
  filedata_colno <- which(cnames=="filedata")

  bidsdata %>%
    mutate(across(1:(filedata_colno-1), as.character))

}
