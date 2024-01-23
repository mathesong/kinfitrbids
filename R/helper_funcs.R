#' Extract filterable attributes from a BIDS list
#'
#' This function extracts the attributes which could be used for filtering. By
#' this, it extracts all parts of the list which are either character or numeric
#' which also have a length of 1. This allows us to filter our data for a
#' specific tracer, or injection type.
#'
#' @param list The list of PET data or blood data
#'
#' @return A data.frame containing the filterable attributes
#' @export
get_filterable_attributes <- function(list) {

  lengths <- purrr::map_dbl(list, length) == 1
  types <- purrr::map_dbl(list, ~is.character(.x)  || is.numeric(.x))
  keep <- lengths * types

  outlist <- list[which(keep==1)]

  outlist <- outlist[which(names(outlist) %in% c("TracerName",
                        "seg",
                        "pvc",
                        "deriv"))]

  tibble::as_tibble(outlist)


}

#' @export
get_petname <- function(filedata) {

  petname <- filedata %>%
    dplyr::filter(measurement == "blood") %>%
    dplyr::slice(1)

  petname <- basename(petname$path)
  stringr::str_remove(petname,
                                 "_tacs.*")

}

#' @export
attributes_to_title <- function(bidsdata, all_attributes = FALSE) {

  if( !all_attributes ) {
    bidsdata <- bidsdata[,which(!apply(bidsdata, 2,
                                       FUN = function(x) length(unique(x))==1))]
  }

  cnames <- colnames(bidsdata)

  filedata_colno <- which(cnames=="filedata")

  cname_attributes <- cnames[1:(filedata_colno-1)]
  attributes <- bidsdata[1:(filedata_colno-1)]

  # i for rows --> attributes
  # j for columns --> measurements

  title <- rep("", times=nrow(attributes))

  for(j in 1:nrow(attributes)) {
    for(i in 1:length(cname_attributes)) {
      title[j] <- paste0(title[j], cname_attributes[i], "-", attributes[j,i], "_")
    }
  }

  stringr::str_remove(title, "_$")

}



