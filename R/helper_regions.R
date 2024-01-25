volweights_single <- function(volume, region, roi_regions) {

  tibble::tibble(
    region = region,
    Volume = volume
  ) %>%
    dplyr::filter(region %in% roi_regions) %>%
    dplyr::mutate(volweight = volume / sum(volume)) %>%
    dplyr::select(region, volweight)

}

tac_combine_single <- function(t_tac, tac, region,
                               volume, volume_region,
                               roi_regions = NA) {


  if(is.na(roi_regions)) {
    roi_regions <- unique(volume_region)
  }

  tacs <- tibble::tibble(
    region = region,
    t_tac = t_tac,
    tac = tac
  )

  volweights <- volweights_single(volume, volume_region, roi_regions)

  dplyr::inner_join(tacs, volweights) %>%
    dplyr::group_by(t_tac) %>%
    summarise(tac = mean(tac * volweight))

}

tac_combine_sample <- function(roi_regions, t_tac, tac, region, tac_id,
                               volume, volume_region, volume_id ) {

  roi_regions <- stringr::str_remove_all(roi_regions, pattern=" ")
  roi_regions <- stringr::str_split(roi_regions, pattern=";")[[1]]

  alltacs <- tibble::tibble(
    id = tac_id,
    t_tac = t_tac,
    tac = tac,
    region = region
  ) %>%
    dplyr::filter(region %in% roi_regions) %>%
    dplyr::group_by(id) %>%
    tidyr::nest(.key = "tacs")

  allvols <- tibble::tibble(
    id = volume_id,
    region = volume_region,
    volume = volume
  ) %>%
    dplyr::filter(region %in% roi_regions) %>%
    dplyr::group_by(id) %>%
    tidyr::nest(.key = "volumes")

  dplyr::inner_join(alltacs, allvols, by="id") %>%
    dplyr::group_by(id) %>%
    mutate(newtac = map2(tacs, volumes,
                       ~suppressMessages(
                         tac_combine_single(
                                           .x$t_tac,
                                           .x$tac,
                                           .x$region,
                                           .y$volume,
                                           .y$region,
                                           NA)))) %>%
    dplyr::ungroup() %>%
    dplyr::select(pet = id, newtac) %>%
    tidyr::unnest(newtac)

}
