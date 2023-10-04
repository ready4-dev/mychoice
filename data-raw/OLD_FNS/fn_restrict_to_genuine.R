
#' @title remove_invalid_responses
#' @description FUNCTION_DESCRIPTION
#' @param responses PARAM_DESCRIPTION
#' @param ip_long_lat_vnm_vec PARAM_DESCRIPTION
#' @param required_country PARAM_DESCRIPTION
#' @param attempt_date_vnm PARAM_DESCRIPTION
#' @param dob_vnm PARAM_DESCRIPTION
#' @param age_vnm PARAM_DESCRIPTION
#' @param email_vnm PARAM_DESCRIPTION
#' @param free_text_q_1 PARAM_DESCRIPTION
#' @param qual_1_vnm PARAM_DESCRIPTION
#' @param qual_rf_cond PARAM_DESCRIPTION
#' @param conseq_digit_thresh PARAM_DESCRIPTION
#' @param min_attempt_duration PARAM_DESCRIPTION
#' @param flags PARAM_DESCRIPTION
#' @param max_nbr_flags PARAM_DESCRIPTION
#' @param allow_all_qual_1_cond PARAM_DESCRIPTION, Default: 'NA'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[purrr]{map}}
#' @rdname remove_invalid_responses
#' @export
#' @importFrom dplyr mutate filter
#' @importFrom rlang sym
#' @importFrom purrr map_lgl
remove_invalid_responses <- function(responses,
                                ip_long_lat_vnm_vec,
                                required_country,
                                attempt_date_vnm,
                                dob_vnm,
                                age_vnm,
                                email_vnm,
                                free_text_q_1,
                                qual_1_vnm,
                                qual_rf_cond,
                                conseq_digit_thresh,
                                min_attempt_duration,
                                flags,
                                max_nbr_flags,
                                allow_all_qual_1_cond = NA_character_){
  responses <- add_ip_country_rf(responses = responses,
                                 ip_long_lat_vnm_vec = ip_long_lat_vnm_vec,
                                 required_country = required_country) %>%
    add_inconsistent_age_rf(attempt_date_vnm = attempt_date_vnm,
                            dob_vnm = dob_vnm,
                            age_vnm = age_vnm) %>%
    add_unusual_email_rf(email_vnm = email_vnm,
                         conseq_digit_thresh = conseq_digit_thresh)
  ## Check for duplicate entries in the assessment of service use quesiton
  responses <- responses %>% dplyr::mutate(rf_dupl_su = duplicated(!!rlang::sym(free_text_q_1))) %>%
    ## Check for qualitive assessment of service use question flag
    dplyr::mutate(rf_qual_su = purrr::map_lgl(!!rlang::sym(qual_1_vnm), ~ ifelse(.x==qual_rf_cond,T,F))) %>%
    ## Check for too rapid survey completion
    dplyr::mutate(rf_too_quick = purrr::map_lgl(`Duration (in seconds)`, ~ ifelse(as.numeric(.x) < min_attempt_duration,T,F)))
  responses <- total_rf(responses = responses,
                        new_vnm = "red_flag_count",
                        flags = flags)
  if(is.na(allow_all_qual_1_cond))
    responses <- responses %>% dplyr::filter(red_flag_count <= max_nbr_flags)
  else
    responses <- responses %>% dplyr::filter(red_flag_count <= max_nbr_flags | !!rlang::sym(qual_1_vnm) ==allow_all_qual_1_cond)
  return(responses)
}

#' @title add_ip_country_rf
#' @description FUNCTION_DESCRIPTION
#' @param responses PARAM_DESCRIPTION
#' @param ip_long_lat_vnm_vec PARAM_DESCRIPTION
#' @param required_country PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sf]{st_as_sf}},\code{\link[sf]{geos_binary_ops}},\code{\link[sf]{st_crs<-}},\code{\link[sf]{st_crs}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}},\code{\link[dplyr]{join}},\code{\link[dplyr]{mutate}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[rnaturalearth]{ne_countries}}
#'  \code{\link[purrr]{map}}
#' @rdname add_ip_country_rf
#' @export
#' @importFrom sf st_as_sf st_intersection st_crs<- st_crs
#' @importFrom dplyr filter select full_join mutate
#' @importFrom rlang sym
#' @importFrom rnaturalearth ne_countries
#' @importFrom purrr map_lgl
add_ip_country_rf <- function(responses,
                              ip_long_lat_vnm_vec,
                              required_country){
  back_tick_names <- names(responses)[!names(responses) == names(responses) %>% make.names()]
  ## Check for country location
  ## Modified from: https://gis.stackexchange.com/questions/297691/assign-country-names-to-long-and-lat-in-r
  sfRegion <- sf::st_as_sf(responses %>%
                             dplyr::filter(!is.na(!!rlang::sym(ip_long_lat_vnm_vec[1]))) %>%
                             dplyr::filter(!is.na(!!rlang::sym(ip_long_lat_vnm_vec[2]))), coords = ip_long_lat_vnm_vec)
  sfCountry <- rnaturalearth::ne_countries(returnclass='sf')
  sfIntersection <- sf::st_intersection(sfCountry,sf::`st_crs<-`(sfRegion,sf::st_crs(sfCountry))) %>%
    dplyr::select(admin,
                  names(responses)[!names(responses) %in% c(ip_long_lat_vnm_vec, back_tick_names)])
  responses <- dplyr::full_join(responses %>%
                                  dplyr::select(names(responses)[!names(responses) %in% ip_long_lat_vnm_vec]),
                                sfIntersection)
  responses %>%
    dplyr::mutate(rf_ip_address = purrr::map_lgl(admin, ~ ifelse(.x == required_country, F,T)))
}

#' @title FUNCTION_TITLE
#' @description add_inconsistent_age_rf
#' @param responses PARAM_DESCRIPTION
#' @param attempt_date_vnm PARAM_DESCRIPTION
#' @param dob_vnm PARAM_DESCRIPTION
#' @param age_vnm PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[janitor]{excel_numeric_to_date}}
#'  \code{\link[lubridate]{interval}},\code{\link[lubridate]{period}}
#'  \code{\link[purrr]{map2}}
#' @rdname add_inconsistent_age_rf
#' @export
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @importFrom janitor excel_numeric_to_date
#' @importFrom lubridate interval years
#' @importFrom purrr map2_lgl
add_inconsistent_age_rf <- function(responses,
                                    attempt_date_vnm,#StartDate
                                    dob_vnm, #Ent_DCE_DOB
                                    age_vnm){ # Ent_DCE_Age
  ## Check for consistent age reporting
  ## Based on: https://stackoverflow.com/questions/32312925/time-difference-in-years-with-lubridate
  responses <-  responses %>%
    dplyr::mutate(der_date_of_input = !!rlang::sym(attempt_date_vnm) %>% as.numeric() %>% janitor::excel_numeric_to_date()) %>%
    dplyr::mutate(der_date_of_birth = !!rlang::sym(dob_vnm) %>% as.numeric() %>% janitor::excel_numeric_to_date()) %>%
    dplyr::mutate(der_age_from_dob = lubridate::interval(der_date_of_birth,der_date_of_input) %/% lubridate::years(1))
  responses %>%
    dplyr::mutate(rf_age_inconsistency = purrr::map2_lgl(!!rlang::sym(age_vnm),der_age_from_dob,
                                                         ~ ifelse(.x==.y,F,T)))
}

#' @title add_unusual_email_rf
#' @description FUNCTION_DESCRIPTION
#' @param responses PARAM_DESCRIPTION
#' @param email_vnm PARAM_DESCRIPTION
#' @param conseq_digit_thresh PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[rlang]{sym}}
#' @rdname add_unusual_email_rf
#' @export
#' @importFrom dplyr mutate
#' @importFrom purrr map map_lgl
#' @importFrom rlang sym
add_unusual_email_rf <- function(responses,
                                 email_vnm, #Ent_DCE_TY_1
                                 conseq_digit_thresh){
  ## Check for unusual email format with five consecutive digits
  responses %>% dplyr::mutate(rf_email_address = purrr::map(!!rlang::sym(email_vnm), ~ get_digits_from_text(.x)) %>%
                                purrr::map( ~ nchar(.x) %>% max()) %>%
                                purrr::map_lgl(~ifelse(.x>=conseq_digit_thresh,T,F)))
}

#' @title total_rf
#' @description FUNCTION_DESCRIPTION
#' @param responses PARAM_DESCRIPTION
#' @param new_vnm PARAM_DESCRIPTION
#' @param flags PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[purrr]{reduce}}
#' @rdname total_rf
#' @export
#' @importFrom dplyr mutate select
#' @importFrom rlang sym
#' @importFrom purrr reduce
total_rf <- function(responses,
                     new_vnm,
                     flags){
  responses %>%
    dplyr::mutate(!!rlang::sym(new_vnm) := purrr::reduce(responses %>% dplyr::select(flags),
                                                         .init = 0,
                                                         ~ .x + .y))

}
