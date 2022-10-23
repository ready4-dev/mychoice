#' Transform replication dataset for analysis
#' @description transform_repln_ds_for_analysis() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform replication dataset for analysis. Function argument repln_ds_tb specifies the object to be updated. Argument areas_lup provides the object to be updated. The function returns Transformed replication dataset (a tibble).
#' @param repln_ds_tb Replication dataset (a tibble)
#' @param areas_lup Areas (a lookup table), Default: NULL
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param person_uid_var_nm_1L_chr Person unique identifier variable name (a character vector of length one), Default: 'person_uid'
#' @param seifa_lup Socio-Economic Indices for Areas (a lookup table), Default: NULL
#' @param set_idx_1L_int Set index (an integer vector of length one), Default: 2
#' @param write_to_1L_chr Write to (a character vector of length one), Default: character(0)
#' @return Transformed replication dataset (a tibble)
#' @rdname transform_repln_ds_for_analysis
#' @export 
#' @importFrom dplyr mutate n rename rowwise c_across starts_with ungroup case_when select
#' @importFrom rlang sym
#' @importFrom purrr map_lgl map_chr map_dbl
#' @importFrom ready4 get_from_lup_obj
transform_repln_ds_for_analysis <- function (repln_ds_tb, areas_lup = NULL, consent_1L_chr = "", 
    person_uid_var_nm_1L_chr = "person_uid", seifa_lup = NULL, 
    set_idx_1L_int = 2L, write_to_1L_chr = character(0)) 
{
    if (identical(write_to_1L_chr, character(0))) 
        write_to_1L_chr <- tempdir()
    tfd_repln_ds_tb <- repln_ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(person_uid_var_nm_1L_chr), 
        1:dplyr::n())) %>% dplyr::rename(rc_age = Ent_DCE_Age) %>% 
        dplyr::mutate(rc_age = as.numeric(rc_age)) %>% dplyr::mutate(der_age_u20 = purrr::map_lgl(rc_age, 
        ~ifelse(.x < 20, T, F))) %>% dplyr::mutate(der_gender = purrr::map_chr(Ent_DCE_gender, 
        ~switch(.x %>% as.numeric(), "Male", "Female", "Other", 
            "Prefer_NTS")) %>% as.factor()) %>% dplyr::mutate(der_gender_male = purrr::map_lgl(der_gender, 
        ~ifelse(.x == "Male", T, F))) %>% dplyr::mutate(der_gender_opnts = purrr::map_lgl(der_gender, 
        ~ifelse(.x %in% c("Other", "Prefer_NTS"), T, F))) %>% 
        dplyr::mutate(rc_pilot_participant = purrr::map_dbl(Ent_DCE_Entpt, 
            ~ifelse(as.numeric(.x) == -99, 3, as.numeric(.x)))) %>% 
        dplyr::mutate(rc_pilot_participant = purrr::map_lgl(rc_pilot_participant, 
            ~switch(.x %>% as.numeric(), T, F, NA))) %>% add_sias_totals(itm_prefix_1L_chr = "Ent_DCE_SIAS", 
        drvd_var_prefix_1L_chr = "der", sias_ctg_sfx_1L_chr = "SIAS_ctg", 
        sias_ttl_sfx_1L_chr = "SIAS_ttl") %>% dplyr::mutate(der_SIAS_nr = purrr::map_lgl(der_SIAS_ctg, 
        ~ifelse(.x == "Normal_Range", T, F))) %>% dplyr::mutate(der_SIAS_sa = purrr::map_lgl(der_SIAS_ctg, 
        ~ifelse(.x == "Social_Anxiety", T, F))) %>% dplyr::rename(rc_time_taken_secs = `Duration (in seconds)`) %>% 
        dplyr::mutate(rc_time_taken_secs = as.numeric(rc_time_taken_secs)) %>% 
        dplyr::mutate(der_under_ten_mins = purrr::map_lgl(rc_time_taken_secs, 
            ~ifelse(.x < 600, T, F)))
    if (is.null(areas_lup)) {
        areas_lup <- make_areas_lup(matches_chr = records_ls$ds_tb$Ent_DCE_postcode, 
            consent_1L_chr = consent_1L_chr, write_to_1L_chr = write_to_1L_chr)
    }
    if (is.null(seifa_lup)) {
        seifa_lup <- make_seifa_lup(url_1L_chr = "https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&2033055001%20-%20poa%20indexes.xls&2033.0.55.001&Data%20Cubes&DC124D1DAC3D9FDDCA25825D000F9267&0&2016&27.03.2018&Latest", 
            fl_nm_1L_chr = "2033055001 - poa indexes.xls", consent_1L_chr = consent_1L_chr, 
            write_to_1L_chr = write_to_1L_chr)
    }
    tfd_repln_ds_tb <- tfd_repln_ds_tb %>% dplyr::mutate(der_SOS = purrr::map_chr(Ent_DCE_postcode, 
        ~ifelse(.x %in% areas_lup$POA_CODE16, ready4::get_from_lup_obj(areas_lup, 
            match_var_nm_1L_chr = "POA_CODE16", match_value_xx = .x, 
            target_var_nm_1L_chr = "SOS_NAME16"), NA_character_)) %>% 
        as.factor()) %>% dplyr::mutate(der_STE = purrr::map_chr(Ent_DCE_postcode, 
        ~ifelse(.x %in% areas_lup$POA_CODE16, ready4::get_from_lup_obj(areas_lup, 
            match_var_nm_1L_chr = "POA_CODE16", match_value_xx = .x, 
            target_var_nm_1L_chr = "STE_NAME16"), NA_character_)) %>% 
        as.factor()) %>% dplyr::mutate(der_SEIFA_Quartile = purrr::map_chr(as.numeric(Ent_DCE_postcode), 
        ~ifelse(.x %in% seifa_lup$postcode_dbl, paste0("SEIFA Quartile ", 
            ready4::get_from_lup_obj(seifa_lup, match_var_nm_1L_chr = "postcode_dbl", 
                match_value_xx = .x, target_var_nm_1L_chr = "quartile_dbl")), 
            NA_character_)) %>% as.factor()) %>% dplyr::rowwise() %>% 
        dplyr::mutate(der_Missing_Tasks = sum(is.na(dplyr::c_across(dplyr::starts_with(records_ls$choice_vars_pfx_1L_chr)))) - 
            length(dce_design_ls$choice_cards_ls[[set_idx_1L_int]]$block_idxs_ls[[1]])) %>% 
        dplyr::ungroup() %>% dplyr::mutate(der_All_Tasks = der_Missing_Tasks == 
        0)
    tfd_repln_ds_tb <- tfd_repln_ds_tb %>% dplyr::mutate(der_urban = dplyr::case_when(der_SOS %in% 
        c("Major Urban", "Other Urban") ~ T, T ~ F)) %>% dplyr::mutate(der_STE_VIC = dplyr::case_when(der_STE == 
        "Victoria" ~ T, T ~ F)) %>% dplyr::mutate(der_STE_QLD = dplyr::case_when(der_STE == 
        "Queensland" ~ T, T ~ F)) %>% dplyr::mutate(der_STE_SA = dplyr::case_when(der_STE == 
        "South Australia" ~ T, T ~ F)) %>% dplyr::mutate(der_STE_WA = dplyr::case_when(der_STE == 
        "Western Australia" ~ T, T ~ F)) %>% dplyr::mutate(der_STE_ACT = dplyr::case_when(der_STE == 
        "Australian Capital Territory" ~ T, T ~ F)) %>% dplyr::mutate(der_STE_TAS = dplyr::case_when(der_STE == 
        "Tasmania" ~ T, T ~ F)) %>% dplyr::mutate(der_SEIFA_Q1 = dplyr::case_when(der_SEIFA_Quartile == 
        "SEIFA Quartile 1" ~ T, T ~ F)) %>% dplyr::mutate(der_SEIFA_Q2 = dplyr::case_when(der_SEIFA_Quartile == 
        "SEIFA Quartile 2" ~ T, T ~ F)) %>% dplyr::mutate(der_SEIFA_Q4 = dplyr::case_when(der_SEIFA_Quartile == 
        "SEIFA Quartile 4" ~ T, T ~ F))
    tfd_repln_ds_tb <- tfd_repln_ds_tb %>% dplyr::select(-c(Ent_DCE_TY_1, 
        IPAddress, ResponseId, Ent_DCE_DOB, Ent_DCE_SAsupport, 
        Q86))
    return(tfd_repln_ds_tb)
}
#' Transform to excel date format
#' @description transform_to_excel_date_fmt() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform to excel date format. Function argument date_chr specifies the object to be updated. Argument format_1L_chr provides the object to be updated. The function is called for its side effects and does not return a value.
#' @param date_chr Date (a character vector)
#' @param format_1L_chr Format (a character vector of length one), Default: 'ymd HMS'
#' @param index_date_1L_chr Index date (a character vector of length one), Default: '1899-12-30'
#' @return NULL
#' @rdname transform_to_excel_date_fmt
#' @export 
#' @importFrom lubridate parse_date_time as_date interval int_flip days
#' @keywords internal
transform_to_excel_date_fmt <- function (date_chr, format_1L_chr = "ymd HMS", index_date_1L_chr = "1899-12-30") 
{
    lubridate::parse_date_time(date_chr, format_1L_chr) %>% lubridate::as_date() %>% 
        lubridate::interval(as.Date(index_date_1L_chr)) %>% lubridate::int_flip()%/%lubridate::days(1)
}
