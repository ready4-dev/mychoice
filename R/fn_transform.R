#' Transform descriptives tibble
#' @description transform_descvs_tb() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform descriptives tibble. Function argument descvs_tb specifies the object to be updated. The function returns Transformed descriptives (a tibble).
#' @param descvs_tb Descriptives (a tibble)
#' @return Transformed descriptives (a tibble)
#' @rdname transform_descvs_tb
#' @export 
#' @importFrom dplyr mutate group_by n filter ungroup select
#' @importFrom purrr map2_chr
#' @keywords internal
transform_descvs_tb <- function (descvs_tb) 
{
    tfd_descvs_tb <- descvs_tb %>% dplyr::mutate(missing_var_chr = Measure %>% 
        purrr::map2_chr(Concept, ~ifelse(.x == "Missing", paste0(.y, 
            "_Missing"), "Ignore"))) %>% dplyr::group_by(missing_var_chr) %>% 
        dplyr::mutate(mssng_idx = 1:dplyr::n()) %>% dplyr::mutate(keep_lgl = (mssng_idx == 
        max(mssng_idx)) | (missing_var_chr == "Ignore")) %>% 
        dplyr::filter(keep_lgl) %>% dplyr::ungroup() %>% dplyr::select(-c(missing_var_chr, 
        mssng_idx, keep_lgl))
    return(tfd_descvs_tb)
}
#' Transform model summary
#' @description transform_mdl_smry() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform model summary. Function argument mdl_smry_tb specifies the object to be updated. Argument dce_design_ls provides the object to be updated. The function returns Model summary (a tibble).
#' @param mdl_smry_tb Model summary (a tibble)
#' @param dce_design_ls Discrete choice experiment design (a list)
#' @param records_ls Records (a list)
#' @param mdl_params_ls Model parameters (a list), Default: NULL
#' @param confidence_intvl_1L_chr Confidence interval (a character vector of length one), Default: character(0)
#' @param confidence_intvl_int Confidence interval (an integer vector), Default: integer(0)
#' @param digits_1L_int Digits (an integer vector of length one), Default: integer(0)
#' @param opt_out_nm_1L_chr Opt out name (a character vector of length one), Default: 'Opt out'
#' @param stat_1L_chr Statistic (a character vector of length one), Default: 'z'
#' @return Model summary (a tibble)
#' @rdname transform_mdl_smry
#' @export 
#' @importFrom dplyr mutate select everything case_when across arrange
#' @importFrom purrr map2_chr map_lgl reduce map_chr flatten_chr map map2_lgl
#' @importFrom stringr str_sub str_remove str_replace str_replace_all
#' @importFrom ready4 get_from_lup_obj make_list_phrase
#' @importFrom Hmisc capitalize
#' @keywords internal
transform_mdl_smry <- function (mdl_smry_tb, dce_design_ls, records_ls, mdl_params_ls = NULL, 
    confidence_intvl_1L_chr = character(0), confidence_intvl_int = integer(0), 
    digits_1L_int = integer(0), opt_out_nm_1L_chr = "Opt out", 
    stat_1L_chr = "z") 
{
    names_chr <- c("Parameter", "Estimate", "Std. Error", paste0(stat_1L_chr, 
        "-value"), paste0("Pr(>|", stat_1L_chr, "|)"), "signif.")
    if (!identical(confidence_intvl_int, integer(0))) {
        names_chr <- append(names_chr, c("lwr", "upr"), after = confidence_intvl_int[1] - 
            1)
    }
    names(mdl_smry_tb) <- names_chr
    if (!identical(confidence_intvl_1L_chr, character(0))) {
        mdl_smry_tb <- mdl_smry_tb %>% dplyr::mutate(CI = lwr %>% 
            purrr::map2_chr(upr, ~paste0("(", .x, ", ", .y, ")")))
    }
    mdl_smry_tb <- mdl_smry_tb %>% dplyr::select(c("Parameter", 
        "Estimate", confidence_intvl_1L_chr, "Std. Error", paste0(stat_1L_chr, 
            "-value"), paste0("Pr(>|", stat_1L_chr, "|)"), "signif."))
    fctr_combined_ls <- make_fctr_atts_dummy_var_nms(dce_design_ls$choice_sets_ls$att_lvls_tb, 
        flatten_1L_lgl = F)
    fctr_separate_ls <- get_fctr_atts_dummy_var_nms(dce_design_ls$choice_sets_ls$att_lvls_tb, 
        flatten_1L_lgl = F)
    classes_ls <- mdl_smry_tb$Parameter %>% strsplit("[.]")
    class_lgl <- classes_ls %>% purrr::map_lgl(~.x[1] == "class")
    if (class_lgl %>% any()) 
        mdl_smry_tb <- mdl_smry_tb %>% dplyr::mutate(Class = class_lgl %>% 
            purrr::map2_chr(classes_ls, ~ifelse(.x, paste0(.y[1], 
                ".", .y[2], "."), ifelse(startsWith(.y[1], "(class)"), 
                paste0("class.", stringr::str_sub(.y[1], start = 8)), 
                "")))) %>% dplyr::mutate(Parameter = Class %>% 
            purrr::map2_chr(Parameter, ~ifelse(.x == "", .y, 
                ifelse(startsWith(.y, "(class)"), paste0("Class ", 
                  stringr::str_sub(.y, start = 8)), stringr::str_remove(.y, 
                  .x)))))
    mdl_smry_tb <- purrr::reduce(1:length(fctr_combined_ls), 
        .init = mdl_smry_tb %>% dplyr::mutate(Concept = NA_character_), 
        ~{
            idx_1L_int <- .y
            dummys_chr <- fctr_combined_ls[[idx_1L_int]]
            combined_chr <- fctr_separate_ls[[idx_1L_int]]
            concept_1L_chr <- names(fctr_separate_ls)[idx_1L_int]
            tbl_tb <- .x %>% dplyr::mutate(Concept = Parameter %>% 
                purrr::map2_chr(Concept, ~ifelse(.x %in% dummys_chr, 
                  concept_1L_chr, ifelse(is.na(.y), .x, .y))))
            tbl_tb %>% dplyr::mutate(Parameter = Parameter %>% 
                purrr::map_chr(~ifelse(.x %in% dummys_chr, ready4::get_from_lup_obj(dce_design_ls$choice_sets_ls$att_lvls_tb, 
                  match_value_xx = combined_chr[.x == dummys_chr], 
                  match_var_nm_1L_chr = "dummy_nm_chr", target_var_nm_1L_chr = "short_nms_chr"), 
                  ifelse(.x %in% get_atts(dce_design_ls$choice_sets_ls$att_lvls_tb, 
                    return_1L_chr = "cont"), ready4::get_from_lup_obj(dce_design_ls$choice_sets_ls$att_lvls_tb, 
                    match_value_xx = .x, match_var_nm_1L_chr = "attribute_chr", 
                    target_var_nm_1L_chr = "short_nms_chr") %>% 
                    unique(), ifelse(.x == records_ls$opt_out_var_nm_1L_chr, 
                    opt_out_nm_1L_chr, .x)))))
        }) %>% dplyr::select(Concept, dplyr::everything())
    if (!is.null(mdl_params_ls)) {
        att_lvls_chr <- c(fctr_combined_ls %>% purrr::flatten_chr(), 
            get_atts(dce_design_ls$choice_sets_ls$att_lvls_tb, 
                return_1L_chr = "cont"), records_ls$opt_out_var_nm_1L_chr)
        att_concepts_chr <- c(1:length(fctr_separate_ls) %>% 
            purrr::map(~rep(names(fctr_separate_ls)[.x], length(fctr_separate_ls[[.x]]))) %>% 
            purrr::flatten_chr(), get_atts(dce_design_ls$choice_sets_ls$att_lvls_tb, 
            return_1L_chr = "cont"), opt_out_nm_1L_chr)
        att_short_nms_chr <- c(fctr_separate_ls %>% purrr::flatten_chr() %>% 
            purrr::map_chr(~ready4::get_from_lup_obj(dce_design_ls$choice_sets_ls$att_lvls_tb, 
                match_var_nm_1L_chr = "dummy_nm_chr", match_value_xx = .x, 
                target_var_nm_1L_chr = "short_nms_chr")), get_atts(dce_design_ls$choice_sets_ls$att_lvls_tb, 
            return_1L_chr = "cont") %>% purrr::map_chr(~ready4::get_from_lup_obj(dce_design_ls$choice_sets_ls$att_lvls_tb, 
            match_var_nm_1L_chr = "attribute_chr", match_value_xx = .x, 
            target_var_nm_1L_chr = "short_nms_chr") %>% unique()), 
            opt_out_nm_1L_chr)
        mdl_smry_tb <- mdl_smry_tb %>% dplyr::mutate(Concept = dplyr::case_when(Concept %>% 
            purrr::map_lgl(~{
                concept_1L_chr <- .x
                paste0(".", mdl_params_ls$candidate_predrs_tb$short_name_chr) %>% 
                  purrr::map_lgl(~endsWith(concept_1L_chr, .x)) %>% 
                  any()
            }) ~ Concept %>% strsplit("[.]") %>% purrr::map_chr(~{
            person_features_chr <- .x[.x %in% mdl_params_ls$candidate_predrs_tb$short_name_chr]
            att_features_chr <- setdiff(.x, person_features_chr)
            att_features_1L_chr <- att_concepts_chr[which(att_features_chr == 
                att_lvls_chr)] %>% ready4::make_list_phrase()
            if (identical(person_features_chr, character(0))) {
                person_features_chr <- NA_character_
            }
            else {
                paste0(att_features_1L_chr, " x ", person_features_chr %>% 
                  purrr::map_chr(~paste0(ready4::get_from_lup_obj(mdl_params_ls$candidate_predrs_tb, 
                    match_var_nm_1L_chr = "short_name_chr", match_value_xx = .x, 
                    target_var_nm_1L_chr = "concept_chr") %>% 
                    Hmisc::capitalize())) %>% ready4::make_list_phrase())
            }
        }), Concept %>% purrr::map_lgl(~{
            concept_1L_chr <- .x
            paste0(":", mdl_params_ls$candidate_predrs_tb$short_name_chr) %>% 
                purrr::map2_lgl(paste0(mdl_params_ls$candidate_predrs_tb$short_name_chr, 
                  ":"), ~endsWith(concept_1L_chr, .x) | startsWith(concept_1L_chr, 
                  .y)) %>% any()
        }) ~ Concept %>% strsplit("[:]") %>% purrr::map_chr(~{
            person_features_chr <- .x[.x %in% mdl_params_ls$candidate_predrs_tb$short_name_chr]
            cls_features_1L_chr <- setdiff(.x, person_features_chr)
            if (identical(person_features_chr, character(0))) {
                person_features_chr <- NA_character_
            }
            else {
                stringr::str_replace(cls_features_1L_chr, "class", 
                  "Class ")
            }
        }), T ~ Concept), Parameter = dplyr::case_when(Parameter %>% 
            purrr::map_lgl(~{
                concept_1L_chr <- .x
                paste0(".", mdl_params_ls$candidate_predrs_tb$short_name_chr) %>% 
                  purrr::map_lgl(~endsWith(concept_1L_chr, .x)) %>% 
                  any()
            }) ~ Parameter %>% strsplit("[.]") %>% purrr::map_chr(~{
            person_features_chr <- .x[.x %in% mdl_params_ls$candidate_predrs_tb$short_name_chr]
            att_features_chr <- setdiff(.x, person_features_chr)
            att_lvl_nm_1L_chr <- att_short_nms_chr[which(att_features_chr == 
                att_lvls_chr)] %>% ready4::make_list_phrase()
            if (identical(person_features_chr, character(0))) {
                person_features_chr <- NA_character_
            }
            else {
                person_features_chr %>% purrr::map_chr(~paste0(att_lvl_nm_1L_chr, 
                  " x ", ready4::get_from_lup_obj(mdl_params_ls$candidate_predrs_tb, 
                    match_var_nm_1L_chr = "short_name_chr", match_value_xx = .x, 
                    target_var_nm_1L_chr = "long_name_chr"))) %>% 
                  ready4::make_list_phrase()
            }
        }), Parameter %>% purrr::map_lgl(~{
            concept_1L_chr <- .x
            paste0(":", mdl_params_ls$candidate_predrs_tb$short_name_chr) %>% 
                purrr::map2_lgl(paste0(mdl_params_ls$candidate_predrs_tb$short_name_chr, 
                  ":"), ~endsWith(concept_1L_chr, .x) | startsWith(concept_1L_chr, 
                  .y)) %>% any()
        }) ~ Parameter %>% strsplit("[:]") %>% purrr::map_chr(~{
            person_features_chr <- .x[.x %in% mdl_params_ls$candidate_predrs_tb$short_name_chr]
            cls_features_1L_chr <- setdiff(.x, person_features_chr)
            if (identical(person_features_chr, character(0))) {
                person_features_chr <- NA_character_
            }
            else {
                paste0(stringr::str_replace(cls_features_1L_chr, 
                  "class", "Class "), " x ", person_features_chr %>% 
                  purrr::map_chr(~paste0(ready4::get_from_lup_obj(mdl_params_ls$candidate_predrs_tb, 
                    match_var_nm_1L_chr = "short_name_chr", match_value_xx = .x, 
                    target_var_nm_1L_chr = "long_name_chr") %>% 
                    Hmisc::capitalize())) %>% ready4::make_list_phrase())
            }
        }), T ~ Parameter))
    }
    sds_ls <- mdl_smry_tb$Concept %>% strsplit("[.]") %>% purrr::map(~if (length(.x) == 
        2) {
        c(.x[1], .x[2] %>% strsplit("[:]") %>% purrr::flatten_chr())
    }
    else {
        .x
    })
    mdl_smry_tb$Concept <- sds_ls %>% purrr::map_chr(~{
        vector_chr <- .x
        ifelse(length(vector_chr) > 1 & vector_chr[1] %in% c("sd", 
            "chol"), paste0(ifelse(vector_chr[1] == "sd", "Standard Deviations", 
            "Cholesky"), " - ", paste0(2:length(vector_chr) %>% 
            purrr::map_chr(~ifelse(vector_chr[.x] %in% (fctr_combined_ls %>% 
                purrr::flatten_chr()), ready4::get_from_lup_obj(dce_design_ls$choice_sets_ls$att_lvls_tb, 
                match_value_xx = (fctr_separate_ls %>% purrr::flatten_chr())[which(vector_chr[.x] == 
                  (fctr_combined_ls %>% purrr::flatten_chr()))], 
                match_var_nm_1L_chr = "dummy_nm_chr", target_var_nm_1L_chr = "attribute_chr") %>% 
                stringr::str_replace_all("_", " "), vector_chr[.x]) %>% 
                stringr::str_replace_all(records_ls$opt_out_var_nm_1L_chr, 
                  opt_out_nm_1L_chr)), collapse = " x ")), paste0(.x))
    })
    sds_ls <- mdl_smry_tb$Parameter %>% strsplit("[.]") %>% purrr::map(~if (length(.x) == 
        2) {
        c(.x[1], .x[2] %>% strsplit("[:]") %>% purrr::flatten_chr())
    }
    else {
        .x
    })
    mdl_smry_tb$Parameter <- sds_ls %>% purrr::map_chr(~{
        vector_chr <- .x
        ifelse(length(vector_chr) > 1 & vector_chr[1] %in% c("sd", 
            "chol"), paste0(paste0(2:length(vector_chr) %>% purrr::map_chr(~ifelse(vector_chr[.x] %in% 
            (fctr_combined_ls %>% purrr::flatten_chr()), ready4::get_from_lup_obj(dce_design_ls$choice_sets_ls$att_lvls_tb, 
            match_value_xx = (fctr_separate_ls %>% purrr::flatten_chr())[which(vector_chr[.x] == 
                (fctr_combined_ls %>% purrr::flatten_chr()))], 
            match_var_nm_1L_chr = "dummy_nm_chr", target_var_nm_1L_chr = "short_nms_chr") %>% 
            stringr::str_replace_all("_", " "), vector_chr[.x]) %>% 
            stringr::str_replace_all(records_ls$opt_out_var_nm_1L_chr, 
                opt_out_nm_1L_chr)), collapse = " x ")), paste0(.x))
    })
    mdl_smry_tb <- mdl_smry_tb %>% dplyr::mutate(Estimate = dplyr::case_when(sds_ls %>% 
        purrr::map_lgl(~length(.x) == 3 & .x[1] %in% c("sd")) ~ 
        Estimate %>% abs(), T ~ Estimate))
    if (class_lgl %>% any()) {
        mdl_smry_tb <- mdl_smry_tb %>% dplyr::select(Class, dplyr::everything()) %>% 
            dplyr::mutate(Class = Class %>% stringr::str_replace_all("class.", 
                "Class ") %>% stringr::str_sub(end = -2)) %>% 
            dplyr::mutate(Class = Class %>% purrr::map2_chr(Concept, 
                ~ifelse(.x == "" & startsWith(.y, "Class "), 
                  "Class", .x)))
    }
    if (!identical(digits_1L_int, integer(0))) 
        mdl_smry_tb <- mdl_smry_tb %>% dplyr::mutate(dplyr::across(where(is.numeric), 
            ~round(.x, digits = digits_1L_int)))
    mdl_smry_tb <- mdl_smry_tb %>% dplyr::arrange(match(Concept, 
        mdl_smry_tb$Concept %>% unique()))
    return(mdl_smry_tb)
}
#' Transform replication dataset for analysis
#' @description transform_repln_ds_for_analysis() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform replication dataset for analysis. Function argument repln_ds_tb specifies the object to be updated. Argument areas_lup provides the object to be updated. The function returns Transformed replication dataset (a tibble).
#' @param repln_ds_tb Replication dataset (a tibble)
#' @param areas_lup Areas (a lookup table), Default: NULL
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param person_uid_var_nm_1L_chr Person unique identifier variable name (a character vector of length one), Default: 'person_uid'
#' @param seifa_lup Socio-Economic Indices for Areas (a lookup table), Default: NULL
#' @param set_idx_1L_int Set index (an integer vector of length one), Default: integer(0)
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
    set_idx_1L_int = integer(0), write_to_1L_chr = character(0)) 
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
            length(dce_design_ls$choice_cards_ls[[{
                if (identical(set_idx_1L_int, integer(0))) {
                  length(dce_design_ls$choice_cards_ls)
                } else {
                  set_idx_1L_int
                }
            }]]$block_idxs_ls[[1]])) %>% dplyr::ungroup() %>% 
        dplyr::mutate(der_All_Tasks = der_Missing_Tasks == 0)
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
