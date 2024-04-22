#' Add age and area comparisons
#' @description add_age_and_area_cmprsns() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add age and area comparisons. The function returns Proportion comparisons (a list).
#' @param prpn_cmprsns_ls Proportion comparisons (a list), Default: list()
#' @param ds_tb Dataset (a tibble)
#' @param age_group_var_nms_chr Age group variable names (a character vector), Default: c("der_age_u20", "Age range", "AGE")
#' @param age_group_lup Age group (a lookup table), Default: NULL
#' @param area_group_var_nms_chr Area group variable names (a character vector), Default: c("der_STE", "Jurisdiction", "REGION")
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param fl_nm_1L_chr File name (a character vector of length one), Default: 'popl_by_ste.csv'
#' @param is_pc_1L_lgl Is pc (a logical vector of length one), Default: T
#' @param period_1L_chr Period (a character vector of length one), Default: '2019-Q2'
#' @param period_var_nm_1L_chr Period variable name (a character vector of length one), Default: 'TIME_PERIOD'
#' @param recode_ls Recode (a list), Default: list()
#' @param target_var_nm_1L_chr Target variable name (a character vector of length one), Default: 'OBS_VALUE'
#' @param url_1L_chr Url (a character vector of length one), Default: character(0)
#' @param write_to_1L_chr Write to (a character vector of length one), Default: character(0)
#' @return Proportion comparisons (a list)
#' @rdname add_age_and_area_cmprsns
#' @export 
#' @importFrom tibble tibble
#' @importFrom ready4 make_prompt get_from_lup_obj
#' @importFrom utils download.file
#' @importFrom dplyr filter arrange mutate group_by summarise pull
#' @importFrom rlang sym
#' @importFrom purrr map_chr map
#' @importFrom stats setNames
add_age_and_area_cmprsns <- function (prpn_cmprsns_ls = list(), ds_tb, age_group_var_nms_chr = c("der_age_u20", 
    "Age range", "AGE"), age_group_lup = NULL, area_group_var_nms_chr = c("der_STE", 
    "Jurisdiction", "REGION"), consent_1L_chr = "", fl_nm_1L_chr = "popl_by_ste.csv", 
    is_pc_1L_lgl = T, period_1L_chr = "2019-Q2", period_var_nm_1L_chr = "TIME_PERIOD", 
    recode_ls = list(), target_var_nm_1L_chr = "OBS_VALUE", url_1L_chr = character(0), 
    write_to_1L_chr = character(0)) 
{
    if (is_pc_1L_lgl) {
        popl_var_nm_1L_chr <- "Population (per-cent)"
        scaling_1L_dbl <- 100
        sample_var_nm_1L_chr <- "Sample (per-cent)"
    }
    else {
        popl_var_nm_1L_chr <- "Population (proportion)"
        scaling_1L_dbl <- 1
        sample_var_nm_1L_chr <- "Sample (proportion)"
    }
    if (identical(recode_ls, list())) {
        recode_ls <- list(macro_nm_1L_chr = "AUS", meso_nms_chr = c("New South Wales", 
            "Victoria", "Queensland", "South Australia", "Western Australia", 
            "Tasmania", "Northern Territory", "Australian Capital Territory"))
    }
    if (is.null(age_group_lup)) {
        age_group_lup <- tibble::tibble(Var_1_xx = c(T, F, NA), 
            Var_2_xx = c("TRUE", "FALSE", NA_character_), Var_3_xx = c("A15", 
                "A20", "25"), Value_xx = c("15-19", "20-25", 
                "20-25"))
    }
    if (identical(character(0), write_to_1L_chr)) 
        write_to_1L_chr <- tempdir()
    if (identical(character(0), url_1L_chr)) 
        url_1L_chr <- "https://api.data.abs.gov.au/data/ABS,ERP_Q,1.0.0/1.3.25+A20+A15..Q?startPeriod=2019-Q2&dimensionAtObservation=AllDimensions&format=csv"
    path_1L_chr <- paste0(write_to_1L_chr, "/", fl_nm_1L_chr)
    if (!consent_1L_chr %in% c("Y", "N")) {
        consent_1L_chr <- ready4::make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file ", 
            fl_nm_1L_chr, " to ", write_to_1L_chr), options_chr = c("Y", 
            "N"), force_from_opts_1L_chr = T)
    }
    if (consent_1L_chr %in% c("Y")) {
        utils::download.file(url_1L_chr, destfile = path_1L_chr, 
            mode = "wb")
        message(paste0("New file created in ", write_to_1L_chr, 
            " :\n", fl_nm_1L_chr))
    }
    population_tb <- read.csv(path_1L_chr) %>% dplyr::filter(!!rlang::sym(period_var_nm_1L_chr) == 
        period_1L_chr) %>% dplyr::arrange(!!rlang::sym(age_group_var_nms_chr[3]), 
        !!rlang::sym(area_group_var_nms_chr[3])) %>% dplyr::mutate(`:=`(!!rlang::sym(area_group_var_nms_chr[2]), 
        !!rlang::sym(area_group_var_nms_chr[3])))
    if (!is.null(recode_ls)) {
        population_tb <- population_tb %>% dplyr::mutate(`:=`(!!rlang::sym(area_group_var_nms_chr[2]), 
            !!rlang::sym(area_group_var_nms_chr[2]) %>% purrr::map_chr(~ifelse(.x == 
                recode_ls$macro_nm_1L_chr, .x, recode_ls$meso_nms_chr[as.numeric(.x)]))))
    }
    if (!is.null(area_group_var_nms_chr)) {
        grouped_area_tb <- population_tb %>% dplyr::group_by(!!rlang::sym(area_group_var_nms_chr[2])) %>% 
            dplyr::summarise(youth_popl_int = sum(!!rlang::sym(target_var_nm_1L_chr))) %>% 
            dplyr::mutate(national_tot_int = max(youth_popl_int)) %>% 
            dplyr::mutate(`:=`(!!rlang::sym(popl_var_nm_1L_chr), 
                youth_popl_int/national_tot_int * scaling_1L_dbl))
        if (!is.null(recode_ls)) {
            grouped_area_tb <- grouped_area_tb %>% dplyr::filter(!!rlang::sym(area_group_var_nms_chr[2]) != 
                recode_ls$macro_nm_1L_chr)
        }
        area_cmprsn_tb <- make_cmprsn_tbl(ds_tb, popl_var_nm_1L_chr = popl_var_nm_1L_chr, 
            sample_var_nm_1L_chr = sample_var_nm_1L_chr, expected_dbl = grouped_area_tb %>% 
                dplyr::pull(!!rlang::sym(popl_var_nm_1L_chr)) %>% 
                stats::setNames(grouped_area_tb %>% dplyr::pull(!!rlang::sym(area_group_var_nms_chr[2]))), 
            grouping_var_nms_chr = area_group_var_nms_chr, is_pc_1L_lgl = is_pc_1L_lgl)
        prpn_cmprsns_ls$area_cmprsn_tb <- area_cmprsn_tb
    }
    if (!is.null(age_group_var_nms_chr)) {
        grouped_age_tb <- population_tb %>% dplyr::group_by(!!rlang::sym(age_group_var_nms_chr[3]))
        if (!is.null(recode_ls)) {
            grouped_age_tb <- grouped_age_tb %>% dplyr::filter(!!rlang::sym(area_group_var_nms_chr[2]) == 
                recode_ls$macro_nm_1L_chr)
        }
        grouped_age_tb <- grouped_age_tb %>% dplyr::mutate(`:=`(!!rlang::sym(age_group_var_nms_chr[2]), 
            !!rlang::sym(age_group_var_nms_chr[3]) %>% purrr::map(~ready4::get_from_lup_obj(age_group_lup, 
                match_value_xx = .x, match_var_nm_1L_chr = "Var_3_xx", 
                target_var_nm_1L_chr = "Value_xx")) %>% unlist())) %>% 
            dplyr::group_by(!!rlang::sym(age_group_var_nms_chr[2])) %>% 
            dplyr::summarise(youth_popl_int = sum(!!rlang::sym(target_var_nm_1L_chr))) %>% 
            dplyr::mutate(national_tot_int = sum(youth_popl_int)) %>% 
            dplyr::mutate(`:=`(!!rlang::sym(popl_var_nm_1L_chr), 
                youth_popl_int/national_tot_int * scaling_1L_dbl))
        age_cmprsn_tb <- make_cmprsn_tbl(ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(age_group_var_nms_chr[1]), 
            !!rlang::sym(age_group_var_nms_chr[1]) %>% purrr::map(~ready4::get_from_lup_obj(age_group_lup, 
                match_value_xx = .x, match_var_nm_1L_chr = "Var_2_xx", 
                target_var_nm_1L_chr = "Value_xx")) %>% unlist())), 
            popl_var_nm_1L_chr = popl_var_nm_1L_chr, sample_var_nm_1L_chr = sample_var_nm_1L_chr, 
            expected_dbl = grouped_age_tb %>% dplyr::pull(!!rlang::sym(popl_var_nm_1L_chr)) %>% 
                stats::setNames(grouped_age_tb %>% dplyr::pull(!!rlang::sym(age_group_var_nms_chr[2]))), 
            grouping_var_nms_chr = age_group_var_nms_chr, is_pc_1L_lgl = is_pc_1L_lgl)
        prpn_cmprsns_ls$age_cmprsn_tb <- age_cmprsn_tb
    }
    return(prpn_cmprsns_ls)
}
#' Add analysis
#' @description add_analysis() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add analysis. The function returns Analysis (a list).
#' @param analysis_ls Analysis (a list), Default: list()
#' @param mdls_ls Models (a list)
#' @param what_chr What (a character vector), Default: c("wtp")
#' @param dce_design_ls Discrete choice experiment design (a list), Default: NULL
#' @param records_ls Records (a list), Default: NULL
#' @return Analysis (a list)
#' @rdname add_analysis
#' @export 
#' @importFrom stats coef vcov setNames
#' @importFrom msm deltamethod
#' @importFrom purrr map2 map_dbl map_dfr
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select
#' @keywords internal
add_analysis <- function (analysis_ls = list(), mdls_ls, what_chr = c("wtp"), 
    dce_design_ls = NULL, records_ls = NULL) 
{
    if ("wtp" %in% what_chr) {
        what_chr <- c(what_chr, "wtp_gmnl", "wtp_gmnl_cor", "wtp_smnl", 
            "wtp_utl_sp") %>% unique()
    }
    if ("share" %in% what_chr) {
        what_chr <- c(what_chr, "prpns_sample", "prpns_mnl") %>% 
            unique()
    }
    if (("wtp_gmnl" %in% what_chr) & !is.null(mdls_ls$gmnl_wtp_mdl)) {
        analysis_ls$wtp_gmnl_smry_ls <- make_gmnl_mdl_smry(mdls_ls$gmnl_wtp_mdl)
    }
    if (("wtp_gmnl_cor" %in% what_chr) & !is.null(mdls_ls$gmnl_wtp_cor_mdl)) {
        analysis_ls$wtp_gmnl_cor_smry_ls <- make_gmnl_mdl_smry(mdls_ls$gmnl_wtp_cor_mdl)
    }
    if (("wtp_smnl" %in% what_chr) & !is.null(mdls_ls$smnl_mdl)) {
        analysis_ls$wtp_smnl_smry_ls <- make_gmnl_mdl_smry(mdls_ls$smnl_mdl)
        analysis_ls$wtp_smnl_price_num <- c(-exp(stats::coef(mdls_ls$smnl_mdl)["het.(Intercept)"]), 
            {
                msm::deltamethod(~-exp(x6), stats::coef(mdls_ls$smnl_mdl), 
                  stats::vcov(mdls_ls$smnl_mdl), ses = T)
            })
    }
    if (("wtp_utl_sp" %in% what_chr) & !is.null(mdls_ls$mnl_mdl)) {
        analysis_ls$wtp_utl_sp_mat <- make_wtp_mat(mdls_ls$mnl_mdl, 
            cost_var_nm_1L_chr = records_ls$cost_var_nm_1L_chr)
    }
    if ("prpns_sample" %in% what_chr) {
        analysis_ls$prpns_obsd_dbl <- ((records_ls$ds_dfidx$option_id[records_ls$ds_dfidx$choice == 
            TRUE] %>% table())/(nrow(records_ls$ds_dfidx)/length(dce_design_ls$choice_sets_ls$alternatives_chr)) %>% 
            sum()) %>% stats::setNames(dce_design_ls$choice_sets_ls$alternatives_chr)
    }
    if ("prpns_mnl" %in% what_chr) {
        append_ls <- mdls_ls[c("mnl_mdl", "mnl_mlogit_mdl", "mixl_mdl", 
            "mixl_mlogit_mdl")] %>% purrr::map2(c("mnl_mdl", 
            "mnl_mlogit_mdl", "mixl_mdl", "mixl_mlogit_mdl"), 
            ~{
                if (.y == "mnl_mdl") {
                  probs_mat <- fitted(.x, type = "probabilities", 
                    outcome = {
                      if (.y == "mnl_mdl") {
                        F
                      }
                      else {
                        NULL
                      }
                    })
                  predd_dbl <- (purrr::map_dbl(1:length(dce_design_ls$choice_sets_ls$alternatives_chr), 
                    ~sum(probs_mat[, .x]))/nrow(probs_mat))
                }
                else {
                  if (.y == "mnl_mlogit_mdl") {
                    predd_dbl <- predict(.x)
                  }
                  else {
                    if (.y == "mixl_mdl") {
                      probs_mat <- fitted(.x, type = "probabilities", 
                        outcome = {
                          if (.y == "mixl_mdl") {
                            F
                          }
                          else {
                            NULL
                          }
                        })
                    }
                    else {
                      probs_mat <- predict(.x, newdata = records_ls$ds_dfidx)
                    }
                  }
                }
                if (.y %in% c("mnl_mdl", "mnl_mlogit_mdl")) {
                  predd_dbl %>% stats::setNames(dce_design_ls$choice_sets_ls$alternatives_chr)
                }
                else {
                  predd_tb <- purrr::map_dfr(1:length(dce_design_ls$choice_sets_ls$alternatives_chr), 
                    ~tibble::tibble(Mean = mean(probs_mat[, .x]), 
                      SD = sd(probs_mat[, .x])))
                  predd_tb %>% dplyr::mutate(Alternative = dce_design_ls$choice_sets_ls$alternatives_chr) %>% 
                    dplyr::select(Alternative, Mean, SD)
                }
            }) %>% stats::setNames(c("prpns_mnl_prdn_dbl", "prpns_mnl_mlogit_prdn_dbl", 
            "prpns_mixl_prdn_tb", "prpns_mixl_mlogit_prdn_tb"))
        analysis_ls$prpns_mnl_prdn_dbl <- analysis_ls$prpns_mnl_mlogit_prdn_dbl <- NULL
        analysis_ls <- append(analysis_ls, append_ls)
        probs_mat <- fitted(mdls_ls$mnl_mlogit_mdl, type = "probabilities")
    }
    return(analysis_ls)
}
#' Add attempt duration red flag
#' @description add_attempt_dur_red_flag() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add attempt duration red flag. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param attempt_dur_min_1L_dbl Attempt duration minimum (a double vector of length one)
#' @param attempt_dur_var_nm_1L_chr Attempt duration variable name (a character vector of length one)
#' @return Data (a tibble)
#' @rdname add_attempt_dur_red_flag
#' @export 
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @importFrom purrr map_lgl
#' @keywords internal
add_attempt_dur_red_flag <- function (data_tb, attempt_dur_min_1L_dbl, attempt_dur_var_nm_1L_chr) 
{
    data_tb <- data_tb %>% dplyr::mutate(red_flag_attempt_dur_lgl = !!rlang::sym(attempt_dur_var_nm_1L_chr) %>% 
        purrr::map_lgl(~ifelse(as.numeric(.x) < attempt_dur_min_1L_dbl, 
            T, F)))
    return(data_tb)
}
#' Add choice models
#' @description add_choice_mdls() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add choice models. The function returns Models (a list).
#' @param mdls_ls Models (a list), Default: list()
#' @param dce_design_ls Discrete choice experiment design (a list)
#' @param mdl_params_ls Model parameters (a list)
#' @param records_ls Records (a list)
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @param indl_predrs_chr Individual predictors (a character vector), Default: 'NA'
#' @param include_int Include (an integer vector), Default: 1:4
#' @param max_concepts_1L_int Maximum concepts (an integer vector of length one), Default: 2
#' @param min_threshold_1L_int Minimum threshold (an integer vector of length one), Default: 2
#' @param nbr_of_clss_1L_int Number of classes (an integer vector of length one), Default: 2
#' @param purpose_chr Purpose (a character vector), Default: 'attributes'
#' @param significant_at_1L_dbl Significant at (a double vector of length one), Default: 0.05
#' @param ... Additional arguments
#' @return Models (a list)
#' @rdname add_choice_mdls
#' @export 
#' @keywords internal
add_choice_mdls <- function (mdls_ls = list(), dce_design_ls, mdl_params_ls, records_ls, 
    exclude_chr = character(0), indl_predrs_chr = NA_character_, 
    include_int = 1:4, max_concepts_1L_int = 2L, min_threshold_1L_int = 2L, 
    nbr_of_clss_1L_int = 2L, purpose_chr = "attributes", significant_at_1L_dbl = 0.05, 
    ...) 
{
    if ("attributes" %in% purpose_chr) {
        if (1 %in% include_int) 
            mdls_ls$mnl_mdl <- fit_choice_mdl(dce_design_ls, 
                mdl_params_ls = mdl_params_ls, records_ls = records_ls, 
                return_1L_chr = "mnl", ...)
        if (2 %in% include_int) 
            mdls_ls$mnl_mlogit_mdl <- fit_choice_mdl(dce_design_ls, 
                mdl_params_ls = mdl_params_ls, records_ls = records_ls, 
                use_mlogit_pkg_1L_lgl = T, return_1L_chr = "mnl", 
                ...)
    }
    if ("heterogeneity" %in% purpose_chr) {
        if (1 %in% include_int) 
            mdls_ls$mixl_mdl <- fit_choice_mdl(dce_design_ls, 
                correlation_1L_lgl = T, mdl_params_ls = mdl_params_ls, 
                records_ls = records_ls, return_1L_chr = "mixl", 
                ...)
        if (2 %in% include_int) 
            mdls_ls$mixl_mlogit_mdl <- fit_choice_mdl(dce_design_ls, 
                correlation_1L_lgl = T, mdl_params_ls = mdl_params_ls, 
                records_ls = records_ls, use_mlogit_pkg_1L_lgl = T, 
                return_1L_chr = "mixl", ...)
        if (3 %in% include_int) 
            mdls_ls$gmnl_mdl <- fit_choice_mdl(dce_design_ls, 
                correlation_1L_lgl = T, mdl_params_ls = mdl_params_ls, 
                records_ls = records_ls, return_1L_chr = "gmnl", 
                ...)
    }
    if ("interactions" %in% purpose_chr) {
        if (1 %in% include_int) 
            mdls_ls$att_predn_mdls_ls <- make_att_predn_mdls_ls(dce_design_ls, 
                mdl_params_ls = mdl_params_ls, records_ls = records_ls, 
                significant_at_1L_dbl = significant_at_1L_dbl)
        if (2 %in% include_int) 
            mdls_ls$mixl_mdl_ls <- make_mxd_lgt_mdl_ls(att_predn_mdls_ls = mdls_ls$att_predn_mdls_ls, 
                dce_design_ls = dce_design_ls, mdl_params_ls = mdl_params_ls, 
                records_ls = records_ls, exclude_chr = exclude_chr, 
                max_concepts_1L_int = max_concepts_1L_int, min_threshold_1L_int = min_threshold_1L_int)
    }
    if ("classes" %in% purpose_chr) {
        if (1 %in% include_int) 
            mdls_ls$lc_mdl <- fit_choice_mdl(dce_design_ls, mdl_params_ls = mdl_params_ls, 
                records_ls = records_ls, return_1L_chr = "lc", 
                indl_predrs_chr = character(0), nbr_of_clss_1L_int = nbr_of_clss_1L_int, 
                ...)
        if (2 %in% include_int) 
            mdls_ls$lc_mxd_mdl <- fit_choice_mdl(dce_design_ls, 
                mdl_params_ls = mdl_params_ls, records_ls = records_ls, 
                return_1L_chr = "lc", indl_predrs_chr = indl_predrs_chr, 
                nbr_of_clss_1L_int = nbr_of_clss_1L_int, ...)
        if (3 %in% include_int) 
            mdls_ls$mm_mdl <- fit_choice_mdl(dce_design_ls, mdl_params_ls = mdl_params_ls, 
                records_ls = records_ls, return_1L_chr = "mm", 
                indl_predrs_chr = indl_predrs_chr, nbr_of_clss_1L_int = nbr_of_clss_1L_int, 
                ...)
        if (4 %in% include_int) 
            mdls_ls$mm_cor_mdl <- fit_choice_mdl(dce_design_ls, 
                mdl_params_ls = mdl_params_ls, records_ls = records_ls, 
                return_1L_chr = "mm", correlation_1L_lgl = T, 
                indl_predrs_chr = indl_predrs_chr, nbr_of_clss_1L_int = nbr_of_clss_1L_int, 
                ...)
    }
    if ("wtp" %in% purpose_chr) {
        if (1 %in% include_int) 
            mdls_ls$smnl_mdl <- fit_choice_mdl(dce_design_ls, 
                mdl_params_ls = mdl_params_ls, records_ls = records_ls, 
                return_1L_chr = "smnl", indl_predrs_chr = character(0), 
                ...)
        if (2 %in% include_int) 
            mdls_ls$gmnl_wtp_mdl <- fit_choice_mdl(dce_design_ls, 
                mdl_params_ls = mdl_params_ls, records_ls = records_ls, 
                return_1L_chr = "gmnl", indl_predrs_chr = character(0), 
                smnl_mdl = mdls_ls$smnl_mdl, ...)
        if (3 %in% include_int) 
            mdls_ls$gmnl_wtp_cor_mdl <- fit_choice_mdl(dce_design_ls, 
                mdl_params_ls = mdl_params_ls, records_ls = records_ls, 
                return_1L_chr = "gmnl", indl_predrs_chr = character(0), 
                smnl_mdl = mdls_ls$smnl_mdl, correlation_1L_lgl = T, 
                ...)
    }
    return(mdls_ls)
}
#' Add cost comparison
#' @description add_cost_comparison() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add cost comparison. The function returns Analysis (a list).
#' @param analysis_ls Analysis (a list)
#' @param choices_ls Choices (a list)
#' @param dce_design_ls Discrete choice experiment design (a list)
#' @param mdls_ls Models (a list)
#' @param mdl_params_ls Model parameters (a list)
#' @param records_ls Records (a list)
#' @param altv_nms_chr Alternative names (a character vector), Default: character(0)
#' @param altv_to_modify_1L_int Alternative to modify (an integer vector of length one), Default: 1
#' @param cost_range_dbl Cost range (a double vector), Default: 0:50
#' @param set_idx_1L_int Set index (an integer vector of length one), Default: integer(0)
#' @param with_chr With (a character vector), Default: 'mnl_mlogit_mdl'
#' @return Analysis (a list)
#' @rdname add_cost_comparison
#' @export 
#' @importFrom purrr map map_dfr
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select everything
#' @importFrom rlang sym
#' @importFrom stats setNames
#' @keywords internal
add_cost_comparison <- function (analysis_ls, choices_ls, dce_design_ls, mdls_ls, mdl_params_ls, 
    records_ls, altv_nms_chr = character(0), altv_to_modify_1L_int = 1, 
    cost_range_dbl = 0:50, set_idx_1L_int = integer(0), with_chr = "mnl_mlogit_mdl") 
{
    analysis_ls$new_data_ls$cost_comparison_ls$ensemble_ls <- purrr::map(cost_range_dbl, 
        ~{
            new_choices_ls <- choices_ls
            new_choices_ls[[altv_to_modify_1L_int]][which(names(new_choices_ls[[altv_to_modify_1L_int]]) == 
                records_ls$cost_var_nm_1L_chr)] <- .x
            new_choices_ls
        })
    analysis_ls$new_data_ls$cost_comparison_ls$predd_shares_ls <- with_chr %>% 
        purrr::map(~{
            with_1L_chr <- .x
            analysis_ls$new_data_ls$cost_comparison_ls$ensemble_ls %>% 
                purrr::map_dfr(~predict_mkt_share(dce_design_ls = dce_design_ls, 
                  mdls_ls = mdls_ls, mdl_params_ls, new_choices_ls = .x, 
                  records_ls = records_ls, altv_nms_chr = altv_nms_chr, 
                  set_idx_1L_int = set_idx_1L_int, with_1L_chr = with_1L_chr) %>% 
                  as.data.frame() %>% t() %>% tibble::as_tibble()) %>% 
                dplyr::mutate(`:=`(!!rlang::sym(records_ls$cost_var_nm_1L_chr), 
                  cost_range_dbl)) %>% dplyr::select(!!rlang::sym(records_ls$cost_var_nm_1L_chr), 
                dplyr::everything())
        }) %>% stats::setNames(paste0(with_chr, "_predd_tb"))
    return(analysis_ls)
}
#' Add country red flag
#' @description add_country_red_flag() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add country red flag. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param lat_lng_var_nms_chr Lat longitude variable names (a character vector)
#' @param valid_countries_chr Valid countries (a character vector)
#' @return Data (a tibble)
#' @rdname add_country_red_flag
#' @export 
#' @importFrom dplyr mutate_at filter select full_join mutate
#' @importFrom sf sf_use_s2 st_as_sf st_intersection `st_crs<-` st_crs
#' @importFrom rlang sym
#' @importFrom rnaturalearth ne_countries
#' @importFrom purrr map_lgl
#' @keywords internal
add_country_red_flag <- function (data_tb, lat_lng_var_nms_chr, valid_countries_chr) 
{
    fn_attribution_1L_chr <- "Function partially based on: https://gis.stackexchange.com/questions/297691/assign-country-names-to-long-and-lat-in-r"
    data_tb <- data_tb %>% dplyr::mutate_at(.vars = lat_lng_var_nms_chr, 
        .funs = as.numeric)
    sf::sf_use_s2(FALSE)
    back_tick_names <- names(data_tb)[!names(data_tb) == names(data_tb) %>% 
        make.names()]
    sfRegion <- sf::st_as_sf(data_tb %>% dplyr::filter(!is.na(!!rlang::sym(lat_lng_var_nms_chr[1]))) %>% 
        dplyr::filter(!is.na(!!rlang::sym(lat_lng_var_nms_chr[2]))), 
        coords = lat_lng_var_nms_chr)
    sfCountry <- rnaturalearth::ne_countries(returnclass = "sf")
    sfIntersection <- sf::st_intersection(sfCountry, sf::`st_crs<-`(sfRegion, 
        sf::st_crs(sfCountry))) %>% dplyr::select(admin, names(data_tb)[!names(data_tb) %in% 
        c(lat_lng_var_nms_chr, back_tick_names)])
    data_tb <- dplyr::full_join(data_tb %>% dplyr::select(names(data_tb)[!names(data_tb) %in% 
        lat_lng_var_nms_chr]), sfIntersection)
    data_tb <- data_tb %>% dplyr::mutate(red_flag_country_lgl = purrr::map_lgl(admin, 
        ~ifelse(.x %in% valid_countries_chr, F, T)))
    return(data_tb)
}
#' Add cut points comparison
#' @description add_cut_pnts_cmprsn() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add cut points comparison. The function returns Proportion comparisons (a list).
#' @param prpn_cmprsns_ls Proportion comparisons (a list), Default: list()
#' @param cmprsn_nm_1L_chr Comparison name (a character vector of length one)
#' @param ds_tb Dataset (a tibble)
#' @param grouping_var_nms_chr Grouping variable names (a character vector)
#' @param expected_dbl Expected (a double vector), Default: numeric(0)
#' @param is_pc_1L_lgl Is pc (a logical vector of length one), Default: T
#' @return Proportion comparisons (a list)
#' @rdname add_cut_pnts_cmprsn
#' @export 
#' @keywords internal
add_cut_pnts_cmprsn <- function (prpn_cmprsns_ls = list(), cmprsn_nm_1L_chr, ds_tb, 
    grouping_var_nms_chr, expected_dbl = numeric(0), is_pc_1L_lgl = T) 
{
    prpn_cmprsns_ls[[cmprsn_nm_1L_chr]] <- make_cut_pnts_cmprsn(ds_tb = ds_tb, 
        grouping_var_nms_chr = grouping_var_nms_chr, expected_dbl = expected_dbl, 
        is_pc_1L_lgl = is_pc_1L_lgl)
    return(prpn_cmprsns_ls)
}
#' Add design specification
#' @description add_design_spec() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add design specification. The function returns Discrete choice experiment design (a list).
#' @param dce_design_ls Discrete choice experiment design (a list), Default: list()
#' @param add_choice_cards_1L_lgl Add choice cards (a logical vector of length one), Default: F
#' @param add_cndt_design_mat Add candidate design (a matrix), Default: F
#' @param alternatives_chr Alternatives (a character vector), Default: character(0)
#' @param att_lvls_tb Attribute levels (a tibble), Default: tibble::tibble()
#' @param block_indcs_ls Block indices (a list), Default: list()
#' @param choice_var_pfx_1L_chr Choice variable prefix (a character vector of length one), Default: 'DCE_B'
#' @param constraints_ls Constraints (a list), Default: list()
#' @param cost_att_idx_1L_int Cost attribute index (an integer vector of length one), Default: integer(0)
#' @param cost_pfx_1L_chr Cost prefix (a character vector of length one), Default: ''
#' @param cost_sfx_1L_chr Cost suffix (a character vector of length one), Default: ''
#' @param draws_1L_int Draws (an integer vector of length one), Default: 10
#' @param nbr_of_blocks_1L_int Number of blocks (an integer vector of length one), Default: integer(0)
#' @param nbr_of_sets_1L_int Number of sets (an integer vector of length one), Default: integer(0)
#' @param opt_out_idx_1L_int Opt out index (an integer vector of length one), Default: integer(0)
#' @param parallel_1L_lgl Parallel (a logical vector of length one), Default: FALSE
#' @param pilot_ds_tb Pilot dataset (a tibble), Default: NULL
#' @param priors_dbl Priors (a double vector), Default: numeric(0)
#' @param priors_idx_1L_int Priors index (an integer vector of length one), Default: integer(0)
#' @param seed_1L_int Seed (an integer vector of length one), Default: 1987
#' @param session_ls Session (a list), Default: list()
#' @param set_idx_1L_int Set index (an integer vector of length one), Default: integer(0)
#' @param transform_att_nms_1L_lgl Transform attribute names (a logical vector of length one), Default: T
#' @return Discrete choice experiment design (a list)
#' @rdname add_design_spec
#' @export 
#' @importFrom tibble tibble
#' @importFrom idefix Profiles
#' @importFrom purrr map
#' @importFrom stats setNames
#' @keywords internal
add_design_spec <- function (dce_design_ls = list(), add_choice_cards_1L_lgl = F, 
    add_cndt_design_mat = F, alternatives_chr = character(0), 
    att_lvls_tb = tibble::tibble(), block_indcs_ls = list(), 
    choice_var_pfx_1L_chr = "DCE_B", constraints_ls = list(), 
    cost_att_idx_1L_int = integer(0), cost_pfx_1L_chr = "", cost_sfx_1L_chr = "", 
    draws_1L_int = 10L, nbr_of_blocks_1L_int = integer(0), nbr_of_sets_1L_int = integer(0), 
    opt_out_idx_1L_int = integer(0), parallel_1L_lgl = FALSE, 
    pilot_ds_tb = NULL, priors_dbl = numeric(0), priors_idx_1L_int = integer(0), 
    seed_1L_int = 1987, session_ls = list(), set_idx_1L_int = integer(0), 
    transform_att_nms_1L_lgl = T) 
{
    if (is.null(dce_design_ls$choice_cards_ls)) {
        dce_design_ls$choice_cards_ls <- list()
    }
    if (is.null(dce_design_ls$choice_sets_ls)) {
        dce_design_ls$choice_sets_ls <- list(alternatives_chr = alternatives_chr, 
            att_lvls_tb = att_lvls_tb, nbr_of_blocks_1L_int = nbr_of_blocks_1L_int, 
            nbr_of_sets_1L_int = nbr_of_sets_1L_int, opt_out_1L_lgl = logical(0), 
            opt_out_idx_1L_int = integer(0))
    }
    if (!identical(alternatives_chr, character(0))) {
        dce_design_ls$choice_sets_ls$alternatives_chr <- alternatives_chr
    }
    if (!identical(att_lvls_tb, tibble::tibble())) {
        dce_design_ls$choice_sets_ls$att_lvls_tb <- att_lvls_tb
    }
    if (!identical(nbr_of_sets_1L_int, integer(0))) {
        dce_design_ls$choice_sets_ls$nbr_of_sets_1L_int <- nbr_of_sets_1L_int
    }
    if (!identical(nbr_of_blocks_1L_int, integer(0))) {
        dce_design_ls$choice_sets_ls$nbr_of_blocks_1L_int <- nbr_of_blocks_1L_int
    }
    if (!identical(opt_out_idx_1L_int, integer(0))) {
        dce_design_ls$choice_sets_ls$opt_out_idx_1L_int <- opt_out_idx_1L_int
    }
    if (!identical(opt_out_idx_1L_int, integer(0))) {
        dce_design_ls$choice_sets_ls$opt_out_1L_lgl <- ifelse(is.na(dce_design_ls$choice_sets_ls$opt_out_idx_1L_int), 
            F, T)
    }
    if (add_cndt_design_mat) {
        dce_design_ls$cndt_design_mat <- idefix::Profiles(lvls = get_att_smrys(dce_design_ls), 
            coding = get_att_smrys(dce_design_ls, return_1L_chr = "type"), 
            c.lvls = get_lvls(dce_design_ls$choice_sets_ls$att_lvls_tb, 
                return_1L_chr = "cont") %>% unname() %>% purrr::map(~as.numeric(.x)))
    }
    else {
        if (is.null(dce_design_ls$cndt_design_mat)) {
            dce_design_ls$cndt_design_mat <- matrix(numeric(0))
        }
    }
    if (is.null(dce_design_ls$cost_att_idx_1L_int) | !identical(cost_att_idx_1L_int, 
        integer(0))) {
        dce_design_ls$cost_att_idx_1L_int <- cost_att_idx_1L_int
    }
    if (is.null(dce_design_ls$cost_pfx_1L_chr) | !identical(cost_pfx_1L_chr, 
        "")) {
        dce_design_ls$cost_pfx_1L_chr <- cost_pfx_1L_chr
    }
    if (is.null(dce_design_ls$cost_sfx_1L_chr) | !identical(cost_sfx_1L_chr, 
        "")) {
        dce_design_ls$cost_sfx_1L_chr <- cost_sfx_1L_chr
    }
    if (is.null(dce_design_ls$efnt_dsn_ls)) {
        dce_design_ls$efnt_dsn_ls <- list()
    }
    if (is.null(dce_design_ls$pilot_analysis_ls)) {
        dce_design_ls$pilot_analysis_ls <- list()
    }
    if (is.null(dce_design_ls$priors_ls)) {
        dce_design_ls$priors_ls <- list()
    }
    if (is.null(dce_design_ls$session_ls)) {
        dce_design_ls$session_ls$Set_1 <- {
            if (identical(session_ls, list())) {
                sessionInfo()
            }
            else {
                session_ls
            }
        }
    }
    else {
        if (!identical(session_ls, list())) {
            append(dce_design_ls$session_ls, list(session_ls) %>% 
                stats::setNames(paste0("Set_", length(dce_design_ls$session_ls) + 
                  1)))
        }
    }
    if (!identical(priors_dbl, numeric(0))) {
        dce_design_ls$priors_ls <- append(dce_design_ls$priors_ls, 
            list(make_priors_ls(dce_design_ls, priors_dbl = priors_dbl, 
                draws_1L_int = draws_1L_int, seed_1L_int = seed_1L_int)) %>% 
                stats::setNames(paste0("Set_", length(dce_design_ls$priors_ls) + 
                  1)))
    }
    if (!identical(priors_idx_1L_int, integer(0))) {
        if (identical(dce_design_ls$pilot_analysis_ls, list())) {
            start_dsn_mat_ls <- NULL
        }
        else {
            start_dsn_mat_ls <- list(dce_design_ls$efnt_dsn_ls[[set_idx_1L_int]]$design)
        }
        dce_design_ls$efnt_dsn_ls <- append(dce_design_ls$efnt_dsn_ls, 
            list(make_efnt_dsn_mat(dce_design_ls, parallel_1L_lgl = parallel_1L_lgl, 
                pilot_analysis_ls = {
                  if (identical(dce_design_ls$pilot_analysis_ls, 
                    list())) {
                    NULL
                  } else {
                    dce_design_ls$pilot_analysis_ls
                  }
                }, priors_idx_1L_int = priors_idx_1L_int, set_idx_1L_int = set_idx_1L_int, 
                start_dsn_mat_ls = start_dsn_mat_ls)) %>% stats::setNames(paste0("Set_", 
                length(dce_design_ls$efnt_dsn_ls) + 1)))
    }
    if (add_choice_cards_1L_lgl) {
        dce_design_ls$choice_cards_ls <- append(dce_design_ls$choice_cards_ls, 
            list(make_choice_cards(dce_design_ls, block_indcs_ls = block_indcs_ls, 
                seed_1L_int = seed_1L_int, set_idx_1L_int = {
                  if (!identical(set_idx_1L_int, integer(0))) {
                    set_idx_1L_int
                  } else {
                    length(dce_design_ls$efnt_dsn_ls)
                  }
                }, transform_att_nms_1L_lgl = transform_att_nms_1L_lgl)) %>% 
                stats::setNames(paste0("Set_", length(dce_design_ls$choice_cards_ls) + 
                  1)))
    }
    if (!is.null(pilot_ds_tb)) {
        dce_design_ls$pilot_analysis_ls <- append(dce_design_ls$pilot_analysis_ls, 
            list(make_pilot_analysis_ls(pilot_ds_tb, dce_design_ls = dce_design_ls, 
                constraints_ls = constraints_ls, choice_var_pfx_1L_chr = choice_var_pfx_1L_chr, 
                draws_1L_int = draws_1L_int, seed_1L_int = seed_1L_int, 
                set_idx_1L_int = set_idx_1L_int)) %>% stats::setNames(paste0("Set_", 
                length(dce_design_ls$pilot_analysis_ls) + 1)))
    }
    return(dce_design_ls)
}
#' Add duplicate text red flag
#' @description add_duplicate_text_red_flag() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add duplicate text red flag. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param unique_by_case_var_nms_chr Unique by case variable names (a character vector)
#' @return Data (a tibble)
#' @rdname add_duplicate_text_red_flag
#' @export 
#' @importFrom purrr reduce
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
add_duplicate_text_red_flag <- function (data_tb, unique_by_case_var_nms_chr) 
{
    data_tb <- purrr::reduce(1:length(unique_by_case_var_nms_chr), 
        .init = data_tb, ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("red_flag_duplicate_txt_var", 
            .y, "_lgl")), duplicated(!!rlang::sym(unique_by_case_var_nms_chr[.y])))))
    return(data_tb)
}
#' Add email red flag
#' @description add_email_red_flag() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add email red flag. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param email_var_nm_1L_chr Email variable name (a character vector of length one)
#' @param email_max_cnstv_digits_1L_int Email maximum consecutive digits (an integer vector of length one)
#' @return Data (a tibble)
#' @rdname add_email_red_flag
#' @export 
#' @importFrom dplyr mutate
#' @importFrom purrr map map_lgl
#' @importFrom rlang sym
#' @keywords internal
add_email_red_flag <- function (data_tb, email_var_nm_1L_chr, email_max_cnstv_digits_1L_int) 
{
    email_max_cnstv_digits_1L_int <- as.integer(email_max_cnstv_digits_1L_int)
    data_tb <- data_tb %>% dplyr::mutate(red_flag_email_lgl = purrr::map(!!rlang::sym(email_var_nm_1L_chr), 
        ~get_digits_from_text(.x)) %>% purrr::map(~nchar(.x) %>% 
        max()) %>% purrr::map_lgl(~ifelse(.x >= email_max_cnstv_digits_1L_int, 
        T, F)))
    return(data_tb)
}
#' Add flags
#' @description add_flags() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add flags. The function returns Records (a list).
#' @param records_ls Records (a list)
#' @param age_var_nm_1L_chr Age variable name (a character vector of length one), Default: character(0)
#' @param attempt_dur_min_1L_dbl Attempt duration minimum (a double vector of length one), Default: numeric(0)
#' @param attempt_dur_var_nm_1L_chr Attempt duration variable name (a character vector of length one), Default: character(0)
#' @param date_stamp_format_1L_chr Date stamp format (a character vector of length one), Default: 'DMY'
#' @param date_stamp_var_nm_1L_chr Date stamp variable name (a character vector of length one), Default: character(0)
#' @param dob_format_1L_chr Date of birth format (a character vector of length one), Default: 'DMY'
#' @param dob_var_nm_1L_chr Date of birth variable name (a character vector of length one), Default: character(0)
#' @param email_max_cnstv_digits_1L_int Email maximum consecutive digits (an integer vector of length one), Default: integer(0)
#' @param email_var_nm_1L_chr Email variable name (a character vector of length one), Default: character(0)
#' @param flags_max_1L_int Flags maximum (an integer vector of length one), Default: 0
#' @param flags_tot_var_nm_1L_chr Flags total variable name (a character vector of length one), Default: 'red_flag_count_int'
#' @param lat_lng_var_nms_chr Lat longitude variable names (a character vector), Default: character(0)
#' @param qltv_ax_green_flag_1L_chr Qualitative assessment green flag (a character vector of length one), Default: 'Green'
#' @param qltv_ax_red_flag_1L_chr Qualitative assessment red flag (a character vector of length one), Default: 'Red'
#' @param qltv_ax_tb Qualitative assessment (a tibble), Default: NULL
#' @param qltv_ax_var_nm_1L_chr Qualitative assessment variable name (a character vector of length one), Default: character(0)
#' @param unique_by_case_var_nms_chr Unique by case variable names (a character vector), Default: character(0)
#' @param valid_countries_chr Valid countries (a character vector), Default: character(0)
#' @return Records (a list)
#' @rdname add_flags
#' @export 
#' @keywords internal
add_flags <- function (records_ls, age_var_nm_1L_chr = character(0), attempt_dur_min_1L_dbl = numeric(0), 
    attempt_dur_var_nm_1L_chr = character(0), date_stamp_format_1L_chr = "DMY", 
    date_stamp_var_nm_1L_chr = character(0), dob_format_1L_chr = "DMY", 
    dob_var_nm_1L_chr = character(0), email_max_cnstv_digits_1L_int = integer(0), 
    email_var_nm_1L_chr = character(0), flags_max_1L_int = 0, 
    flags_tot_var_nm_1L_chr = "red_flag_count_int", lat_lng_var_nms_chr = character(0), 
    qltv_ax_green_flag_1L_chr = "Green", qltv_ax_red_flag_1L_chr = "Red", 
    qltv_ax_tb = NULL, qltv_ax_var_nm_1L_chr = character(0), 
    unique_by_case_var_nms_chr = character(0), valid_countries_chr = character(0)) 
{
    records_ls$flags_ls <- list(age_var_nm_1L_chr = age_var_nm_1L_chr, 
        date_stamp_format_1L_chr = date_stamp_format_1L_chr, 
        date_stamp_var_nm_1L_chr = date_stamp_var_nm_1L_chr, 
        dob_format_1L_chr = dob_format_1L_chr, dob_var_nm_1L_chr = dob_var_nm_1L_chr, 
        attempt_dur_min_1L_dbl = attempt_dur_min_1L_dbl, attempt_dur_var_nm_1L_chr = attempt_dur_var_nm_1L_chr, 
        email_max_cnstv_digits_1L_int = email_max_cnstv_digits_1L_int, 
        email_var_nm_1L_chr = email_var_nm_1L_chr, lat_lng_var_nms_chr = lat_lng_var_nms_chr, 
        valid_countries_chr = valid_countries_chr, unique_by_case_var_nms_chr = unique_by_case_var_nms_chr, 
        flags_max_1L_int = flags_max_1L_int, flags_tot_var_nm_1L_chr = flags_tot_var_nm_1L_chr, 
        qltv_ax_green_flag_1L_chr = if (!is.null(qltv_ax_tb)) {
            qltv_ax_green_flag_1L_chr
        } else {
            character(0)
        }, qltv_ax_red_flag_1L_chr = if (!is.null(qltv_ax_tb)) {
            qltv_ax_red_flag_1L_chr
        } else {
            character(0)
        }, qltv_ax_tb = qltv_ax_tb, qltv_ax_var_nm_1L_chr = qltv_ax_var_nm_1L_chr)
    return(records_ls)
}
#' Add new choice comparison
#' @description add_new_choice_cmprsn() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add new choice comparison. The function returns Analysis (a list).
#' @param analysis_ls Analysis (a list)
#' @param dce_design_ls Discrete choice experiment design (a list)
#' @param mdl_params_ls Model parameters (a list)
#' @param new_choices_ls New choices (a list)
#' @param records_ls Records (a list)
#' @param altv_nms_chr Alternative names (a character vector), Default: character(0)
#' @param set_idx_1L_int Set index (an integer vector of length one), Default: integer(0)
#' @param with_chr With (a character vector), Default: 'mnl_mlogit_mdl'
#' @return Analysis (a list)
#' @rdname add_new_choice_cmprsn
#' @export 
#' @importFrom purrr map_dfc
#' @importFrom tibble tibble
#' @importFrom rlang sym
#' @importFrom dplyr mutate select everything
#' @keywords internal
add_new_choice_cmprsn <- function (analysis_ls, dce_design_ls, mdl_params_ls, new_choices_ls, 
    records_ls, altv_nms_chr = character(0), set_idx_1L_int = integer(0), 
    with_chr = "mnl_mlogit_mdl") 
{
    comparison_idx_1L_int <- ifelse(!"new_data_ls" %in% names(analysis_ls), 
        1, length(names(analysis_ls$new_data_ls)[startsWith(names(analysis_ls$new_data_ls), 
            "comparison_")]) + 1)
    comparison_nm_1L_chr <- paste0("comparison_", comparison_idx_1L_int, 
        "_ls")
    if (identical(altv_nms_chr, character(0))) {
        altv_nms_chr <- dce_design_ls$choice_sets_ls$alternatives_chr
    }
    analysis_ls$new_data_ls[[comparison_nm_1L_chr]]$new_choices_ls <- new_choices_ls
    analysis_ls$new_data_ls[[comparison_nm_1L_chr]]$predd_share_tb <- with_chr %>% 
        purrr::map_dfc(~{
            with_1L_chr <- .x
            tibble::tibble(`:=`(!!rlang::sym(paste0(with_1L_chr, 
                "_prediction")), predict_mkt_share(dce_design_ls = dce_design_ls, 
                mdls_ls = mdls_ls, mdl_params_ls, new_choices_ls = new_choices_ls, 
                records_ls = records_ls, altv_nms_chr = altv_nms_chr, 
                set_idx_1L_int = set_idx_1L_int, with_1L_chr = with_1L_chr)))
        }) %>% dplyr::mutate(Alternative = altv_nms_chr) %>% 
        dplyr::select(Alternative, dplyr::everything())
    return(analysis_ls)
}
#' Add proportion comparisons
#' @description add_prpn_cmprsns() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add proportion comparisons. The function returns Comparision (a tibble).
#' @param comparison_tb Comparison (a tibble)
#' @param ds_tb Dataset (a tibble)
#' @param grouping_var_nms_chr Grouping variable names (a character vector)
#' @param is_pc_1L_lgl Is pc (a logical vector of length one), Default: T
#' @param popl_var_nm_1L_chr Population variable name (a character vector of length one), Default: 'Population (per-cent)'
#' @param tmp_var_nm_1L_chr Temporary variable name (a character vector of length one), Default: 'cmprsn_test_ls'
#' @return Comparision (a tibble)
#' @rdname add_prpn_cmprsns
#' @export 
#' @importFrom dplyr mutate pull select
#' @importFrom rlang sym
#' @importFrom purrr map2 pluck map_dbl
#' @importFrom lessR Prop_test
#' @importFrom rstatix add_significance
#' @keywords internal
add_prpn_cmprsns <- function (comparison_tb, ds_tb, grouping_var_nms_chr, is_pc_1L_lgl = T, 
    popl_var_nm_1L_chr = "Population (per-cent)", tmp_var_nm_1L_chr = "cmprsn_test_ls") 
{
    comparison_tb <- comparison_tb %>% dplyr::mutate(`:=`(!!rlang::sym(tmp_var_nm_1L_chr), 
        !!rlang::sym(grouping_var_nms_chr[2]) %>% purrr::map2(comparison_tb[[popl_var_nm_1L_chr]], 
            ~{
                total_1L_int <- ds_tb %>% dplyr::pull(!!rlang::sym(grouping_var_nms_chr[1])) %>% 
                  table() %>% purrr::pluck(.x)
                if (is.null(total_1L_int)) {
                  total_1L_int <- 0
                }
                lessR::Prop_test(n_succ = total_1L_int, n_fail = length(ds_tb %>% 
                  dplyr::pull(!!rlang::sym(grouping_var_nms_chr[1])) %>% 
                  na.omit()) - total_1L_int, pi = .y/ifelse(is_pc_1L_lgl, 
                  100, 1))
            }))) %>% dplyr::mutate(CI_95_Lower = !!rlang::sym(tmp_var_nm_1L_chr) %>% 
        purrr::map_dbl(~ifelse(!is.null(.x), .x$conf.int[1] * 
            ifelse(is_pc_1L_lgl, 100, 1), NA_real_)), CI_95_Upper = !!rlang::sym(tmp_var_nm_1L_chr) %>% 
        purrr::map_dbl(~ifelse(!is.null(.x), .x$conf.int[2] * 
            ifelse(is_pc_1L_lgl, 100, 1), NA_real_)), p = !!rlang::sym(tmp_var_nm_1L_chr) %>% 
        purrr::map_dbl(~ifelse(!is.null(.x), .x$p.value, NA_real_)))
    comparision_tb <- comparison_tb %>% rstatix::add_significance(p.col = "p") %>% 
        dplyr::select(-!!rlang::sym(tmp_var_nm_1L_chr))
    return(comparision_tb)
}
#' Add qualitative assessment red flag
#' @description add_qltv_ax_red_flag() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add qualitative assessment red flag. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param qltv_ax_red_flag_1L_chr Qualitative assessment red flag (a character vector of length one)
#' @param qltv_ax_var_nm_1L_chr Qualitative assessment variable name (a character vector of length one)
#' @return Data (a tibble)
#' @rdname add_qltv_ax_red_flag
#' @export 
#' @importFrom dplyr mutate
#' @importFrom purrr map_lgl
#' @importFrom rlang sym
#' @keywords internal
add_qltv_ax_red_flag <- function (data_tb, qltv_ax_red_flag_1L_chr, qltv_ax_var_nm_1L_chr) 
{
    data_tb <- data_tb %>% dplyr::mutate(red_flag_qltv_ax_lgl = purrr::map_lgl(!!rlang::sym(qltv_ax_var_nm_1L_chr), 
        ~ifelse(.x == qltv_ax_red_flag_1L_chr, T, F)))
    return(data_tb)
}
#' Add red flag totals
#' @description add_red_flag_totals() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add red flag totals. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param flags_chr Flags (a character vector)
#' @param flags_tot_var_nm_1L_chr Flags total variable name (a character vector of length one)
#' @return Data (a tibble)
#' @rdname add_red_flag_totals
#' @export 
#' @importFrom dplyr select mutate
#' @importFrom rlang sym
#' @importFrom purrr reduce
#' @keywords internal
add_red_flag_totals <- function (data_tb, flags_chr, flags_tot_var_nm_1L_chr) 
{
    flags_tb <- data_tb %>% dplyr::select(flags_chr)
    flags_tb[is.na(flags_tb)] <- FALSE
    data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(flags_tot_var_nm_1L_chr), 
        purrr::reduce(flags_tb, .init = 0, ~.x + .y)))
    return(data_tb)
}
#' Add red flags
#' @description add_red_flags() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add red flags. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param age_var_nm_1L_chr Age variable name (a character vector of length one), Default: character(0)
#' @param attempt_dur_min_1L_dbl Attempt duration minimum (a double vector of length one), Default: numeric(0)
#' @param attempt_dur_var_nm_1L_chr Attempt duration variable name (a character vector of length one), Default: character(0)
#' @param date_stamp_format_1L_chr Date stamp format (a character vector of length one), Default: 'DMY'
#' @param date_stamp_var_nm_1L_chr Date stamp variable name (a character vector of length one), Default: character(0)
#' @param dob_format_1L_chr Date of birth format (a character vector of length one), Default: 'DMY'
#' @param dob_var_nm_1L_chr Date of birth variable name (a character vector of length one), Default: character(0)
#' @param email_max_cnstv_digits_1L_int Email maximum consecutive digits (an integer vector of length one), Default: integer(0)
#' @param email_var_nm_1L_chr Email variable name (a character vector of length one), Default: character(0)
#' @param flags_tot_var_nm_1L_chr Flags total variable name (a character vector of length one), Default: 'red_flag_count_int'
#' @param lat_lng_var_nms_chr Lat longitude variable names (a character vector), Default: character(0)
#' @param qltv_ax_red_flag_1L_chr Qualitative assessment red flag (a character vector of length one), Default: 'Red'
#' @param qltv_ax_tb Qualitative assessment (a tibble), Default: NULL
#' @param qltv_ax_var_nm_1L_chr Qualitative assessment variable name (a character vector of length one), Default: character(0)
#' @param unique_by_case_var_nms_chr Unique by case variable names (a character vector), Default: character(0)
#' @param valid_countries_chr Valid countries (a character vector), Default: character(0)
#' @return Data (a tibble)
#' @rdname add_red_flags
#' @export 
#' @importFrom dplyr left_join
#' @keywords internal
add_red_flags <- function (data_tb, age_var_nm_1L_chr = character(0), attempt_dur_min_1L_dbl = numeric(0), 
    attempt_dur_var_nm_1L_chr = character(0), date_stamp_format_1L_chr = "DMY", 
    date_stamp_var_nm_1L_chr = character(0), dob_format_1L_chr = "DMY", 
    dob_var_nm_1L_chr = character(0), email_max_cnstv_digits_1L_int = integer(0), 
    email_var_nm_1L_chr = character(0), flags_tot_var_nm_1L_chr = "red_flag_count_int", 
    lat_lng_var_nms_chr = character(0), qltv_ax_red_flag_1L_chr = "Red", 
    qltv_ax_tb = NULL, qltv_ax_var_nm_1L_chr = character(0), 
    unique_by_case_var_nms_chr = character(0), valid_countries_chr = character(0)) 
{
    flags_chr <- character(0)
    if (!identical(valid_countries_chr, character(0))) {
        data_tb <- add_country_red_flag(data_tb = data_tb, lat_lng_var_nms_chr = lat_lng_var_nms_chr, 
            valid_countries_chr = valid_countries_chr)
        flags_chr <- c(flags_chr, "red_flag_country_lgl")
    }
    if (!identical(dob_var_nm_1L_chr, character(0)) | !identical(age_var_nm_1L_chr, 
        character(0))) {
        data_tb <- data_tb %>% add_reported_age_red_flag(date_stamp_var_nm_1L_chr = date_stamp_var_nm_1L_chr, 
            dob_var_nm_1L_chr = dob_var_nm_1L_chr, age_var_nm_1L_chr = age_var_nm_1L_chr)
        flags_chr <- c(flags_chr, "red_flag_reported_age_lgl")
    }
    if (!identical(email_var_nm_1L_chr, character(0))) {
        data_tb <- data_tb %>% add_email_red_flag(email_var_nm_1L_chr = email_var_nm_1L_chr, 
            email_max_cnstv_digits_1L_int = email_max_cnstv_digits_1L_int)
        flags_chr <- c(flags_chr, "red_flag_email_lgl")
    }
    if (!identical(unique_by_case_var_nms_chr, character(0))) {
        data_tb <- data_tb %>% add_duplicate_text_red_flag(unique_by_case_var_nms_chr = unique_by_case_var_nms_chr)
        flags_chr <- c(flags_chr, paste0("red_flag_duplicate_txt_var", 
            1:length(unique_by_case_var_nms_chr), "_lgl"))
    }
    if (!identical(qltv_ax_var_nm_1L_chr, character(0))) {
        data_tb <- data_tb %>% dplyr::left_join(qltv_ax_tb)
        data_tb <- data_tb %>% add_qltv_ax_red_flag(qltv_ax_red_flag_1L_chr = qltv_ax_red_flag_1L_chr, 
            qltv_ax_var_nm_1L_chr = qltv_ax_var_nm_1L_chr)
        flags_chr <- c(flags_chr, "red_flag_qltv_ax_lgl")
    }
    if (!identical(attempt_dur_min_1L_dbl, numeric(0))) {
        data_tb <- data_tb %>% add_attempt_dur_red_flag(attempt_dur_min_1L_dbl = attempt_dur_min_1L_dbl, 
            attempt_dur_var_nm_1L_chr = attempt_dur_var_nm_1L_chr)
        flags_chr <- c(flags_chr, "red_flag_attempt_dur_lgl")
    }
    data_tb <- add_red_flag_totals(data_tb = data_tb, flags_chr = flags_chr, 
        flags_tot_var_nm_1L_chr = flags_tot_var_nm_1L_chr)
    return(data_tb)
}
#' Add reported age red flag
#' @description add_reported_age_red_flag() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add reported age red flag. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param date_stamp_var_nm_1L_chr Date stamp variable name (a character vector of length one)
#' @param dob_var_nm_1L_chr Date of birth variable name (a character vector of length one)
#' @param age_var_nm_1L_chr Age variable name (a character vector of length one)
#' @param date_stamp_format_1L_chr Date stamp format (a character vector of length one), Default: 'DMY'
#' @param dob_format_1L_chr Date of birth format (a character vector of length one), Default: 'DMY'
#' @return Data (a tibble)
#' @rdname add_reported_age_red_flag
#' @export 
#' @importFrom stringr str_extract_all
#' @importFrom purrr pluck map map2_lgl
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @importFrom lubridate interval years
#' @keywords internal
add_reported_age_red_flag <- function (data_tb, date_stamp_var_nm_1L_chr, dob_var_nm_1L_chr, 
    age_var_nm_1L_chr, date_stamp_format_1L_chr = "DMY", dob_format_1L_chr = "DMY") 
{
    fn_attribution_1L_chr <- "This function is partly based on: https://stackoverflow.com/questions/32312925/time-difference-in-years-with-lubridate"
    fmt_seq_ls <- list(date_stamp_format_1L_chr %>% tolower() %>% 
        stringr::str_extract_all("y|m|d") %>% purrr::pluck(1) %>% 
        unique(), dob_format_1L_chr %>% tolower() %>% stringr::str_extract_all("y|m|d") %>% 
        purrr::pluck(1) %>% unique())
    data_tb <- data_tb %>% dplyr::mutate(der_date_of_input = !!rlang::sym(date_stamp_var_nm_1L_chr) %>% 
        strsplit("/") %>% purrr::map(~ISOdate(gsub(" .*$", "", 
        .x[which(fmt_seq_ls[[1]] == "y")]), gsub(" .*$", "", 
        .x[which(fmt_seq_ls[[1]] == "m")]), gsub(" .*$", "", 
        .x[which(fmt_seq_ls[[1]] == "d")])) %>% format(format = "%Y-%m-%d")) %>% 
        unlist() %>% as.Date())
    data_tb <- data_tb %>% dplyr::mutate(der_date_of_birth = !!rlang::sym(dob_var_nm_1L_chr) %>% 
        strsplit("/") %>% purrr::map(~ISOdate(gsub(" .*$", "", 
        .x[which(fmt_seq_ls[[2]] == "y")]), gsub(" .*$", "", 
        .x[which(fmt_seq_ls[[2]] == "m")]), gsub(" .*$", "", 
        .x[which(fmt_seq_ls[[2]] == "d")])) %>% format(format = "%Y-%m-%d")) %>% 
        unlist() %>% as.Date()) %>% dplyr::mutate(der_age_from_dob = lubridate::interval(der_date_of_birth, 
        der_date_of_input)%/%lubridate::years(1))
    data_tb <- data_tb %>% dplyr::mutate(red_flag_reported_age_lgl = purrr::map2_lgl(!!rlang::sym(age_var_nm_1L_chr), 
        der_age_from_dob, ~ifelse(.x == .y, F, T)))
    return(data_tb)
}
#' Add Social Interaction Anxiety Scale totals
#' @description add_sias_totals() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add social interaction anxiety scale totals. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param itm_prefix_1L_chr Item prefix (a character vector of length one)
#' @param drvd_var_prefix_1L_chr Derived variable prefix (a character vector of length one)
#' @param sias_ctg_sfx_1L_chr Social Interaction Anxiety Scale category suffix (a character vector of length one), Default: 'SIAS_ctg'
#' @param sias_tot_sfx_1L_chr Social Interaction Anxiety Scale total suffix (a character vector of length one), Default: 'SIAS_tot'
#' @return Data (a tibble)
#' @rdname add_sias_totals
#' @export 
#' @importFrom dplyr mutate_at vars setdiff starts_with matches intersect rename_at contains mutate select
#' @importFrom purrr map_dbl map_chr
#' @importFrom rlang sym
#' @keywords internal
add_sias_totals <- function (data_tb, itm_prefix_1L_chr, drvd_var_prefix_1L_chr, 
    sias_ctg_sfx_1L_chr = "SIAS_ctg", sias_tot_sfx_1L_chr = "SIAS_tot") 
{
    der_suffix <- paste0("_", drvd_var_prefix_1L_chr)
    new_sias_prefix <- paste0(drvd_var_prefix_1L_chr, "_", itm_prefix_1L_chr)
    data_tb <- data_tb %>% dplyr::mutate_at(dplyr::vars(dplyr::setdiff(dplyr::starts_with(itm_prefix_1L_chr), 
        dplyr::matches("_5$|_9$|_11$"))), .funs = list(der = ~purrr::map_dbl(., 
        ~switch(as.numeric(.x), 0, 1, 2, 3, 4)))) %>% dplyr::mutate_at(dplyr::vars(dplyr::intersect(dplyr::starts_with(itm_prefix_1L_chr), 
        dplyr::matches("_5$|_9$|_11$"))), .funs = list(der = ~purrr::map_dbl(., 
        ~switch(as.numeric(.x), 4, 3, 2, 1, 0)))) %>% dplyr::rename_at(dplyr::vars(dplyr::contains(der_suffix)), 
        list(~paste(drvd_var_prefix_1L_chr, gsub(der_suffix, 
            "", .), sep = "_")))
    data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(drvd_var_prefix_1L_chr, 
        "_", sias_tot_sfx_1L_chr)), rowSums(data_tb %>% dplyr::select(dplyr::starts_with(new_sias_prefix))))) %>% 
        dplyr::mutate(`:=`(!!rlang::sym(paste0(drvd_var_prefix_1L_chr, 
            "_", sias_ctg_sfx_1L_chr)), !!rlang::sym(paste0(drvd_var_prefix_1L_chr, 
            "_", sias_tot_sfx_1L_chr)) %>% purrr::map_chr(~ifelse(.x < 
            34, "Normal_Range", ifelse(.x < 43, "Social_Phobia", 
            "Social_Anxiety"))) %>% factor(levels = c("Normal_Range", 
            "Social_Phobia", "Social_Anxiety"), ordered = T))) %>% 
        return(data_tb)
}
