#' Print comparison kable
#' @description print_cmprsn_kbl() is a Print function that prints output to console Specifically, this function implements an algorithm to print comparison kable. The function is called for its side effects and does not return a value.
#' @param cmprsn_tb Comparison (a tibble)
#' @param caption_1L_chr Caption (a character vector of length one)
#' @param footnotes_chr Footnotes (a character vector), Default: character(0)
#' @return Comparison (a kable)
#' @rdname print_cmprsn_kbl
#' @export 
#' @importFrom dplyr mutate rename across select
#' @importFrom purrr map2_chr
#' @importFrom kableExtra kbl footnote
#' @keywords internal
print_cmprsn_kbl <- function (cmprsn_tb, caption_1L_chr, footnotes_chr = character(0)) 
{
    if (identical(footnotes_chr, character(0))) 
        footnotes_chr <- "Significance codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘n.s.’ 1"
    cmprsn_tb <- cmprsn_tb %>% dplyr::mutate(p.signif = ifelse(p.signif == 
        "****", "***", ifelse(p.signif == "ns", "n.s.", p.signif))) %>% 
        dplyr::rename(signif. = p.signif) %>% dplyr::mutate(dplyr::across(where(is.numeric), 
        ~round(.x, digits = params$X@digits_int))) %>% dplyr::mutate(CI = CI_95_Lower %>% 
        purrr::map2_chr(CI_95_Upper, ~paste0("(", .x, ", ", .y, 
            ")"))) %>% dplyr::select(c(1:3, 9, 6:8))
    cmprsn_kbl <- cmprsn_tb %>% kableExtra::kbl(booktabs = T, 
        longtable = T, caption = caption_1L_chr) %>% kableExtra::footnote(general = footnotes_chr)
    return(cmprsn_kbl)
}
#' Print model summary kable
#' @description print_mdl_smry_kbl() is a Print function that prints output to console Specifically, this function implements an algorithm to print model summary kable. The function is called for its side effects and does not return a value.
#' @param tfd_mdl_smry_tb Transformed model summary (a tibble)
#' @param records_ls Records (a list)
#' @param caption_1L_chr Caption (a character vector of length one)
#' @param footnotes_chr Footnotes (a character vector), Default: character(0)
#' @return Model summary (a kable)
#' @rdname print_mdl_smry_kbl
#' @export 
#' @importFrom dplyr select
#' @importFrom kableExtra kbl kable_styling pack_rows footnote
#' @keywords internal
print_mdl_smry_kbl <- function (tfd_mdl_smry_tb, records_ls, caption_1L_chr, footnotes_chr = character(0)) 
{
    if (identical(footnotes_chr, character(0))) 
        footnotes_chr <- "Significance codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘n.s.’ 1"
    mdl_smry_kbl <- tfd_mdl_smry_tb %>% dplyr::select(-Concept) %>% 
        kableExtra::kbl(booktabs = T, longtable = T, caption = caption_1L_chr) %>% 
        kableExtra::kable_styling() %>% kableExtra::pack_rows(index = make_smry_grouping_idxs(tfd_mdl_smry_tb, 
        records_ls = records_ls)) %>% kableExtra::footnote(general = footnotes_chr)
    return(mdl_smry_kbl)
}
#' Print transformed model summary
#' @description print_tfd_mdl_smry() is a Print function that prints output to console Specifically, this function implements an algorithm to print transformed model summary. The function is called for its side effects and does not return a value.
#' @param model_mdl Model (a model)
#' @param caption_1L_chr Caption (a character vector of length one)
#' @param dce_design_ls Discrete choice experiment design (a list)
#' @param mdl_params_ls Model parameters (a list), Default: NULL
#' @param records_ls Records (a list)
#' @param confidence_intvl_1L_chr Confidence interval (a character vector of length one), Default: character(0)
#' @param confidence_intvl_int Confidence interval (an integer vector), Default: integer(0)
#' @param digits_1L_int Digits (an integer vector of length one), Default: 3
#' @param footnotes_chr Footnotes (a character vector), Default: character(0)
#' @param print_call_1L_lgl Print call (a logical vector of length one), Default: T
#' @param significance_1L_chr Significance (a character vector of length one), Default: character(0)
#' @param opt_out_nm_1L_chr Opt out name (a character vector of length one), Default: 'Opt out'
#' @return Transformed model summary (a kable)
#' @rdname print_tfd_mdl_smry
#' @export 
#' @keywords internal
print_tfd_mdl_smry <- function (model_mdl, caption_1L_chr, dce_design_ls, mdl_params_ls = NULL, 
    records_ls, confidence_intvl_1L_chr = character(0), confidence_intvl_int = integer(0), 
    digits_1L_int = 3L, footnotes_chr = character(0), print_call_1L_lgl = T, 
    significance_1L_chr = character(0), opt_out_nm_1L_chr = "Opt out") 
{
    if (identical(significance_1L_chr, character(0))) 
        significance_1L_chr <- "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘n.s. ’ 1"
    if (print_call_1L_lgl) {
        call_chr <- c("Call: ", model_mdl$call %>% print() %>% 
            capture.output() %>% trimws())
    }
    else {
        call_chr <- character(0)
    }
    footnotes_chr <- c(significance_1L_chr, call_chr, footnotes_chr)
    tfd_mdl_smry_kbl <- make_mdl_smry(model_mdl) %>% transform_mdl_smry(dce_design_ls = dce_design_ls, 
        mdl_params_ls = mdl_params_ls, records_ls = records_ls, 
        confidence_intvl_1L_chr = confidence_intvl_1L_chr, confidence_intvl_int = confidence_intvl_int, 
        digits_1L_int = digits_1L_int, opt_out_nm_1L_chr = opt_out_nm_1L_chr) %>% 
        print_mdl_smry_kbl(records_ls = records_ls, caption_1L_chr = caption_1L_chr, 
            footnotes_chr = footnotes_chr)
    return(tfd_mdl_smry_kbl)
}
