#' Knit choice descriptives report
#' @description knit_choice_descvs_rprt() is a Knit function that knits an RMD or Rmarkdown file. Specifically, this function implements an algorithm to knit choice descriptives report. The function is called for its side effects and does not return a value.
#' @param choice_descvs_params_ls Choice descriptives parameters (a list)
#' @param path_to_mdl_rprt_tmpl_1L_chr Path to model report template (a character vector of length one)
#' @return No return value, called for side effects.
#' @rdname knit_choice_descvs_rprt
#' @export 
#' @importFrom purrr pmap
#' @seealso [knitr::knit_expand()] and [knitr::knit_child()]
#' @keywords internal
knit_choice_descvs_rprt <- function (choice_descvs_params_ls, path_to_mdl_rprt_tmpl_1L_chr) 
{
    src <- purrr::pmap(choice_descvs_params_ls, ~knitr::knit_expand(path_to_mdl_rprt_tmpl_1L_chr, 
        att_txt_1L_chr = ..1, att_var_nm_1L_chr = ..2 %>% deparse()))
    res <- knitr::knit_child(text = unlist(src), quiet = TRUE)
    cat(res, sep = "\n")
}
#' Knit individual models report
#' @description knit_indl_mdls_rprt() is a Knit function that knits an RMD or Rmarkdown file. Specifically, this function implements an algorithm to knit individual models report. The function is called for its side effects and does not return a value.
#' @param indl_mdl_knit_params_ls Individual model knit parameters (a list)
#' @param path_to_mdl_rprt_tmpl_1L_chr Path to model report template (a character vector of length one)
#' @return No return value, called for side effects.
#' @rdname knit_indl_mdls_rprt
#' @export 
#' @importFrom purrr pmap
#' @seealso [knitr::knit_expand()] and [knitr::knit_child()]
#' @keywords internal
knit_indl_mdls_rprt <- function (indl_mdl_knit_params_ls, path_to_mdl_rprt_tmpl_1L_chr) 
{
    src <- purrr::pmap(indl_mdl_knit_params_ls, ~knitr::knit_expand(path_to_mdl_rprt_tmpl_1L_chr, 
        idx_1L_int = ..1, characteristics_1L_chr = ..2 %>% deparse()))
    res <- knitr::knit_child(text = unlist(src), quiet = TRUE)
    cat(res, sep = "\n")
}
