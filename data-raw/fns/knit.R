knit_choice_descvs_rprt <- function(choice_descvs_params_ls, path_to_mdl_rprt_tmpl_1L_chr){
  src <- purrr::pmap(choice_descvs_params_ls,
                     ~ knitr::knit_expand(path_to_mdl_rprt_tmpl_1L_chr,
                                          att_txt_1L_chr = ..1,
                                          att_var_nm_1L_chr = ..2 %>% deparse()))
  res <- knitr::knit_child(text = unlist(src), quiet = TRUE)
  cat(res, sep = "\n")
}
knit_indl_mdls_rprt <- function (indl_mdl_knit_params_ls, path_to_mdl_rprt_tmpl_1L_chr) {
  src <- purrr::pmap(indl_mdl_knit_params_ls,
                     ~ knitr::knit_expand(path_to_mdl_rprt_tmpl_1L_chr,
                                          idx_1L_int = ..1,
                                          characteristics_1L_chr = ..2 %>% deparse()))
  res <- knitr::knit_child(text = unlist(src), quiet = TRUE)
  cat(res, sep = "\n")
}
