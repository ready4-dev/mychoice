get_atts <- function(att_lvls_tb,
                     return_1L_chr = "all"){
  atts_chr <- att_lvls_tb$attribute_chr %>% unique()
  if(return_1L_chr == "fctr"){
    atts_chr <- atts_chr[make_fctr_atts_indcs(att_lvls_tb)]
  }
  if(return_1L_chr == "cont"){
    atts_chr <- atts_chr[make_cont_atts_indcs(att_lvls_tb)]
  }
  return(atts_chr)
}
get_fctr_atts_dummy_var_nms <- function(att_lvls_tb,
                                        flatten_1L_lgl = F){
  fctr_atts_chr <- get_atts(att_lvls_tb, return_1L_chr = "fctr")
  fctr_atts_dummy_var_nms_xx <- fctr_atts_chr %>%
    purrr::map(~ att_lvls_tb %>% dplyr::filter(attribute_chr == .x) %>% dplyr::pull(dummy_nm_chr) %>% purrr::discard(is.na)) %>%
    stats::setNames(fctr_atts_chr)
  if(flatten_1L_lgl){
    fctr_atts_dummy_var_nms_xx <- fctr_atts_dummy_var_nms_xx %>%
      purrr::flatten_chr()
  }
  return(fctr_atts_dummy_var_nms_xx)
}
get_nbr_of_choices <- function(choice_sets_ls){
  nbr_of_choices_1L_int <- choice_sets_ls$nbr_of_sets_1L_int/choice_sets_ls$nbr_of_blocks_1L_int
  return(nbr_of_choices_1L_int)
}
get_opt_out_var_nm <- function(case_choices_mat,
                               choice_sets_ls){
  opt_out_nm_1L_chr <- colnames(case_choices_mat)[!colnames(case_choices_mat) %in% (c(make_fctr_atts_tmp_var_nms(choice_sets_ls$att_lvls_tb),make_cont_atts_tmp_var_nms(choice_sets_ls$att_lvls_tb)))]
  return(opt_out_nm_1L_chr)
}
get_signft_concepts <- function(att_predn_mdls_ls,
                                mdl_params_ls,
                                min_threshold_1L_int = 1L,
                                exclude_chr = character(0)){
  signft_concepts_tb <- make_signft_concepts_tbl(att_predn_mdls_ls,
                                                 mdl_params_ls = mdl_params_ls)
  signft_concepts_chr <- dplyr::filter(signft_concepts_tb,
                                       total_int >= min_threshold_1L_int,
                                       !concept_chr %in% exclude_chr) %>%
    dplyr::pull(concept_chr)
  return(signft_concepts_chr)
}
