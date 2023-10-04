get_att_smrys <- function(dce_design_ls,
                          return_1L_chr = "levels"){
  att_smry_tb <- dce_design_ls$choice_sets_ls$att_lvls_tb %>% dplyr::group_by(attribute_chr) %>% dplyr::summarise(levels = dplyr::n(), type = dplyr::first(continuous_lgl) %>% ifelse("C","D"))
  return_xx  <- get_atts(dce_design_ls$choice_sets_ls$att_lvls_tb) %>%
    purrr::map(~ready4::get_from_lup_obj(att_smry_tb,
                                         match_var_nm_1L_chr = "attribute_chr",
                                         match_value_xx = .x,
                                         target_var_nm_1L_chr = return_1L_chr))
  if(return_1L_chr == "levels")
    return_xx <- return_xx %>% purrr::flatten_int()
  if(return_1L_chr == "type")
    return_xx <- return_xx %>% purrr::flatten_chr()
  return(return_xx)
}
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
get_lvls <- function(att_lvls_tb, return_1L_chr = "all"){
  atts_chr <- get_atts(att_lvls_tb, return_1L_chr = return_1L_chr)
  lvls_ls <- atts_chr %>%
    purrr::map(~att_lvls_tb %>% dplyr::filter(attribute_chr == .x) %>% dplyr::pull(level_chr)) %>%
    stats::setNames(atts_chr)
  return(lvls_ls)
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
