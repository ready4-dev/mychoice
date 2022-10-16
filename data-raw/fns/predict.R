predict_mkt_share <- function(dce_design_ls,
                              mdls_ls,
                              mdl_params_ls,
                              new_choices_ls,
                              records_ls,
                              #alternative_var_nm_1L_chr = "alternative",
                              altv_nms_chr = character(0),
                              with_1L_chr = "mnl_mlogit_mdl"){
  choices_ls <- make_choices_ls(dce_design_ls,
                                new_choices_ls = new_choices_ls)
  replace_cards_int <- 1#if(with_1L_chr %in% "mixl_mlogit_mdl"){1}else{integer(0)}
  new_choices_dfidx <- make_new_choice_ds(choices_ls,
                                          dce_design_ls = dce_design_ls,
                                          mdl_params_ls = mdl_params_ls,
                                          replace_cards_int = replace_cards_int,
                                          records_ls = records_ls) #
  if(identical(altv_nms_chr, character(0))){
    altv_nms_chr <- dce_design_ls$choice_sets_ls$alternatives_chr
  }
  predd_mat <- predict(mdls_ls %>% purrr::pluck(with_1L_chr), newdata = new_choices_dfidx)
  if(!identical(replace_cards_int, integer(0))){
    indices_int <- matrix(which(new_choices_dfidx$card_id==1), ncol = length(altv_nms_chr))
    predd_mat <- predd_mat[1+((matrix(which(new_choices_dfidx$card_id==1), nrow = length(altv_nms_chr)) %>% t())[,1]-1)/length(altv_nms_chr),]
  }
  predd_share_dbl <- 1:ncol(predd_mat) %>% purrr::map_dbl(~mean(predd_mat[,.x])) %>% stats::setNames(altv_nms_chr)
  return(predd_share_dbl)
}
