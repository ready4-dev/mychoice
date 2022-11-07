print_cmprsn_kbl <- function(cmprsn_tb,
                             caption_1L_chr,
                             footnotes_chr = character(0)){
  if(identical(footnotes_chr, character(0)))
    footnotes_chr <- "Significance codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘n.s.’ 1"
  cmprsn_tb <- cmprsn_tb %>%
    dplyr::mutate(p.signif = ifelse(p.signif =="****","***",
                                    ifelse(p.signif=="ns",
                                           "n.s.",
                                           p.signif))) %>%
    dplyr::rename(`signif.` = `p.signif`) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, digits = params$X@digits_int))) %>%
    dplyr::mutate(CI = CI_95_Lower %>% purrr::map2_chr(CI_95_Upper,~paste0("(",.x,", ",.y,")"))) %>%
    dplyr::select(c(1:3,9,6:8))
  cmprsn_kbl <- cmprsn_tb %>%
    kableExtra::kbl(booktabs = T, longtable = T,
                    caption = caption_1L_chr) %>%
    kableExtra::footnote(general = footnotes_chr)
  return(cmprsn_kbl)
}
print_mdl_smry_kbl <- function(tfd_mdl_smry_tb,
                               records_ls,
                               caption_1L_chr,
                               footnotes_chr = character(0)){
  if(identical(footnotes_chr, character(0)))
    footnotes_chr <-  "Significance codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘n.s.’ 1"
  mdl_smry_kbl <- tfd_mdl_smry_tb %>%
    dplyr::select(-Concept) %>%
    kableExtra::kbl(booktabs = T, longtable = T,
                    caption = caption_1L_chr) %>%
    kableExtra::kable_styling() %>%
    kableExtra::pack_rows(index=make_smry_grouping_idxs(tfd_mdl_smry_tb,records_ls = records_ls)) %>%
    kableExtra::footnote(general = footnotes_chr)
  return(mdl_smry_kbl)
}
print_tfd_mdl_smry <- function(model_mdl,
                               caption_1L_chr,
                               dce_design_ls,
                               mdl_params_ls = NULL,
                               records_ls,
                               confidence_intvl_1L_chr = character(0),
                               confidence_intvl_int = integer(0),
                               digits_1L_int = 3L,
                               footnotes_chr = character(0),
                               print_call_1L_lgl = T,
                               significance_1L_chr = character(0),
                               opt_out_nm_1L_chr = "Opt out"){
  if(identical(significance_1L_chr, character(0)))
    significance_1L_chr <- "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘n.s. ’ 1"
  if(print_call_1L_lgl){
    call_chr <- c("Call: ",
                  model_mdl$call %>% print() %>% capture.output()  %>% trimws())
  }else{
    call_chr <- character(0)
  }
  footnotes_chr <- c(significance_1L_chr,
                     call_chr,
                     footnotes_chr)
  tfd_mdl_smry_kbl <- make_mdl_smry(model_mdl)  %>%
    transform_mdl_smry(dce_design_ls = dce_design_ls,
                       mdl_params_ls = mdl_params_ls,
                       records_ls = records_ls,
                       confidence_intvl_1L_chr = confidence_intvl_1L_chr,
                       confidence_intvl_int = confidence_intvl_int,
                       digits_1L_int = digits_1L_int,
                       opt_out_nm_1L_chr = opt_out_nm_1L_chr)  %>%
    print_mdl_smry_kbl(records_ls = records_ls,
                       caption_1L_chr = caption_1L_chr,
                       footnotes_chr = footnotes_chr)
  return(tfd_mdl_smry_kbl)
}
