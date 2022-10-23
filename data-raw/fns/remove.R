remove_no_choice_responses <- function(responses_tb,
                                       choice_card_pfx_1L_chr = "DCE_B"){
  responses_tb <- responses_tb[rowSums(is.na(responses_tb[,names(responses_tb)[purrr::map_lgl(names(responses_tb),
                                                                                              ~startsWith(.x,choice_card_pfx_1L_chr))
  ]])) !=30,]
  return(responses_tb)
}
remove_red_flag_cases <- function(data_tb,
                                  flags_max_1L_int = 0,
                                  flags_tot_var_nm_1L_chr = "red_flag_count_int",
                                  qltv_ax_green_flag_1L_chr = "Green",
                                  qltv_ax_var_nm_1L_chr = character(0)){
  flags_max_1L_int <- as.integer(flags_max_1L_int)
  if(identical(qltv_ax_var_nm_1L_chr, character(0))){
    data_tb <- data_tb %>% dplyr::filter(!!rlang::sym(flags_tot_var_nm_1L_chr) <= flags_max_1L_int)
  }else{
    data_tb <- data_tb %>% dplyr::filter(!!rlang::sym(flags_tot_var_nm_1L_chr) <= flags_max_1L_int | !!rlang::sym(qltv_ax_var_nm_1L_chr) == qltv_ax_green_flag_1L_chr)
  }
  return(data_tb)
}
