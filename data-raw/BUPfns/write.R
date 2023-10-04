write_choice_mdlng_ws <- function(paths_ls,
                                  consent_1L_chr = ""){
  paths_ls <- purrr::reduce(list("mychoice",
                                 c("Private","Public"),
                                 c("Replication","Results"),
                                 c("Records","Models")),
                            .init = paths_ls,
                            ~ write_prj_outp_dirs(.y,
                                                  output_data_dir_1L_chr = .x %>% purrr::pluck(ifelse("mychoice" %in% .y,
                                                                                                      "output_data_dir_1L_chr",
                                                                                                      ifelse("Private" %in% .y,
                                                                                                             "mychoice",
                                                                                                             ifelse("Replication" %in% .y,
                                                                                                                    "Public",
                                                                                                                    "Private"))
                                                  )),
                                                  paths_ls = .x,
                                                  consent_1L_chr = consent_1L_chr))
  return(paths_ls)
}
write_preprocessing_outp <- function(paths_ls,
                                     mdl_params_ls = NULL,
                                     preprocessing_log_ls = NULL,
                                     records_ls = NULL,
                                     consent_1L_chr = ""){
  paths_ls <- write_choice_mdlng_ws(paths_ls,
                                    consent_1L_chr = consent_1L_chr)
  if(!is.null(mdl_params_ls))
    write_obj_with_prompt(mdl_params_ls,
                          obj_nm_1L_chr = "mdl_params_ls",
                          outp_dir_1L_chr = paths_ls$Replication,
                          consent_1L_chr = consent_1L_chr)
  if(!is.null(preprocessing_log_ls))
    write_obj_with_prompt(preprocessing_log_ls,
                          obj_nm_1L_chr = "preprocessing_log_ls",
                          outp_dir_1L_chr = paths_ls$Replication,
                          consent_1L_chr = consent_1L_chr)
  if(!is.null(records_ls))
    write_obj_with_prompt(records_ls,
                          obj_nm_1L_chr = "records_ls",
                          outp_dir_1L_chr = paths_ls$Records,
                          consent_1L_chr = consent_1L_chr)
  return(paths_ls)
}
