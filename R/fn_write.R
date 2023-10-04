#' Write choice modelling workspace
#' @description write_choice_mdlng_ws() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write choice modelling workspace. The function returns Paths (a list).
#' @param paths_ls Paths (a list)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @return Paths (a list)
#' @rdname write_choice_mdlng_ws
#' @export 
#' @importFrom purrr reduce pluck
#' @importFrom ready4 write_prj_outp_dirs
#' @keywords internal
write_choice_mdlng_ws <- function (paths_ls, consent_1L_chr = "") 
{
    paths_ls <- purrr::reduce(list("mychoice", c("Private", "Public"), 
        c("Replication", "Results"), c("Records", "Models")), 
        .init = paths_ls, ~ready4::write_prj_outp_dirs(.y, output_data_dir_1L_chr = .x %>% 
            purrr::pluck(ifelse("mychoice" %in% .y, "output_data_dir_1L_chr", 
                ifelse("Private" %in% .y, "mychoice", ifelse("Replication" %in% 
                  .y, "Public", "Private")))), paths_ls = .x, 
            consent_1L_chr = consent_1L_chr))
    return(paths_ls)
}
#' Write preprocessing output
#' @description write_preprocessing_outp() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write preprocessing output. The function returns Paths (a list).
#' @param paths_ls Paths (a list)
#' @param mdl_params_ls Model parameters (a list), Default: NULL
#' @param preprocessing_log_ls Preprocessing log (a list), Default: NULL
#' @param records_ls Records (a list), Default: NULL
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @return Paths (a list)
#' @rdname write_preprocessing_outp
#' @export 
#' @importFrom ready4 write_obj_with_prompt
#' @keywords internal
write_preprocessing_outp <- function (paths_ls, mdl_params_ls = NULL, preprocessing_log_ls = NULL, 
    records_ls = NULL, consent_1L_chr = "") 
{
    paths_ls <- write_choice_mdlng_ws(paths_ls, consent_1L_chr = consent_1L_chr)
    if (!is.null(mdl_params_ls)) 
        ready4::write_obj_with_prompt(mdl_params_ls, obj_nm_1L_chr = "mdl_params_ls", 
            outp_dir_1L_chr = paths_ls$Replication, consent_1L_chr = consent_1L_chr)
    if (!is.null(preprocessing_log_ls)) 
        ready4::write_obj_with_prompt(preprocessing_log_ls, obj_nm_1L_chr = "preprocessing_log_ls", 
            outp_dir_1L_chr = paths_ls$Replication, consent_1L_chr = consent_1L_chr)
    if (!is.null(records_ls)) 
        ready4::write_obj_with_prompt(records_ls, obj_nm_1L_chr = "records_ls", 
            outp_dir_1L_chr = paths_ls$Records, consent_1L_chr = consent_1L_chr)
    return(paths_ls)
}
