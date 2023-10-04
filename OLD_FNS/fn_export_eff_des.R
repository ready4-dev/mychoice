#' @title export_eff_des
#' @description FUNCTION_DESCRIPTION
#' @param survey_features_ls PARAM_DESCRIPTION
#' @param parallel PARAM_DESCRIPTION, Default: FALSE
#' @param output_dir PARAM_DESCRIPTION
#' @param pilot_analysis PARAM_DESCRIPTION, Default: NULL
#' @param start_des PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[idefix]{Modfed}}
#' @rdname export_eff_des
#' @export
#' @importFrom idefix Modfed
export_eff_des <- function(survey_features_ls,
                           parallel = FALSE,
                           output_dir,
                           pilot_analysis = NULL,
                           start_des){
  if(is.null(pilot_analysis))
    par.draws <- survey_features_ls$p.d
  else
    par.draws <- pilot_analysis$sample
  no_app_optout_ls <- idefix::Modfed(cand.set = survey_features_ls$candidate_des_mat,
                                     n.sets = survey_features_ls$n_sets,
                                     n.alts = survey_features_ls$n_alts,
                                     no.choice = survey_features_ls$no_choice_lgl,
                                     alt.cte = survey_features_ls$alt_cte,
                                     parallel = parallel,
                                     par.draws = par.draws,
                                     start.des = start_des)
  ## This section needs abstracting before the function can be used for other survey designs.
  dir.create(output_dir)
  dir.create(paste0(output_dir,"/block_1"))
  dir.create(paste0(output_dir,"/block_2"))
  ##
  saveRDS(no_app_optout_ls,paste0(output_dir,"/no_app_optout_ls.rds"))
  no_app_optout_ls
}

