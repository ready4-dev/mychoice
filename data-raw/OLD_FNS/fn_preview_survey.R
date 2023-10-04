
#' @title preview_survey
#' @description FUNCTION_DESCRIPTION
#' @param no_app_optout_ls PARAM_DESCRIPTION
#' @param survey_features_ls PARAM_DESCRIPTION
#' @param pilot PARAM_DESCRIPTION, Default: T
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_replace}}
#'  \code{\link[idefix]{SurveyApp}}
#' @rdname preview_survey
#' @export
#' @importFrom stringr str_replace_all
#' @importFrom idefix SurveyApp
preview_survey <- function(no_app_optout_ls,
                           survey_features_ls,
                           pilot = T){
  attributes <- names(survey_features_ls$lvl_names) %>% stringr::str_replace_all("_"," ")
  labels <- survey_features_ls$lvl_names %>% unname()
  if(pilot)
    i.text <- "Pilot Survey Preview (All choice cards from all blocks)"
  else
    i.text <- "Final Survey Preview (All choice cards from all blocks)"
  b.text <- "Please choose the alternative you prefer"
  e.text <- "Thanks for taking the survey"
  idefix::SurveyApp(des = no_app_optout_ls$design,
                    n.total = survey_features_ls$n_sets,
                    alts = survey_features_ls$alternatives,
                    atts = attributes,
                    lvl.names = labels,
                    c.lvls = survey_features_ls$con_lvls,
                    coding = survey_features_ls$c_typ,
                    buttons.text = b.text,
                    intro.text = i.text,
                    end.text = e.text,
                    no.choice = survey_features_ls$no_choice_idx,
                    alt.cte = survey_features_ls$alt_cte)
}
