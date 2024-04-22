#' Predict market share
#' @description predict_mkt_share() is a Predict function that applies a model to make predictions. Specifically, this function implements an algorithm to predict market share. The function returns Predicted share (a double vector).
#' @param dce_design_ls Discrete choice experiment design (a list)
#' @param mdls_ls Models (a list)
#' @param mdl_params_ls Model parameters (a list)
#' @param new_choices_ls New choices (a list)
#' @param records_ls Records (a list)
#' @param altv_nms_chr Alternative names (a character vector), Default: character(0)
#' @param set_idx_1L_int Set index (an integer vector of length one), Default: integer(0)
#' @param with_1L_chr With (a character vector of length one), Default: 'mnl_mlogit_mdl'
#' @return Predicted share (a double vector)
#' @rdname predict_mkt_share
#' @export 
#' @importFrom purrr pluck map_dbl
#' @importFrom stats setNames
#' @keywords internal
predict_mkt_share <- function (dce_design_ls, mdls_ls, mdl_params_ls, new_choices_ls, 
    records_ls, altv_nms_chr = character(0), set_idx_1L_int = integer(0), 
    with_1L_chr = "mnl_mlogit_mdl") 
{
    choices_ls <- make_choices_ls(dce_design_ls, new_choices_ls = new_choices_ls, 
        set_idx_1L_int = set_idx_1L_int)
    replace_cards_int <- 1
    new_choices_dfidx <- make_new_choice_ds(choices_ls, dce_design_ls = dce_design_ls, 
        mdl_params_ls = mdl_params_ls, replace_cards_int = replace_cards_int, 
        records_ls = records_ls)
    if (identical(altv_nms_chr, character(0))) {
        altv_nms_chr <- dce_design_ls$choice_sets_ls$alternatives_chr
    }
    predd_mat <- predict(mdls_ls %>% purrr::pluck(with_1L_chr), 
        newdata = new_choices_dfidx)
    if (!identical(replace_cards_int, integer(0))) {
        indices_int <- matrix(which(new_choices_dfidx$card_id == 
            1), ncol = length(altv_nms_chr))
        predd_mat <- predd_mat[1 + ((matrix(which(new_choices_dfidx$card_id == 
            1), nrow = length(altv_nms_chr)) %>% t())[, 1] - 
            1)/length(altv_nms_chr), ]
    }
    predd_share_dbl <- 1:ncol(predd_mat) %>% purrr::map_dbl(~mean(predd_mat[, 
        .x])) %>% stats::setNames(altv_nms_chr)
    return(predd_share_dbl)
}
