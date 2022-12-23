#' Reorder design matrix
#' @description reorder_design_mat() is a Reorder function that reorders an object to conform to a pre-specified schema. Specifically, this function implements an algorithm to reorder design matrix. The function returns Reordered (a matrix).
#' @param design_mat Design (a matrix)
#' @param block_indcs_ls Block indices (a list)
#' @param choice_sets_ls Choice sets (a list)
#' @return Reordered (a matrix)
#' @rdname reorder_design_mat
#' @export 
#' @importFrom purrr map flatten_int flatten_chr
#' @keywords internal
reorder_design_mat <- function (design_mat, block_indcs_ls, choice_sets_ls) 
{
    alternatives_1L_int <- length(choice_sets_ls$alternatives_chr)
    active_1L_int <- alternatives_1L_int - choice_sets_ls$opt_out_1L_lgl
    mat_row_nms_chr <- purrr::map(paste0("set", block_indcs_ls %>% 
        purrr::flatten_int(), ".alt"), ~c(paste0(.x, 1:active_1L_int), 
        ifelse(choice_sets_ls$opt_out_1L_lgl, "no.choice", length(choice_sets_ls$alternatives_chr)))) %>% 
        purrr::flatten_chr()
    reordered_mat <- design_mat[mat_row_nms_chr, ]
    return(reordered_mat)
}
