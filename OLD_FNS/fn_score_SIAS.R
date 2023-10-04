#' @title calculate_sias_totals
#' @description FUNCTION_DESCRIPTION
#' @param responses PARAM_DESCRIPTION
#' @param itm_prefix_1L_chr PARAM_DESCRIPTION
#' @param drvd_var_prefix_1L_chr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{setops}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#'  \code{\link[purrr]{map}}
#' @rdname calculate_sias_totals
#' @export
#' @importFrom dplyr mutate_at vars setdiff starts_with matches intersect rename_at contains mutate select
#' @importFrom purrr map_dbl
calculate_sias_totals <- function(responses,
                       itm_prefix_1L_chr,
                       drvd_var_prefix_1L_chr){
  der_suffix <- paste0("_",drvd_var_prefix_1L_chr)
  new_sias_prefix <- paste0(drvd_var_prefix_1L_chr,"_",itm_prefix_1L_chr)
  responses<-  responses %>%
    dplyr::mutate_at(dplyr::vars(dplyr::setdiff(dplyr::starts_with(itm_prefix_1L_chr), dplyr::matches('_5$|_9$|_11$'))),
                     .funs = list(der = ~ purrr::map_dbl(.,~ switch(as.numeric(.x),0,1,2,3,4) ))) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::intersect(dplyr::starts_with(itm_prefix_1L_chr), dplyr::matches('_5$|_9$|_11$'))),
                     .funs = list(der = ~ purrr::map_dbl(.,~ switch(as.numeric(.x),4,3,2,1,0) ))) %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains(der_suffix)), list( ~paste(drvd_var_prefix_1L_chr, gsub(der_suffix, "", .), sep = "_")))
  responses %>%
    dplyr::mutate(der_SIAS_TOT = rowSums(responses %>% dplyr::select(dplyr::starts_with(new_sias_prefix))))
}
