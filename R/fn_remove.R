#' Remove no choice responses
#' @description remove_no_choice_responses() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove no choice responses. Function argument responses_tb specifies the object to be updated. Argument choice_card_pfx_1L_chr provides the object to be updated. The function returns Responses (a tibble).
#' @param responses_tb Responses (a tibble)
#' @param choice_card_pfx_1L_chr Choice card prefix (a character vector of length one), Default: 'DCE_B'
#' @return Responses (a tibble)
#' @rdname remove_no_choice_responses
#' @export 
#' @importFrom purrr map_lgl
#' @keywords internal
remove_no_choice_responses <- function (responses_tb, choice_card_pfx_1L_chr = "DCE_B") 
{
    responses_tb <- responses_tb[rowSums(is.na(responses_tb[, 
        names(responses_tb)[purrr::map_lgl(names(responses_tb), 
            ~startsWith(.x, choice_card_pfx_1L_chr))]])) != 30, 
        ]
    return(responses_tb)
}
#' Remove red flag cases
#' @description remove_red_flag_cases() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove red flag cases. Function argument data_tb specifies the object to be updated. Argument flags_max_1L_int provides the object to be updated. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param flags_max_1L_int Flags maximum (an integer vector of length one), Default: 0
#' @param flags_tot_var_nm_1L_chr Flags total variable name (a character vector of length one), Default: 'red_flag_count_int'
#' @param qltv_ax_green_flag_1L_chr Qualitative assessment green flag (a character vector of length one), Default: 'Green'
#' @param qltv_ax_var_nm_1L_chr Qualitative assessment variable name (a character vector of length one), Default: character(0)
#' @return Data (a tibble)
#' @rdname remove_red_flag_cases
#' @export 
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @keywords internal
remove_red_flag_cases <- function (data_tb, flags_max_1L_int = 0, flags_tot_var_nm_1L_chr = "red_flag_count_int", 
    qltv_ax_green_flag_1L_chr = "Green", qltv_ax_var_nm_1L_chr = character(0)) 
{
    flags_max_1L_int <- as.integer(flags_max_1L_int)
    if (identical(qltv_ax_var_nm_1L_chr, character(0))) {
        data_tb <- data_tb %>% dplyr::filter(!!rlang::sym(flags_tot_var_nm_1L_chr) <= 
            flags_max_1L_int)
    }
    else {
        data_tb <- data_tb %>% dplyr::filter(!!rlang::sym(flags_tot_var_nm_1L_chr) <= 
            flags_max_1L_int | !!rlang::sym(qltv_ax_var_nm_1L_chr) == 
            qltv_ax_green_flag_1L_chr)
    }
    return(data_tb)
}
