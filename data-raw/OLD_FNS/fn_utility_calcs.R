#' @title convert_from_dummy
#' @description FUNCTION_DESCRIPTION
#' @param choice_att_tb PARAM_DESCRIPTION
#' @param attribute_nm PARAM_DESCRIPTION
#' @param dummy_nms_vec PARAM_DESCRIPTION
#' @param level_nms_vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{slice}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[purrr]{map}}
#' @rdname convert_from_dummy
#' @export
#' @importFrom dplyr mutate select slice
#' @importFrom rlang sym
#' @importFrom purrr map_chr
convert_from_dummy <- function(choice_att_tb,
                               attribute_nm,
                               dummy_nms_vec,
                               level_nms_vec){
  choice_att_tb %>%
    dplyr::mutate(!!rlang::sym(attribute_nm) := purrr::map_chr(1:nrow(choice_att_tb),
                                                               ~ choice_att_tb %>%
                                                                 dplyr::select(dummy_nms_vec) %>%
                                                                 dplyr::slice(.x) %>%
                                                                 unlist() %>%
                                                                 as.vector() %>%
                                                                 which() %>%
                                                                 + 1 %>%
                                                                 level_nms_vec[.] %>%
                                                                 ifelse(identical(.,character(0)),level_nms_vec[1],.)

    ) %>% as.factor())
}

#' @title transform_chr_to_num
#' @description FUNCTION_DESCRIPTION
#' @param col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details Based on https://stackoverflow.com/questions/24129124/how-to-determine-if-a-character-vector-is-a-valid-numeric-or-integer-vector
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname transform_chr_to_num
#' @export

transform_chr_to_num <- function(col) {
  if(suppressWarnings(all(!is.na(as.numeric(as.character(col[!is.na(col)])))))) {
    as.numeric(as.character(col))
  } else {
    col
  }
}

#' @title convert_na_col_to_logical
#' @description FUNCTION_DESCRIPTION
#' @param col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname convert_na_col_to_logical
#' @export

convert_na_col_to_logical <- function(col) {
  if(suppressWarnings(all(is.na(col)))) {
    as.logical(as.character(col))
  } else {
    col
  }
}

#' @title transform_to_excel_date_fmt
#' @description FUNCTION_DESCRIPTION
#' @param date PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[lubridate]{parse_date_time}},\code{\link[lubridate]{as_date}},\code{\link[lubridate]{interval}},\code{\link[lubridate]{period}}
#' @rdname transform_to_excel_date_fmt
#' @export
#' @importFrom lubridate parse_date_time as_date interval int_flip days
transform_to_excel_date_fmt <- function(date){
  lubridate::parse_date_time(date,"ymd HMS") %>%
    lubridate::as_date() %>%
    lubridate::interval(as.Date("1899-12-30")) %>%
    lubridate::int_flip() %/% lubridate::days(1)
}
#' @title get_digits_from_text
#' @description FUNCTION_DESCRIPTION
#' @param string PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details from:: http://stla.github.io/stlapblog/posts/Numextract.html
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_digits_from_text
#' @export

get_digits_from_text <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
}
