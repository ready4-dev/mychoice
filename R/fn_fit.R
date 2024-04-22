#' Fit choice model
#' @description fit_choice_mdl() is a Fit function that fits a model of a specified type to a dataset. Specifically, this function implements an algorithm to fit choice model. The function returns Model (a model).
#' @param dce_design_ls Discrete choice experiment design (a list)
#' @param records_ls Records (a list)
#' @param mdl_params_ls Model parameters (a list)
#' @param return_1L_chr Return (a character vector of length one), Default: 'mnl'
#' @param correlation_1L_lgl Correlation (a logical vector of length one), Default: F
#' @param formula_env Formula (an environment), Default: new.env(parent = globalenv())
#' @param halton_xx Halton (an output object of multiple potential types), Default: NA
#' @param indl_predrs_chr Individual predictors (a character vector), Default: 'NA'
#' @param mvar_ls Variables that enter the mean of random parameters (a list), Default: NULL
#' @param nbr_of_clss_1L_int Number of classes (an integer vector of length one), Default: 2
#' @param smnl_mdl Scaled Multinomial Logit Model (a model), Default: NULL
#' @param use_mlogit_pkg_1L_lgl Use mlogit package package (a logical vector of length one), Default: F
#' @param ... Additional arguments
#' @return Model (a model)
#' @rdname fit_choice_mdl
#' @export 
#' @importFrom purrr map_lgl map map2 pluck
#' @importFrom stats coef setNames
#' @importFrom mlogit mlogit
#' @importFrom gmnl gmnl
#' @importFrom stringr str_squish
#' @keywords internal
fit_choice_mdl <- function (dce_design_ls, records_ls, mdl_params_ls, return_1L_chr = "mnl", 
    correlation_1L_lgl = F, formula_env = new.env(parent = globalenv()), 
    halton_xx = NA, indl_predrs_chr = NA_character_, mvar_ls = NULL, 
    nbr_of_clss_1L_int = 2L, smnl_mdl = NULL, use_mlogit_pkg_1L_lgl = F, 
    ...) 
{
    args_ls <- list(...)
    if (is.na(indl_predrs_chr[1]) & !identical(indl_predrs_chr, 
        character(0))) {
        indl_predrs_chr <- if (return_1L_chr %in% c("mnl", "mixl", 
            "gmnl")) {
            character(0)
        }
        else {
            make_candidate_predrs_chr(mdl_params_ls$candidate_predrs_tb, 
                as_selection_ls_1L_lgl = mdl_params_ls$as_selection_ls_1L_lgl, 
                ds_tb = records_ls$ds_tb, concepts_chr = mdl_params_ls$concepts_chr, 
                types_chr = mdl_params_ls$types_chr)
        }
    }
    f_fml <- make_choice_mdlng_fml(dce_design_ls$choice_sets_ls, 
        candidate_predrs_chr = indl_predrs_chr, choice_var_nm_1L_chr = records_ls$choice_var_nm_1L_chr, 
        cost_var_nm_1L_chr = records_ls$cost_var_nm_1L_chr, formula_env = formula_env, 
        opt_out_var_nm_1L_chr = records_ls$opt_out_var_nm_1L_chr, 
        return_1L_chr = return_1L_chr, use_mlogit_pkg_1L_lgl = use_mlogit_pkg_1L_lgl, 
        wtp_mdl_1L_lgl = !is.null(smnl_mdl))
    random_params_xx <- if (return_1L_chr %in% c("mnl", "lc", 
        "smnl")) {
        NULL
    }
    else {
        mdl_params_ls$random_params_chr
    }
    if (return_1L_chr == "gmnl" & !is.null(smnl_mdl)) {
        random_params_xx <- random_params_xx[names(random_params_xx) != 
            records_ls$cost_var_nm_1L_chr]
    }
    if (return_1L_chr == "smnl" | (return_1L_chr == "gmnl" & 
        !is.null(smnl_mdl))) {
        if (return_1L_chr == "smnl") {
            fixed_lgl <- c(make_choice_atts(dce_design_ls$choice_sets_ls, 
                opt_out_var_nm_1L_chr = records_ls$opt_out_var_nm_1L_chr) %>% 
                purrr::map_lgl(~ifelse(.x %in% get_atts(dce_design_ls$choice_sets_ls$att_lvls_tb, 
                  "cont"), T, F)), c(T, F))
            start_xx <- c(head(fixed_lgl, -2), F, F) %>% as.integer()
            if (is.null(args_ls$method)) {
                args_ls$method <- "bhhh"
            }
        }
        if (return_1L_chr == "gmnl") {
            coefs_dbl <- stats::coef(smnl_mdl)
            cost_idx_1L_int <- 1
            start_xx <- c(append(coefs_dbl, c(1), after = cost_idx_1L_int - 
                1), rep(0.1, ifelse(correlation_1L_lgl, (0.5 * 
                length(random_params_xx) * (length(random_params_xx) + 
                1)), length(coefs_dbl) - 1)), 0.1, 0)
            fixed_lgl <- rep(F, length(start_xx))
            fixed_lgl[cost_idx_1L_int] <- T
            fixed_lgl[length(start_xx)] <- T
            if (is.null(args_ls$method)) {
                args_ls$method <- "bfgs"
            }
        }
        args_ls <- append(args_ls, list(fixed = fixed_lgl, start = start_xx))
    }
    if (((return_1L_chr %in% c("mm", "smnl")) | (return_1L_chr == 
        "gmnl" & !is.null(smnl_mdl))) & is.null(args_ls$iterlim)) {
        args_ls$iterlim <- 500
    }
    panel_xx <- if (return_1L_chr == "mnl") {
        F
    }
    else {
        T
    }
    R_int <- if (return_1L_chr == "smnl") {
        1
    }
    else {
        mdl_params_ls$draws_1L_int
    }
    data_xx <- if (!return_1L_chr %in% c("lc", "smnl", "gmnl") | 
        (return_1L_chr == "gmnl" & is.null(smnl_mdl))) {
        records_ls$ds_dfidx
    }
    else {
        if (return_1L_chr == c("lc")) {
            make_choice_mdlng_ds(case_choices_mat = records_ls$case_choices_mat, 
                candidate_predrs_tb = mdl_params_ls$candidate_predrs_tb, 
                card_id_var_nm_1L_chr = records_ls$card_id_var_nm_1L_chr, 
                choice_sets_ls = dce_design_ls$choice_sets_ls, 
                ds_tb = records_ls$ds_tb, person_card_uid_var_nm_1L_chr = records_ls$person_card_uid_var_nm_1L_chr, 
                person_uid_var_nm_1L_chr = records_ls$person_uid_var_nm_1L_chr, 
                concepts_chr = character(0), return_1L_chr = "mlogit.data", 
                types_chr = character(0), as_selection_ls_1L_lgl = F)
        }
        else {
            ds_dfidx <- records_ls$ds_dfidx
            ds_dfidx[, records_ls$cost_var_nm_1L_chr] <- ds_dfidx[, 
                records_ls$cost_var_nm_1L_chr] * -1
            ds_dfidx
        }
    }
    args_ls <- append(list(formula = f_fml, data = data_xx, correlation = correlation_1L_lgl, 
        model = return_1L_chr, haltons = halton_xx, mvar = mvar_ls, 
        panel = panel_xx, Q = nbr_of_clss_1L_int, R = R_int, 
        ranp = random_params_xx), args_ls)
    if (use_mlogit_pkg_1L_lgl) {
        args_ls$halton <- args_ls$haltons
        args_ls$rpar <- args_ls$ranp
        args_ls$ranp <- NULL
        args_ls$haltons <- NULL
        args_ls$model <- NULL
        args_ls$mvar <- NULL
        args_ls$Q <- NULL
        model_mdl <- mlogit::mlogit(formula = f_fml, data = args_ls$data, 
            correlation = args_ls$correlation, halton = args_ls$halton, 
            panel = args_ls$panel, R = args_ls$R, rpar = args_ls$rpar, 
            ...)
    }
    else {
        if (!is.null(args_ls$fixed)) {
            model_mdl <- do.call(gmnl::gmnl, args_ls)
        }
        else {
            model_mdl <- gmnl::gmnl(formula = f_fml, data = args_ls$data, 
                correlation = args_ls$correlation, model = args_ls$model, 
                haltons = args_ls$haltons, mvar = args_ls$mvar, 
                panel = args_ls$panel, Q = args_ls$Q, R = args_ls$R, 
                ranp = args_ls$ranp, ...)
        }
    }
    names_chr <- setdiff(intersect(names(model_mdl$call), names(args_ls)), 
        "data")
    new_call_ls <- names_chr %>% purrr::map(~if (is.character(args_ls[[.x]])) {
        args_ls[[.x]]
    }
    else {
        paste0(deparse(args_ls[[.x]]), collapse = "") %>% stringr::str_squish()
    }) %>% stats::setNames(names_chr)
    new_call_ls <- append(new_call_ls, list(data = "ds_tb"), 
        after = 1)
    names_chr <- names(new_call_ls)
    new_call_ls <- names(model_mdl$call) %>% purrr::map2(model_mdl$call, 
        ~{
            if (.x %in% names_chr) {
                return_xx <- new_call_ls[[.x]]
                if (is.character(return_xx) & length(return_xx) == 
                  1 & .x != "model") 
                  return_xx <- return_xx %>% str2lang()
            }
            else {
                return_xx <- if (.y %>% deparse() %>% purrr::pluck(1) %>% 
                  startsWith("args_ls")) {
                  "NULL" %>% str2lang()
                }
                else {
                  .y
                }
            }
            return_xx
        }) %>% stats::setNames(names(model_mdl$call)) %>% as.call()
    model_mdl$call <- new_call_ls
    return(model_mdl)
}
