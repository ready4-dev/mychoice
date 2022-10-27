make_areas_lup <- function(X_Ready4useRepos = ready4use::Ready4useRepos(),
                           area_types_chr = c("POA","SOS"),
                           consent_1L_chr = "",
                           fl_nm_1L_chr = "geo_import",
                           matches_chr = character(0),
                           match_var_nm_1L_chr = "POA_CODE16",
                           repo_fl_fmt_1L_chr = ".tab",
                           save_fmt_1L_chr = ".csv",
                           save_type_1L_chr = "original",
                           write_to_1L_chr = character(0),
                           year_1L_int = 2016){
  if(identical(X_Ready4useRepos,ready4use::Ready4useRepos()))
    X <- ready4use::Ready4useRepos(dv_nm_1L_chr = "readyforwhatsnext",
                                   dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/JHSCDJ",
                                   dv_server_1L_chr = "dataverse.harvard.edu")
  if(identical(write_to_1L_chr, character(0)))
    write_to_1L_chr <- tempdir()
  path_1L_chr <- make_local_path_to_dv_data(save_dir_path_1L_chr = write_to_1L_chr,
                                            fl_nm_1L_chr = fl_nm_1L_chr,
                                            save_fmt_1L_chr = save_fmt_1L_chr)
  write_dv_fl_to_loc(ds_ui_1L_chr = X@dv_ds_nm_1L_chr,
                     fl_nm_1L_chr = fl_nm_1L_chr,
                     repo_fl_fmt_1L_chr = repo_fl_fmt_1L_chr,
                     save_type_1L_chr = save_type_1L_chr,
                     server_1L_chr = X@dv_server_1L_chr,
                     dest_path_1L_chr = path_1L_chr,
                     consent_1L_chr = consent_1L_chr)
  geometries_tb <- read.csv(path_1L_chr) %>% dplyr::filter(year == year_1L_int,
                                                           area_type %in% area_types_chr) %>%
    dplyr::arrange(area_type) %>%
    dplyr::select(area_type,uid,download_url,file_type,file_name,inc_file_main)
  geometries_ls <- purrr::pmap(geometries_tb,
                               ~{
                                 url_1L_chr <- ..3
                                 path_1L_chr <- paste0(write_to_1L_chr,
                                                       "/",..5)
                                 if(!consent_1L_chr %in% c("Y", "N")){
                                   consent_1L_chr <- ready4::make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write the file ",
                                                                                              fl_nm_1L_chr,
                                                                                              " to ",
                                                                                              write_to_1L_chr),
                                                                         options_chr = c("Y", "N"),
                                                                         force_from_opts_1L_chr = T)
                                 }
                                 if(consent_1L_chr %in% c("Y")){
                                   utils::download.file(url_1L_chr,
                                                        destfile = path_1L_chr,
                                                        mode = 'wb')
                                   message(paste0("New file created in ",
                                                  write_to_1L_chr,
                                                  " :\n",
                                                  fl_nm_1L_chr))
                                 }
                                 paths_chr <- unzip(path_1L_chr,
                                                    exdir = write_to_1L_chr)
                                 sf::st_read(paths_chr[paths_chr %>% endsWith(".shp")])
                               }) %>%
    stats::setNames(geometries_tb$area_type)
  if(!identical(matches_chr, character(0)))
    geometries_ls[[area_types_chr[1]]] <- geometries_ls[[area_types_chr[1]]] %>% dplyr::filter(!!rlang::sym(match_var_nm_1L_chr) %in% matches_chr)
  geometries_ls$intersected_sf <- sf::st_intersection(geometries_ls[[area_types_chr[1]]],geometries_ls[[area_types_chr[2]]])
  duplicated_lgl <- duplicated(geometries_ls$intersected_sf[[match_var_nm_1L_chr]])
  duplicated_chr <- geometries_ls$intersected_sf[[match_var_nm_1L_chr]][duplicated_lgl] %>% unique() %>% sort()
  areas_lup <- geometries_ls$intersected_sf %>% dplyr::filter(!!rlang::sym(match_var_nm_1L_chr) %in% duplicated_chr) %>%
    dplyr::mutate(Area_Unit = sf::st_area(.) %>% units::set_units(km^2)) %>%
    dplyr::group_by(!!rlang::sym(match_var_nm_1L_chr)) %>%
    dplyr::mutate(Max_Group = max(Area_Unit)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Main_Unit = dplyr::case_when(Area_Unit == Max_Group ~ T,
                                               T ~ F)) %>%
    dplyr::arrange(!!rlang::sym(match_var_nm_1L_chr)) %>%
    dplyr::filter(Main_Unit) %>%
    dplyr::select(-c(Max_Group, Area_Unit, Main_Unit)) %>%
    rbind(geometries_ls$intersected_sf %>% dplyr::filter(!(!!rlang::sym(match_var_nm_1L_chr) %in% duplicated_chr))) %>%
    sf::st_drop_geometry()
  return(areas_lup)
}
make_att_predn_mdls_ls <- function(dce_design_ls,
                                   mdl_params_ls,
                                   records_ls,
                                   return_1L_chr = "mixl",
                                   formula_env = new.env(parent = globalenv()),
                                   significant_at_1L_dbl = 0.05#,...
){
  candidate_choice_predrs_ls <- make_candidate_choice_predrs_ls(dce_design_ls,
                                                                mdl_params_ls = mdl_params_ls,
                                                                records_ls = records_ls)
  return_ls <- purrr::map2(candidate_choice_predrs_ls,
                           names(candidate_choice_predrs_ls),
                           ~ {
                             att_predn_mdl <- fit_choice_mdl(dce_design_ls,
                                                             mdl_params_ls = mdl_params_ls,
                                                             records_ls = records_ls,
                                                             return_1L_chr = return_1L_chr,
                                                             formula_env = formula_env,
                                                             indl_predrs_chr = .x,
                                                             mvar_ls = list(.x) %>% stats::setNames(.y))
                             mdl_summary_ls <- gmnl::getSummary.gmnl(att_predn_mdl)
                             significant_predrs_chr <- intersect(paste0(paste0(.y,"."),.x),
                                                                 names((mdl_summary_ls$coef[,"p"] < significant_at_1L_dbl))[mdl_summary_ls$coef[,"p"]<significant_at_1L_dbl])
                             if(identical(significant_predrs_chr, character(0))){
                               significant_predrs_chr <- NA_character_
                             }else{
                               significant_predrs_chr <- significant_predrs_chr %>% stringi::stri_replace_first_fixed(paste0(.y,"."),"")
                             }
                             list(att_predn_mdl = att_predn_mdl,
                                  significant_predrs_chr = significant_predrs_chr)
                           }) %>%
    stats::setNames(names(candidate_choice_predrs_ls))
  return(return_ls)
}
make_candidate_choice_predrs_ls <- function(dce_design_ls,
                                            mdl_params_ls,
                                            records_ls){
  choice_atts_chr <- make_choice_atts(dce_design_ls$choice_sets_ls,
                                      opt_out_var_nm_1L_chr = records_ls$opt_out_var_nm_1L_chr)
  candidate_predrs_chr <- make_candidate_predrs_chr(mdl_params_ls$candidate_predrs_tb,
                                                    as_selection_ls_1L_lgl = mdl_params_ls$as_selection_ls_1L_lgl,
                                                    ds_tb = records_ls$ds_tb,
                                                    concepts_chr = mdl_params_ls$concepts_chr,
                                                    types_chr = mdl_params_ls$types_chr )
  candidate_choice_predrs_ls <- purrr::map(choice_atts_chr,
                                           ~ candidate_predrs_chr) %>%
    stats::setNames(choice_atts_chr)
  return(candidate_choice_predrs_ls)
}
make_candidate_predrs_chr <- function(candidate_predrs_tb,
                                      concepts_chr = character(0),
                                      ds_tb = NULL,
                                      types_chr = character(0),
                                      as_selection_ls_1L_lgl = F){
  if((length(concepts_chr) != length(types_chr) | (length(types_chr) + length(concepts_chr) == 0) ) | !as_selection_ls_1L_lgl){
    if(!identical(concepts_chr, character(0)))
      candidate_predrs_tb <- candidate_predrs_tb %>% dplyr::filter(concept_chr %in% concepts_chr)
    if(!identical(types_chr, character(0)))
      candidate_predrs_tb <- candidate_predrs_tb %>% dplyr::filter(type_chr %in% types_chr)
    candidate_predrs_chr <- candidate_predrs_tb$short_name_chr
  }else{
    candidate_predrs_chr <- purrr::map2(concepts_chr,
                                        types_chr,
                                        ~ ready4::get_from_lup_obj(candidate_predrs_tb %>% dplyr::filter(concept_chr == .x),
                                                                   match_value_xx = .y,
                                                                   match_var_nm_1L_chr = "type_chr",
                                                                   target_var_nm_1L_chr = "short_name_chr")) %>%
      purrr::flatten_chr()
  }
  if(!is.null(ds_tb))
    candidate_predrs_chr <- candidate_predrs_chr[candidate_predrs_chr %>% purrr::map_lgl(~ds_tb %>% dplyr::pull(!!rlang::sym(.x)) %>% purrr::discard(is.na) %>% unique() %>% length() >1)]
  return(candidate_predrs_chr)
}
make_cards_html_ls <- function(block_choice_tbs_ls){
  purrr::map(1:length(block_choice_tbs_ls),
             ~ make_choice_card_html(block_choice_tbs_ls %>% purrr::pluck(.x)))
}
make_case_choices_mat <- function(ds_tb,
                                  block_idxs_ls,
                                  choice_sets_ls,
                                  design_mat,
                                  choice_vars_pfx_1L_chr = "DCE_B"){
  choice_responses_tb <- make_choice_responses_ds(ds_tb,
                                                  choice_vars_pfx_1L_chr = choice_vars_pfx_1L_chr )
  nbr_of_choices_1L_int <- get_nbr_of_choices(choice_sets_ls)
  all_choices_indcs_ls <-purrr::map(1:nrow(ds_tb),
                                    ~ {
                                      first_idcs_int <- which(!is.na(choice_responses_tb %>%
                                                                       dplyr::slice(.x) %>%
                                                                       as.numeric()))
                                      block_ref_1L_int <- (first_idcs_int[nbr_of_choices_1L_int])/(nbr_of_choices_1L_int)
                                      nbr_of_choice_alts_1L_int <- nbr_of_choices_1L_int*length(choice_sets_ls$alternatives_chr)
                                      ((block_ref_1L_int-1)*nbr_of_choice_alts_1L_int+1):((block_ref_1L_int)*nbr_of_choice_alts_1L_int)
                                    }
  )
  reordered_mat <- reorder_design_mat(design_mat = design_mat,
                                      block_idxs_ls = block_idxs_ls,
                                      choice_sets_ls = choice_sets_ls)
  case_choices_mat <- purrr::map(all_choices_indcs_ls,
                                 ~ reordered_mat[.x,]) %>%
    purrr::reduce(~rbind(.x,.y))
  return(case_choices_mat)
}
make_case_choices_ds <- function(case_choices_mat,
                                 choice_sets_ls,
                                 new_opt_out_var_nm_1L_chr = "opt_out"){
  opt_out_nm_1L_chr <- get_opt_out_var_nm(case_choices_mat = case_choices_mat, choice_sets_ls = choice_sets_ls)
  case_choices_tb <- case_choices_mat %>%
    tibble::as_tibble() %>%
    dplyr::mutate_at(.vars = make_fctr_atts_tmp_var_nms(choice_sets_ls$att_lvls_tb) %>%
                       c(opt_out_nm_1L_chr),
                     as.logical)
  if(!identical(opt_out_nm_1L_chr, character(0)))
    case_choices_tb <- case_choices_tb %>%
    dplyr::rename(!!rlang::sym(new_opt_out_var_nm_1L_chr) := !!rlang::sym(opt_out_nm_1L_chr))
  case_choices_tb <- purrr::reduce(make_cont_atts_rename_ls(choice_sets_ls$att_lvls_tb),
                                   .init = case_choices_tb,
                                   ~ .x %>% dplyr::rename(!!rlang::sym(.y[1]) := !!rlang::sym(.y[2])))
  case_choices_tb <- purrr::reduce(purrr::map2(make_fctr_atts_dummy_var_nms(choice_sets_ls$att_lvls_tb),
                                               make_fctr_atts_tmp_var_nms(choice_sets_ls$att_lvls_tb),
                                               ~ c(.x,.y)),
                                   .init = case_choices_tb,
                                   ~ .x %>% dplyr::rename(!!rlang::sym(.y[1]) := !!rlang::sym(.y[2])))
  return(case_choices_tb)
}
make_choice_atts <- function(choice_sets_ls,
                             opt_out_var_nm_1L_chr = "opt_out"){
  choice_atts_chr <- c(make_fctr_atts_dummy_var_nms(choice_sets_ls$att_lvls_tb),
                       get_atts(choice_sets_ls$att_lvls_tb,"cont"))
  if(choice_sets_ls$opt_out_1L_lgl){
    choice_atts_chr <- c(opt_out_var_nm_1L_chr, choice_atts_chr)
  }
  return(choice_atts_chr)
}
make_choice_card_html <- function(choice_card_tb){
  formatted_tb <- t(choice_card_tb) %>%
    tibble::as_tibble(rownames = "Attribute") %>%
    dplyr::filter(Attribute != "Choice") %>%
    dplyr::rename(`Social Anxiety App 1` = V1,
                  `Social Anxiety App 2` = V2)
  row_names <- formatted_tb %>% dplyr::pull(Attribute)
  formatted_tb <- formatted_tb %>% dplyr::select(-Attribute)
  formatted_tb <- formatted_tb %>%
    as.data.frame()
  rownames(formatted_tb) <- row_names
  card_kbl <- formatted_tb %>%
    knitr::kable(escape = F) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "left") %>%
    kableExtra::column_spec(1,bold = T, border_right = T) %>%
    kableExtra::column_spec(2:3,
                            color = "black", border_right = T)
  return(card_kbl)
}
make_choice_cards <- function(dce_design_ls,
                              block_idxs_ls = list(),
                              seed_1L_int = 1987,
                              set_idx_1L_int = integer(0),
                              transform_att_nms_1L_lgl = T){
  set.seed(seed_1L_int)
  survey_ls <- idefix::Decode(des = dce_design_ls$efnt_dsn_ls[[{if(identical(set_idx_1L_int,integer(0))){
    length(dce_design_ls$efnt_dsn_ls)
  }else{
    set_idx_1L_int
  }}]]$design,
                              lvl.names = make_tfd_lvls_ls(dce_design_ls),
                              coding = get_att_smrys(dce_design_ls,return_1L_chr = "type"),
                              c.lvls = get_lvls(dce_design_ls$choice_sets_ls$att_lvls_tb, return_1L_chr = "cont") %>%
                                unname() %>%
                                purrr::map(~as.numeric(.x)),
                              alt.cte = dce_design_ls$priors_ls[[{if(identical(set_idx_1L_int,integer(0))){
                                length(dce_design_ls$priors_ls)
                              }else{
                                min(set_idx_1L_int,length(dce_design_ls$priors_ls))
                              }}]]$altv_con_int,
                              n.alts = length(dce_design_ls$choice_sets_ls$alternatives_chr),
                              no.choice = dce_design_ls$choice_sets_ls$opt_out_idx_1L_int)
  attributes_chr <- dce_design_ls$choice_sets_ls$att_lvls_tb$attribute_chr %>% unique()
  if(transform_att_nms_1L_lgl){
    attributes_chr <- stringr::str_replace_all(attributes_chr,"_"," ")
  }
  choices_tb <- tibble::as_tibble(survey_ls$design,
                                  rownames = "Choice")
  colnames(choices_tb) <- c("Choice", attributes_chr)
  choices_tb <- choices_tb %>%
    dplyr::filter(!startsWith(Choice, "no"))
  if(identical(block_idxs_ls,list())){
    indices_int <- 1:dce_design_ls$choice_sets_ls$nbr_of_sets_1L_int
    folds_int <- cut(indices_int,
                     breaks = dce_design_ls$choice_sets_ls$nbr_of_blocks_1L_int,
                     labels = FALSE) %>% sample()
    block_idxs_ls <- 1:dce_design_ls$choice_sets_ls$nbr_of_blocks_1L_int %>%
      purrr::map(~indices_int[folds_int==.x])
  }
  choice_cards_tb_ls <- purrr::map(block_idxs_ls,
                                   ~ make_choice_cards_tb_ls(.x,choices_tb))
  choice_cards_html_ls <- purrr::map(choice_cards_tb_ls,
                                     ~ make_cards_html_ls(.x))
  choice_cards_ls <- list(survey_ls = survey_ls,
                          block_idxs_ls = block_idxs_ls,
                          choices_tb = choices_tb,
                          choice_cards_tb_ls = choice_cards_tb_ls,
                          choice_cards_html_ls = choice_cards_html_ls)
  return(choice_cards_ls)
}
make_choice_cards_tb_ls <- function(blocks_int,
                                    choices_tb){
  block_choice_tbs_ls <- purrr::map(blocks_int,
                                    ~ dplyr::filter(choices_tb, startsWith(Choice, paste0("set",.x,"."))))
  return(block_choice_tbs_ls)
}
make_choice_int <- function(ds_tb,
                            choice_sets_ls,
                            choice_vars_pfx_1L_chr ="DCE_B"){
  choice_int <- purrr::map(1:nrow(ds_tb),
                           ~ make_choice_responses_ds(ds_tb,
                                                      choice_vars_pfx_1L_chr = choice_vars_pfx_1L_chr) %>%
                             dplyr::slice(.x) %>%
                             as.numeric() %>%
                             purrr::map(
                               ~ if(is.na(.x)){
                                 NULL
                               }else{
                                 choice_idcs_int <- rep(0,length(choice_sets_ls$alternatives_chr))
                                 choice_idcs_int[.x] <- 1
                                 choice_idcs_int
                               }) %>%
                             unlist()) %>%
    unlist()
  return(choice_int)
}
make_choice_mdlng_ds <- function(case_choices_mat,
                                 candidate_predrs_tb,
                                 choice_sets_ls,
                                 ds_tb,
                                 person_uid_var_nm_1L_chr,
                                 alternative_var_nm_1L_chr = "alternative",
                                 as_selection_ls_1L_lgl = F,
                                 card_id_var_nm_1L_chr = "card_id",
                                 choice_vars_pfx_1L_chr = "DCE_B",
                                 concepts_chr = character(0),
                                 person_card_uid_var_nm_1L_chr = "PERSONcard_id",
                                 return_1L_chr = "dfidx",
                                 types_chr = character(0)){
  case_choices_tb <- make_case_choices_ds(case_choices_mat = case_choices_mat,
                                          choice_sets_ls = choice_sets_ls)
  choice_int <- make_choice_int(ds_tb,
                                choice_sets_ls = choice_sets_ls,
                                choice_vars_pfx_1L_chr = choice_vars_pfx_1L_chr)
  participants_tb <- make_participants_ds(ds_tb,
                                          candidate_predrs_chr = make_candidate_predrs_chr(candidate_predrs_tb,
                                                                                           concepts_chr = concepts_chr,
                                                                                           ds_tb = ds_tb,
                                                                                           types_chr = types_chr,
                                                                                           as_selection_ls_1L_lgl = as_selection_ls_1L_lgl),
                                          choice_sets_ls = choice_sets_ls,
                                          person_uid_var_nm_1L_chr = person_uid_var_nm_1L_chr,
                                          cases_are_responses_1L_lgl = T)

  choice_mdlng_ds_tb <- case_choices_tb %>%
    dplyr::mutate(!!rlang::sym(alternative_var_nm_1L_chr) := rep(choice_sets_ls$alternatives_chr,
                                                                 nrow(ds_tb)*(choice_sets_ls$nbr_of_sets_1L_int / choice_sets_ls$nbr_of_blocks_1L_int))) %>%
    cbind(participants_tb) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(!!rlang::sym(card_id_var_nm_1L_chr) := rep(1:(choice_sets_ls$nbr_of_sets_1L_int / choice_sets_ls$nbr_of_blocks_1L_int),length(choice_sets_ls$alternatives_chr)) %>%
                    sort() %>% rep(nrow(ds_tb))) %>%
    dplyr::mutate(choice = as.logical(choice_int)) %>% #
    dplyr::mutate(!!rlang::sym(person_card_uid_var_nm_1L_chr) := paste0("P", !!rlang::sym(person_uid_var_nm_1L_chr),
                                                                        "C", !!rlang::sym(card_id_var_nm_1L_chr))) %>%
    dplyr::mutate(option_id = rep(LETTERS[1:length(choice_sets_ls$alternatives_chr)],
                                  nrow(case_choices_tb)/length(choice_sets_ls$alternatives_chr)))
  choice_mdlng_ds_tb <- choice_mdlng_ds_tb %>%
    dplyr::select(choice,
                  !!rlang::sym(person_uid_var_nm_1L_chr),
                  option_id,
                  dplyr::everything()) %>%
    dplyr::mutate(option_id = purrr::map_int(option_id, ~ which(LETTERS==.x)))
  choice_mdlng_ds_tb <- choice_mdlng_ds_tb %>%
    dplyr::mutate(chid = rep(1:length(choice_sets_ls$alternatives_chr),nrow(choice_mdlng_ds_tb)/length(choice_sets_ls$alternatives_chr)))
  choice_mdlng_ds_tb <- choice_mdlng_ds_tb %>%
    dplyr::mutate(dplyr::across(c(where(is.logical), -choice),
                                .fns = as.numeric))
  choice_mdlng_ds_tb <- choice_mdlng_ds_tb %>%
    dplyr::mutate(case_uid = 1:dplyr::n())
  if(return_1L_chr == "dfidx"){
    choice_mdlng_xx <- dfidx::dfidx(choice_mdlng_ds_tb,
                                    idx = list(c(person_card_uid_var_nm_1L_chr, person_uid_var_nm_1L_chr)),
                                    choice = "choice",
                                    shape = "long")
    class(choice_mdlng_xx) <- c(class(choice_mdlng_xx), "mlogit.data")
  }else{
    if(return_1L_chr == "mlogit.data"){
      choice_mdlng_xx <- mlogit::mlogit.data(choice_mdlng_ds_tb, # This function is deprecated.
                                             chid.var = person_card_uid_var_nm_1L_chr, # Temporary fix to enable estimation of glm lc models.
                                             alt.var = "option_id",
                                             id.var = person_uid_var_nm_1L_chr,
                                             choice = "choice",
                                             shape = "long")
    }else{
      choice_mdlng_xx <- choice_mdlng_ds_tb
    }
  }
  return(choice_mdlng_xx)
}
make_choice_mdlng_fml <- function(choice_sets_ls,
                                  candidate_predrs_chr = character(0),
                                  choice_atts_chr = character(0),
                                  choice_var_nm_1L_chr = "choice",
                                  cost_var_nm_1L_chr = "Cost",
                                  formula_env = new.env(parent = globalenv()),
                                  opt_out_var_nm_1L_chr = "opt_out",
                                  return_1L_chr = "mnl",
                                  use_mlogit_pkg_1L_lgl = F,
                                  wtp_mdl_1L_lgl = F){
  if(identical(choice_atts_chr, character(0)))
    choice_atts_chr <- make_choice_atts(choice_sets_ls,
                                        opt_out_var_nm_1L_chr = opt_out_var_nm_1L_chr)
  if(return_1L_chr=="gmnl" & wtp_mdl_1L_lgl){
    choice_atts_chr <-  c(cost_var_nm_1L_chr, choice_atts_chr[choice_atts_chr != cost_var_nm_1L_chr])
  }
  fml_1L_chr <- paste0(choice_var_nm_1L_chr,
                       " ~ ",
                       paste0(choice_atts_chr, collapse= " + "),
                       ifelse(return_1L_chr %in% c("lc","mm","smnl") | (return_1L_chr == "gmnl" & wtp_mdl_1L_lgl),
                              "  | 0 | 0",
                              ifelse(use_mlogit_pkg_1L_lgl,"  | 0","  | -1")
                       ))
  if((return_1L_chr %in% c("lc","mm","smnl") | (return_1L_chr == "gmnl" & wtp_mdl_1L_lgl)) & (!is.na(candidate_predrs_chr[1])|identical(candidate_predrs_chr,character(0))) | (return_1L_chr == "mixl" & !use_mlogit_pkg_1L_lgl & (!is.na(candidate_predrs_chr[1])|!identical(candidate_predrs_chr,character(0))))){
    fml_1L_chr <- paste0(fml_1L_chr,
                         " | 0 | ",
                         paste0(candidate_predrs_chr, collapse= " + "),
                         ifelse(return_1L_chr %in% c("lc","mm","smnl","gmnl","mixl"),
                                ifelse(identical(candidate_predrs_chr, character(0)),
                                       " 1",
                                       ""),
                                " - 1"))
  }
  formula_fml <- Formula::Formula(as.formula(fml_1L_chr, env = formula_env))
  return(formula_fml)
}
make_choice_responses_ds <- function(ds_tb,
                                     choice_vars_pfx_1L_chr = "DCE_B"){
  choice_responses_tb <- ds_tb %>%
    dplyr::select(names(ds_tb)[names(ds_tb) %>% startsWith(choice_vars_pfx_1L_chr)])
  return(choice_responses_tb)
}
make_choice_smrys <- function(dce_design_ls,
                              mdl_params_ls,
                              records_ls,
                              text_size_1L_int = 14,
                              wrapping_1L_int = 40){
  ds_tb <- make_choice_mdlng_ds(case_choices_mat = records_ls$case_choices_mat,
                                candidate_predrs_tb = mdl_params_ls$candidate_predrs_tb,
                                card_id_var_nm_1L_chr = records_ls$card_id_var_nm_1L_chr,
                                choice_sets_ls = dce_design_ls$choice_sets_ls,
                                ds_tb = records_ls$ds_tb,
                                person_card_uid_var_nm_1L_chr = records_ls$person_card_uid_var_nm_1L_chr,
                                person_uid_var_nm_1L_chr = records_ls$person_uid_var_nm_1L_chr,
                                concepts_chr = character(0),
                                types_chr = character(0),
                                as_selection_ls_1L_lgl = F,
                                return_1L_chr = "df")
  fctr_atts_ls <- get_fctr_atts_dummy_var_nms(dce_design_ls$choice_sets_ls$att_lvls_tb)
  if(!is.null(fctr_atts_ls)){
    fctr_var_nms_ls <- fctr_atts_ls %>% purrr::map2(names(fctr_atts_ls),
                                                    ~ paste0(paste0(.y,"_"),.x))
    choice_by_fctr_atts_ls <- 1:length(fctr_var_nms_ls) %>%
      purrr::map( ~{
        concept_1L_chr <- names(fctr_atts_ls)[.x]
        dummy_vars_chr <- fctr_var_nms_ls[[.x]]
        stubs_chr <- fctr_atts_ls[[.x]]
        new_tb <- ds_tb %>%
          dplyr::select(records_ls$choice_var_nm_1L_chr,
                        tidyselect::all_of(dummy_vars_chr),
                        {
                          if(dce_design_ls$choice_sets_ls$opt_out_1L_lgl){
                            records_ls$opt_out_var_nm_1L_chr
                          }else{
                            character(0)
                          }
                        }) %>%
          dplyr::mutate(!!rlang::sym(concept_1L_chr) := dce_design_ls$choice_sets_ls$att_lvls_tb %>% dplyr::filter(attribute_chr == concept_1L_chr,
                                                                                                                   is.na(dummy_nm_chr)) %>%
                          dplyr::pull(level_chr))
        if(dce_design_ls$choice_sets_ls$opt_out_1L_lgl){
          new_tb <- new_tb %>%
            dplyr::filter(!!rlang::sym(records_ls$opt_out_var_nm_1L_chr) != 1)
        }
        new_tb <- 1:length(stubs_chr) %>%
          purrr::reduce(.init = new_tb,
                        ~ .x %>%
                          dplyr::mutate(!!rlang::sym(concept_1L_chr) := dplyr::case_when(!!rlang::sym(dummy_vars_chr[.y]) == 1 ~ ready4::get_from_lup_obj(dce_design_ls$choice_sets_ls$att_lvls_tb,
                                                                                                                                                          match_value_xx = stubs_chr[.y],
                                                                                                                                                          match_var_nm_1L_chr = "dummy_nm_chr",
                                                                                                                                                          target_var_nm_1L_chr = "level_chr"),
                                                                                         T ~ !!rlang::sym(concept_1L_chr))))
        fml_1L_chr <- paste0(records_ls$choice_var_nm_1L_chr,"~",concept_1L_chr)
        plot_plt <- CGPfunctions::PlotXTabs2(new_tb,
                                             y = !!rlang::sym(records_ls$choice_var_nm_1L_chr),
                                             x = !!rlang::sym(concept_1L_chr),
                                             title = paste0("Choices made by respondents according to ",concept_1L_chr," attribute"))
        plot_plt$layers[[3]] <- NULL
        df_n_label_tb <- plot_plt$data %>% dplyr::group_by(x) %>% dplyr::summarise(N = sum(counts)) %>%
          dplyr::mutate(N = paste0("(n = ", N, ")", sep = ""))
        plot_plt$data$x <- plot_plt$data$x %>% purrr::map_chr(~paste0(.x,
                                                                      " ",
                                                                      ready4::get_from_lup_obj(df_n_label_tb,
                                                                                               match_value_xx = .x,
                                                                                               match_var_nm_1L_chr = "x",
                                                                                               target_var_nm_1L_chr = "N")))
        plot_plt <- plot_plt + ggplot2::theme_void()
        plot_plt <- plot_plt + ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size=text_size_1L_int)) + ggplot2::aes(stringr::str_wrap(x,wrapping_1L_int))
        list(table_tbl = stats::xtabs(eval(parse(text = fml_1L_chr)), new_tb),
             plot_plt = plot_plt)
      }) %>% stats::setNames(names(fctr_atts_ls))
  }else{
    choice_by_fctr_atts_ls <- NULL
  }
  cont_vars_chr <- setdiff(make_choice_atts(dce_design_ls$choice_sets_ls), c(fctr_var_nms_ls %>% purrr::flatten_chr(),records_ls$opt_out_var_nm_1L_chr))
  choice_by_cont_vars_ls <- cont_vars_chr %>%
    purrr::map(~{
      new_tb <- ds_tb
      if(dce_design_ls$choice_sets_ls$opt_out_1L_lgl){
        new_tb <- new_tb %>%
          dplyr::filter(!!rlang::sym(records_ls$opt_out_var_nm_1L_chr) != 1)
      }
      new_tb <- new_tb %>%
        dplyr::select(records_ls$choice_var_nm_1L_chr,
                      tidyselect::all_of(.x))
      fml_1L_chr <- paste0(records_ls$choice_var_nm_1L_chr,"~",.x)
      list(table_tble = stats::xtabs(eval(parse(text = fml_1L_chr)), new_tb),
           plot_plt = CGPfunctions::PlotXTabs2(new_tb,
                                               y = !!rlang::sym(records_ls$choice_var_nm_1L_chr),
                                               x = !!rlang::sym(.x),
                                               title = paste0("Choices made by respondents according to ",.x," attribute")))
    }) %>% stats::setNames(cont_vars_chr)
  if(dce_design_ls$choice_sets_ls$opt_out_1L_lgl){
    fml_1L_chr <- paste0(records_ls$choice_var_nm_1L_chr,"~",records_ls$opt_out_var_nm_1L_chr)
    choice_by_opt_out_ls <- list(opt_out = list(table_tbl = stats::xtabs(eval(parse(text = fml_1L_chr)),
                                                                         records_ls$ds_dfidx),
                                                plot_plt = CGPfunctions::PlotXTabs2(ds_tb,
                                                                                    y = !!rlang::sym(records_ls$choice_var_nm_1L_chr),
                                                                                    x = !!rlang::sym(records_ls$opt_out_var_nm_1L_chr),
                                                                                    title = paste0("Choices made by respondents according to ","opt-out"," alternative"))))
  }else{
    choice_by_opt_out_ls <- NULL
  }
   choice_by_atts_ls <- append(choice_by_fctr_atts_ls,
                              append(choice_by_cont_vars_ls, choice_by_opt_out_ls))
  return(choice_by_atts_ls)
}
make_choices_ls <- function(dce_design_ls,
                            block_idx_1L_int = 1L,
                            by_altv_1L_lgl = T,
                            card_idx_1L_int = 1L,
                            new_choices_ls = NULL,
                            set_idx_1L_int = integer(0)){## Check calls
  alternatives_1L_int <- length(dce_design_ls$choice_sets_ls$alternatives_chr)
  active_1L_int <- alternatives_1L_int - dce_design_ls$choice_sets_ls$opt_out_1L_lgl
  start_1L_int <- (dce_design_ls$choice_cards_ls[[{if(identical(set_idx_1L_int,integer(0))){
    length(dce_design_ls$choice_cards_ls)
    }else{
      set_idx_1L_int
    }}]]$block_idxs_ls[[block_idx_1L_int]][card_idx_1L_int]-1)*alternatives_1L_int + 1
  choice_mat <- dce_design_ls$efnt_dsn_ls[[{if(identical(set_idx_1L_int,integer(0))){
    length(dce_design_ls$efnt_dsn_ls)
  }else{
    set_idx_1L_int
  }}]]$design[start_1L_int:(start_1L_int+alternatives_1L_int-1),]
  fctr_atts_tmp_var_nms_chr <- make_fctr_atts_tmp_var_nms(dce_design_ls$choice_sets_ls$att_lvls_tb)
  fctr_atts_dummy_var_nms_chr <- make_fctr_atts_dummy_var_nms(dce_design_ls$choice_sets_ls$att_lvls_tb)
  fctr_atts_chr <- dce_design_ls$choice_sets_ls$att_lvls_tb %>% dplyr::filter(!is.na(dummy_nm_chr)) %>% dplyr::pull(attribute_chr) %>% unique()
  fctr_atts_optns_ls <- fctr_atts_chr %>% purrr::map(~paste0(.x,
                                                             "_",
                                                             ready4::get_from_lup_obj(dce_design_ls$choice_sets_ls$att_lvls_tb,
                                                                                      match_value_xx = .x,
                                                                                      match_var_nm_1L_chr = "attribute_chr",
                                                                                      target_var_nm_1L_chr = "dummy_nm_chr") %>%
                                                               purrr::discard(is.na))) %>%
    stats::setNames(fctr_atts_chr)
  fctr_atts_ls <- purrr::map2(fctr_atts_tmp_var_nms_chr, fctr_atts_dummy_var_nms_chr,
                              ~ {
                                var_nm_1L_chr <- .y
                                choice_mat[,.x][1:active_1L_int] %>% as.logical() %>%
                                  purrr::map_chr(~ifelse(.x,
                                                         var_nm_1L_chr,
                                                         NA_character_))
                              }
  )
  fctr_atts_by_altv_ls <- 1:active_1L_int %>% purrr::map(~{
    idx_1L_int <- .x
    fctr_atts_ls %>% purrr::map_chr(~.x[idx_1L_int]) %>% purrr::discard(is.na)
  })
  fctr_atts_ls <- fctr_atts_optns_ls %>%
    purrr::map2(names(fctr_atts_optns_ls),
                ~{
                  optns_chr <- .x
                  att_nm_1L_chr <- .y
                  fctr_atts_by_altv_ls %>%
                    purrr::map_chr(~{
                      var_nm_1L_chr <- intersect(.x,optns_chr)
                      ifelse(identical(var_nm_1L_chr, character(0)),
                             NA_character_,

                             stringi::stri_replace_first_fixed(var_nm_1L_chr,paste0(att_nm_1L_chr,"_"),"")
                      )
                    })
                })
  cont_atts_tmp_var_nms_chr  <- make_cont_atts_tmp_var_nms(dce_design_ls$choice_sets_ls$att_lvls_tb)
  con_atts_ls <- cont_atts_tmp_var_nms_chr %>%
    purrr::map(~choice_mat[,.x][1:active_1L_int] %>% as.numeric()) %>%
    stats::setNames(make_cont_atts_rename_ls(dce_design_ls$choice_sets_ls$att_lvls_tb) %>% purrr::map_chr(~.x[1]))
  choices_ls <- append(con_atts_ls,fctr_atts_ls)
  if(by_altv_1L_lgl | !is.null(new_choices_ls)){
    choices_ls <- 1:active_1L_int %>%
      purrr::map(~{
        idx_1L_int <- .x
        choices_ls %>% purrr::map(~.x[idx_1L_int])
      }) %>%
      stats::setNames(dce_design_ls$choice_sets_ls$alternatives_chr[1:active_1L_int])
  }
  if(!is.null(new_choices_ls)){
    choices_ls <- new_choices_ls %>%
      stats::setNames(names(choices_ls))
  }
  return(choices_ls)

}
make_cmprsn_tbl <- function(ds_tb,
                            grouping_var_nms_chr,
                            popl_var_nm_1L_chr,
                            sample_var_nm_1L_chr,
                            expected_dbl = numeric(0),
                            is_pc_1L_lgl = T){
  scaling_1L_dbl = ifelse(is_pc_1L_lgl, 100,1)
  cmprsn_tb <- ds_tb %>%
    dplyr::pull(!!rlang::sym(grouping_var_nms_chr[1])) %>%
    table() %>%
    tibble::as_tibble()
  names(cmprsn_tb) <- c(grouping_var_nms_chr[1], "n")
  total_1L_int <- sum(cmprsn_tb$n)
  if(identical(expected_dbl, numeric(0))){
    expected_dbl <- rep((1/nrow(cmprsn_tb))*scaling_1L_dbl, nrow(cmprsn_tb))
  }
  if(length(expected_dbl) %in% c(1,nrow(cmprsn_tb))){
    cmprsn_tb <- cmprsn_tb %>%
      dplyr::mutate(!!rlang::sym(popl_var_nm_1L_chr) := tidyselect::all_of(expected_dbl))
  }else{
    cmprsn_tb <-  tibble::tibble(!!rlang::sym(grouping_var_nms_chr[1]) := tidyselect::all_of(names(expected_dbl)),
                                 !!rlang::sym(popl_var_nm_1L_chr) := tidyselect::all_of(expected_dbl)) %>%
      dplyr::left_join(cmprsn_tb) %>%
      dplyr::select(c(!!rlang::sym(grouping_var_nms_chr[1]),n,!!rlang::sym(popl_var_nm_1L_chr))) %>%
      dplyr::mutate(n = n %>% purrr::map_dbl(~ifelse(is.na(.x),0,.x)))
  }
  cmprsn_tb <- cmprsn_tb %>%
    dplyr::mutate(!!rlang::sym(sample_var_nm_1L_chr) := n/total_1L_int *scaling_1L_dbl)
  cmprsn_tb <- cmprsn_tb %>%
    dplyr::rename(!!rlang::sym(grouping_var_nms_chr[2]) := grouping_var_nms_chr[1])
  cmprsn_tb <- add_prpn_cmprsns(cmprsn_tb,
                                ds_tb = ds_tb,
                                grouping_var_nms_chr = grouping_var_nms_chr,
                                popl_var_nm_1L_chr = popl_var_nm_1L_chr,
                                is_pc_1L_lgl = is_pc_1L_lgl)
  cmprsn_tb <- cmprsn_tb %>%
    dplyr::select(c(!!rlang::sym(grouping_var_nms_chr[2]),n,!!rlang::sym(sample_var_nm_1L_chr),CI_95_Lower,CI_95_Upper,!!rlang::sym(popl_var_nm_1L_chr),p,p.signif))
  return(cmprsn_tb)
}
make_cont_atts_tmp_var_nms <- function(att_lvls_tb){
  cont_atts_tmp_var_nms_chr <- purrr::map_chr((1:length(get_atts(att_lvls_tb)))[make_cont_atts_indcs(att_lvls_tb)],
                                              ~ paste0("Var",.x))
  return(cont_atts_tmp_var_nms_chr)
}
make_cont_atts_rename_ls <- function(att_lvls_tb){
  cont_atts_rename_ls <- purrr::map2(get_atts(att_lvls_tb)[make_cont_atts_indcs(att_lvls_tb)],
                                     make_cont_atts_tmp_var_nms(att_lvls_tb),~ c(.x,.y))
  return(cont_atts_rename_ls)
}
make_cont_atts_indcs <- function(att_lvls_tb){
  is_continuous_lgl <- get_atts(att_lvls_tb) %>%
    purrr::map_lgl(~att_lvls_tb %>% dplyr::filter(attribute_chr == .x) %>% dplyr::pull(continuous_lgl) %>% all())
  cont_atts_indcs_int <- (1:length(is_continuous_lgl))[is_continuous_lgl]
  return(cont_atts_indcs_int)
}
make_cut_pnts_cmprsn <- function(ds_tb,
                                 grouping_var_nms_chr,
                                 expected_dbl = numeric(0),
                                 is_pc_1L_lgl = T){
  if(is_pc_1L_lgl){
    popl_var_nm_1L_chr <- "Population (per-cent)"
    sample_var_nm_1L_chr <- "Sample (per-cent)"
  }else{
    popl_var_nm_1L_chr <- "Population (proportion)"
    sample_var_nm_1L_chr <- "Sample (proportion)"
  }
  cmprsn_tb <- make_cmprsn_tbl(ds_tb,
                               grouping_var_nms_chr = grouping_var_nms_chr,
                               popl_var_nm_1L_chr = popl_var_nm_1L_chr,
                               sample_var_nm_1L_chr = sample_var_nm_1L_chr,
                               expected_dbl = expected_dbl,
                               is_pc_1L_lgl = is_pc_1L_lgl)
  return(cmprsn_tb)
}
make_efnt_dsn_mat <- function(dce_design_ls,
                              parallel_1L_lgl = FALSE,
                              pilot_analysis_ls = list(),
                              priors_idx_1L_int = 1L,
                              set_idx_1L_int = integer(0),
                              start_dsn_mat_ls = NULL){
  efnt_dsn_mat <- idefix::Modfed(cand.set = dce_design_ls$cndt_design_mat,
                                 n.sets = dce_design_ls$choice_sets_ls$nbr_of_sets_1L_int,
                                 n.alts = length(dce_design_ls$choice_sets_ls$alternatives_chr),
                                 no.choice = dce_design_ls$choice_sets_ls$opt_out_1L_lgl,
                                 alt.cte = dce_design_ls$priors_ls[[priors_idx_1L_int]]$altv_con_int,
                                 parallel = parallel_1L_lgl,
                                 par.draws = {if(identical(pilot_analysis_ls,list())){dce_design_ls$priors_ls[[priors_idx_1L_int]]$params_xx}else{pilot_analysis_ls[[{if(identical(set_idx_1L_int,integer(0))){
                                   length(pilot_analysis_ls)
                                 }else{
                                   set_idx_1L_int
                                 }}]]$sample}},
                                 start.des = start_dsn_mat_ls)
  return(efnt_dsn_mat)
}
make_fctr_atts_indcs <- function(att_lvls_tb){
  fctr_atts_indcs_int <- setdiff(1:length(get_atts(att_lvls_tb)),make_cont_atts_indcs(att_lvls_tb))
  return(fctr_atts_indcs_int)
}
make_fctr_atts_dummy_var_nms <- function(att_lvls_tb,
                                         flatten_1L_lgl = T){
  new_dummy_att_var_nm_xx <- purrr::map2(get_atts(att_lvls_tb,"fctr"),
                                         get_fctr_atts_dummy_var_nms(att_lvls_tb),
                                         ~paste0(.x,"_",.y))
  if(flatten_1L_lgl){
    new_dummy_att_var_nm_xx <- new_dummy_att_var_nm_xx %>%
      purrr::flatten_chr()
  }
  return(new_dummy_att_var_nm_xx )
}
make_fctr_atts_tmp_var_nms <- function(att_lvls_tb,
                                       flatten_1L_lgl = T,
                                       pfx_1L_chr = "Var"){
  fctr_atts_tmp_var_nms_xx <- purrr::map2(make_fctr_atts_indcs(att_lvls_tb),
                                          make_nbr_of_lvls(att_lvls_tb,"fctr"),
                                          ~ paste0(pfx_1L_chr,.x) %>%
                                            paste0(2:.y)) # Will this naming logic be ok if reference value is not first level?
  if(flatten_1L_lgl){
    fctr_atts_tmp_var_nms_xx <- fctr_atts_tmp_var_nms_xx %>%
      purrr::flatten_chr()
  }
  return(fctr_atts_tmp_var_nms_xx)
}
make_flags_smry_ls <- function(ds_tb,
                               qltv_ax_green_flag_1L_chr,
                               qltv_ax_var_nm_1L_chr,
                               flags_max_1L_int = 0,
                               flags_tot_var_nm_1L_chr = "red_flag_count_int",
                               preprocessing_log_ls = NULL){
  flags_smry_ls <- list(flags_max_1L_int = flags_max_1L_int)
  flags_smry_ls$flags_freq_df <- table(ds_tb %>% dplyr::pull(flags_tot_var_nm_1L_chr)) %>% as.data.frame()
  smry_tb <- ds_tb %>%
    dplyr::mutate(green_flag_qltv_ax_lgl = !!rlang::sym(qltv_ax_var_nm_1L_chr) %>%
                    purrr::map_lgl(~.x == qltv_ax_green_flag_1L_chr)) %>%
    dplyr::mutate(over_rf_threshold_lgl = !!rlang::sym(flags_tot_var_nm_1L_chr) > flags_max_1L_int) %>%
    dplyr::mutate(qltv_force_in_lgl = green_flag_qltv_ax_lgl %>%
                    purrr::map2_lgl(over_rf_threshold_lgl,
                                    ~ ifelse(is.na(.x),
                                             F,
                                             .x & .y))) %>%
    dplyr::mutate(qltv_force_out_lgl =purrr::map2_lgl(red_flag_qltv_ax_lgl,
                                                      red_flag_count_int,
                                                      ~ {
                                                        qual_dbl <- ifelse(is.na(.x),0,.x)
                                                        qual_dbl >= .y & qual_dbl > flags_max_1L_int
                                                      }) )
  smry_tb <- smry_tb %>%
    dplyr::mutate(exluded_by_alg_lgl = ((purrr::map2_dbl(red_flag_count_int,red_flag_qltv_ax_lgl,
                                                         ~ .x - ifelse(is.na(.y),
                                                                       0,
                                                                       as.numeric(.y)))) > flags_max_1L_int) & (qltv_force_in_lgl %>%
                                                                                                                  purrr::map_lgl(~ifelse(is.na(.x),
                                                                                                                                         T,
                                                                                                                                         !.x))))

  smry_tb <- smry_tb %>%
    dplyr::summarise(dplyr::across(c(names(smry_tb)[names(smry_tb) %>% startsWith("red_flag")],over_rf_threshold_lgl, green_flag_qltv_ax_lgl, qltv_force_in_lgl, qltv_force_out_lgl, exluded_by_alg_lgl),
                                   ~sum(.x, na.rm = T)))
  flags_smry_ls$flags_smry_tb <- smry_tb
  if(!is.null(preprocessing_log_ls))
    flags_smry_ls <- append(flags_smry_ls, preprocessing_log_ls)
  return(flags_smry_ls)
}
make_gmnl_mdl_smry <- function(gmnl_mdl){
  gmnl_smry_ls <-  summary(gmnl_mdl)
  gmnl_smry_ls$call$data <- "data"
  gmnl_smry_ls$call[[1]] <- 'gmnl::gmnl'
  gmnl_smry_ls$call$formula <- (gmnl_smry_ls$call$formula %>% as.character())[2]
  return(gmnl_smry_ls)
}
make_mdl_params_ls <- function(candidate_predrs_tb,
                               dce_design_ls,
                               records_ls,
                               as_selection_ls_1L_lgl = F,
                               concepts_chr = character(0),
                               draws_1L_int = 50L,
                               opt_out_dstr_1L_chr = "n",
                               types_chr = character(0)){
  mdl_params_ls <- list(candidate_predrs_tb = candidate_predrs_tb)
  mdl_params_ls$as_selection_ls_1L_lgl <- as_selection_ls_1L_lgl
  mdl_params_ls$concepts_chr <- concepts_chr
  mdl_params_ls$types_chr <- types_chr
  if(!dce_design_ls$choice_sets_ls$opt_out_1L_lgl){
    mdl_params_ls$opt_out_dstr_1L_chr <- character(0)
  }else{
    mdl_params_ls$opt_out_dstr_1L_chr <- opt_out_dstr_1L_chr
  }
  mdl_params_ls$random_params_chr <- make_random_params_chr(dce_design_ls$choice_sets_ls$att_lvls_tb,
                                                            opt_out_dstr_1L_chr = mdl_params_ls$opt_out_dstr_1L_chr,
                                                            opt_out_var_nm_1L_chr = records_ls$opt_out_var_nm_1L_chr)
  mdl_params_ls$draws_1L_int <- draws_1L_int
  return(mdl_params_ls)
}
make_mxd_lgt_mdl_ls <-  function(att_predn_mdls_ls,
                                 dce_design_ls,
                                 mdl_params_ls,
                                 records_ls,
                                 exclude_chr = character(0),
                                 formula_env = new.env(parent = globalenv()),
                                 max_concepts_1L_int = 2L,
                                 min_threshold_1L_int = 1L,
                                 return_1L_chr = "mixl"){
  return_ls <- NULL
  signft_concepts_chr <- get_signft_concepts(att_predn_mdls_ls, mdl_params_ls = mdl_params_ls,
                                             min_threshold_1L_int = min_threshold_1L_int, exclude_chr = exclude_chr)
  if(length(signft_concepts_chr) > 0){
    signft_concepts_tb <- make_signft_concepts_tbl(att_predn_mdls_ls, mdl_params_ls) %>%
      dplyr::filter(concept_chr %in% signft_concepts_chr)
    choice_atts_chr <- signft_concepts_tb$predicts_ls %>% purrr::flatten_chr() %>% unique()
    #candidate_predrs_chr <- signft_concepts_tb$predictors_ls %>% purrr::flatten_chr()
    candidate_predrs_ls <- signft_concepts_chr %>%
      purrr::map(~ intersect(make_candidate_predrs_chr(mdl_params_ls$candidate_predrs_tb,types_chr = mdl_params_ls$types_chr),
                             mdl_params_ls$candidate_predrs_tb %>%
                               dplyr::filter(concept_chr == .x) %>%
                               dplyr::pull(short_name_chr)
      )) %>%
      stats::setNames(signft_concepts_chr)
    combns_ls <- 1:max_concepts_1L_int %>%
      purrr::map(~{
        combns_mat <- utils::combn(signft_concepts_chr,.x)
        1:ncol(combns_mat) %>%
          purrr::map(~combns_mat[,.x])
      }) %>%
      purrr::flatten()
    mvars_ls_ls <- combns_ls %>%
      purrr::map(~{
        concepts_chr <- .x
        predictors_chr <- candidate_predrs_ls[concepts_chr] %>% purrr::flatten_chr()
        atts_chr <- signft_concepts_tb %>%
          dplyr::filter(concept_chr %in% concepts_chr) %>%
          dplyr::pull(predicts_ls) %>%
          purrr::flatten_chr() %>%
          unique() %>%
          intersect(choice_atts_chr)
        candidate_choice_predrs_ls <- purrr::map(atts_chr,
                                                 ~ {
                                                   slimmed_predrs_chr <- intersect(signft_concepts_tb %>%
                                                                                     dplyr::pull(paste0(.x,"_predrs_chr")) %>%
                                                                                     purrr::discard(is.na), predictors_chr)
                                                   if(identical(slimmed_predrs_chr,character(0))){
                                                     character(0)
                                                   }else{
                                                     predictors_chr
                                                   }
                                                 }) %>%
          stats::setNames(atts_chr) %>%
          purrr::compact()
      })
    mxd_lgt_mdl_ls <- purrr::map(mvars_ls_ls,
                                 ~ {
                                   mvar_ls <- .x %>%
                                     purrr::map2(names(.x),
                                                 ~ {
                                                   var_predrs_chr <- .x
                                                   if(identical(.y, records_ls$opt_out_var_nm_1L_chr)){
                                                     var_nms_chr <- .y
                                                   }else{
                                                     params_tb <- dce_design_ls$choice_sets_ls$att_lvls_tb %>%
                                                       dplyr::filter(attribute_chr ==.y)
                                                     if(params_tb$continuous_lgl[1]){
                                                       var_nms_chr <- .y
                                                     }else{
                                                       var_nms_chr <- paste0(.y,"_",dplyr::filter(params_tb,!is.na(dummy_nm_chr)) %>%
                                                                               dplyr::pull(dummy_nm_chr))
                                                     }
                                                   }
                                                   var_nms_chr %>% purrr::map(~var_predrs_chr) %>% stats::setNames(var_nms_chr)
                                                 }) %>% purrr::flatten()
                                   predn_mdl <- fit_choice_mdl(dce_design_ls,
                                                               mdl_params_ls = mdl_params_ls,
                                                               records_ls = records_ls,
                                                               return_1L_chr = return_1L_chr,
                                                               formula_env = formula_env,
                                                               indl_predrs_chr = mvar_ls %>% purrr::flatten_chr() %>% unique(),
                                                               correlation_1L_lgl = T,
                                                               mvar_ls = mvar_ls)
                                   predn_mdl
                                 }) %>%
      stats::setNames(combns_ls %>% purrr::map_chr(~paste0(.x,collapse = "_")))
  }
  return(mxd_lgt_mdl_ls)
}
make_mdl_smry <- function(model_mdl,
                          return_1L_chr = "coefficients"){
  smry_ls <- summary(model_mdl)
  if("gmnl" %in% class(model_mdl)#startsWith(deparse(smry_ls$call)[1],"gmnl")
  ){
    smry_ls <- gmnl::getSummary.gmnl(model_mdl)
    coef_mat <- smry_ls$coef
    perf_tb <- smry_ls$sumstat %>% purrr::discard(is.na) %>% as.data.frame() %>% tibble::rownames_to_column("Statistic") %>%
      tibble::as_tibble() %>% dplyr::rename(Value = ".")
    p_var_nm_1L_chr <- "p"
  }else{
    coef_mat <- smry_ls$CoefTable
    perf_tb <- tibble::tibble(Statistic = c("logLik","AIC", "BIC","N"),
                              Value = c(smry_ls$logLik,AIC(model_mdl),BIC(model_mdl),smry_ls$freq %>% sum()))
    p_var_nm_1L_chr <-"Pr(>|z|)"
  }
  call_1L_chr <- smry_ls$call %>% deparse() %>% paste0(collapse = "") %>% stringr::str_squish()
  coef_tb <- coef_mat %>%  as.data.frame() %>% tibble::rownames_to_column("Parameter") %>%
    tibble::as_tibble() %>% dplyr::mutate(sign = cut(!!rlang::sym(p_var_nm_1L_chr), breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1,Inf),
                                                     labels = c("***", "**", "*", ".","n.s."), right = FALSE))
  if(return_1L_chr == "coefficients")
    return_xx <- coef_tb
  if(return_1L_chr == "performance")
    return_xx <- perf_tb %>%
    dplyr::filter(!is.na(Value))
  if(return_1L_chr == "call")
    return_xx <- call_1L_chr
  return(return_xx)
}
make_nbr_of_lvls <- function(att_lvls_tb,
                             return_1L_chr = "all"){
  nbr_of_lvls_int <- att_lvls_tb$attribute_chr %>% unique() %>%
    purrr::map_int(~ att_lvls_tb %>% dplyr::filter(attribute_chr == .x) %>% dplyr::pull(level_chr) %>% length())
  if(return_1L_chr == "fctr"){
    nbr_of_lvls_int <- nbr_of_lvls_int[make_fctr_atts_indcs(att_lvls_tb)]
  }
  if(return_1L_chr == "cont"){
    nbr_of_lvls_int <- nbr_of_lvls_int[make_cont_atts_indcs(att_lvls_tb)]
  }
  return(nbr_of_lvls_int)
}
make_new_choice_ds <- function(choices_ls,
                               dce_design_ls,
                               mdl_params_ls,
                               records_ls,
                               replace_cards_int = integer(0),
                               return_1L_chr = "dfidx"){
  new_choices_tb <- make_choice_mdlng_ds(alternative_var_nm_1L_chr = records_ls$alternative_var_nm_1L_chr,
                                         case_choices_mat = records_ls$case_choices_mat,
                                         candidate_predrs_tb = mdl_params_ls$candidate_predrs_tb,
                                         card_id_var_nm_1L_chr = records_ls$card_id_var_nm_1L_chr,
                                         choice_sets_ls = dce_design_ls$choice_sets_ls,
                                         ds_tb = records_ls$ds_tb,
                                         person_card_uid_var_nm_1L_chr = records_ls$person_card_uid_var_nm_1L_chr,
                                         person_uid_var_nm_1L_chr = records_ls$person_uid_var_nm_1L_chr,
                                         concepts_chr = character(0),
                                         return_1L_chr = "df",
                                         types_chr = character(0))
  if(!identical(replace_cards_int, integer(0))){
    unchanged_tb <- new_choices_tb %>% dplyr::filter(!(!!rlang::sym(records_ls$card_id_var_nm_1L_chr) %in% replace_cards_int))
    new_choices_tb <- new_choices_tb %>% dplyr::filter(!!rlang::sym(records_ls$card_id_var_nm_1L_chr) %in% replace_cards_int)
  }
  new_choices_tb <- purrr::reduce(1:length(choices_ls),
                                  .init = new_choices_tb,
                                  ~{
                                    ds_tb <- .x
                                    values_ls <- choices_ls[[.y]]
                                    altv_nm_1L_chr <- names(choices_ls)[.y]
                                    purrr::reduce(1:length(values_ls),
                                                  .init = ds_tb,
                                                  ~ {
                                                    att_nm_1L_chr <- names(values_ls)[.y]
                                                    idx_1L_int <- .y
                                                    data_tb <- .x
                                                    if(att_nm_1L_chr %in% (dce_design_ls$choice_sets_ls$att_lvls_tb %>% dplyr::filter(continuous_lgl) %>% dplyr::pull(attribute_chr) %>% unique())){
                                                      data_tb %>%
                                                        dplyr::mutate(!!rlang::sym(att_nm_1L_chr) := !!rlang::sym(records_ls$alternative_var_nm_1L_chr) %>% purrr::map2_dbl(!!rlang::sym(att_nm_1L_chr),
                                                                                                                                                                            ~ ifelse(.x == altv_nm_1L_chr,
                                                                                                                                                                                     as.numeric(values_ls[idx_1L_int]),
                                                                                                                                                                                     .y)))
                                                    }else{
                                                      dummy_var_nms_chr <- get_fctr_atts_dummy_var_nms(dce_design_ls$choice_sets_ls$att_lvls_tb) %>% purrr::pluck(att_nm_1L_chr)
                                                      dummy_var_nms_chr %>%
                                                        purrr::reduce(.init =  data_tb,
                                                                      ~ {
                                                                        var_nm_1L_chr <- paste0(att_nm_1L_chr,"_",.y)
                                                                        .x %>%
                                                                          dplyr::mutate(!!rlang::sym(var_nm_1L_chr) := !!rlang::sym(records_ls$alternative_var_nm_1L_chr) %>% purrr::map2_dbl(!!rlang::sym(var_nm_1L_chr),
                                                                                                                                                                                              ~ ifelse(.x == altv_nm_1L_chr,
                                                                                                                                                                                                       ifelse(identical(paste0(att_nm_1L_chr,"_", as.character(values_ls[idx_1L_int])),
                                                                                                                                                                                                                        var_nm_1L_chr),
                                                                                                                                                                                                              1,
                                                                                                                                                                                                              0),
                                                                                                                                                                                                       .y)
                                                                          ))
                                                                      })
                                                    }
                                                  })
                                  })
  if(!identical(replace_cards_int, integer(0))){
    new_choices_tb <- dplyr::bind_rows(new_choices_tb, unchanged_tb) %>%
      dplyr::arrange(!!rlang::sym(records_ls$person_uid_var_nm_1L_chr), !!rlang::sym(records_ls$card_id_var_nm_1L_chr))
  }
  if(return_1L_chr == "dfidx"){
    new_choices_xx <- dfidx::dfidx(new_choices_tb,
                                   idx = list(c(records_ls$person_card_uid_var_nm_1L_chr, records_ls$person_uid_var_nm_1L_chr)),
                                   choice = "choice",
                                   shape = "long")
  }else{
    new_choices_xx <- new_choices_tb
  }
  return(new_choices_xx)
}
make_participants_ds <- function(ds_tb,
                                 candidate_predrs_chr,
                                 choice_sets_ls,
                                 person_uid_var_nm_1L_chr,
                                 cases_are_responses_1L_lgl = F){
  participants_tb <- ds_tb %>%
    dplyr::select(c(tidyselect::all_of(person_uid_var_nm_1L_chr), tidyselect::all_of(candidate_predrs_chr))) %>%
    dplyr::slice(rep(1:dplyr::n(), each = (choice_sets_ls$nbr_of_sets_1L_int / choice_sets_ls$nbr_of_blocks_1L_int)*length(choice_sets_ls$alternatives_chr)))
  if(!cases_are_responses_1L_lgl){
    participants_tb <- participants_tb %>% dplyr::distinct()
  }
  return(participants_tb)
}
make_pilot_analysis_ls <- function(pilot_ds_tb,
                                   dce_design_ls,
                                   constraints_ls = list(),
                                   choice_var_pfx_1L_chr = "DCE_B",
                                   draws_1L_int = 100L,
                                   seed_1L_int = 1987,
                                   set_idx_1L_int = integer(0)){
  set.seed(seed_1L_int)
  pilot_ds_tb <- pilot_ds_tb %>%
    make_choice_responses_ds(choice_vars_pfx_1L_chr = choice_var_pfx_1L_chr)
  case_choices_mat <- make_case_choices_mat(pilot_ds_tb,
                                            block_idxs_ls = dce_design_ls$choice_cards_ls[[{if(identical(set_idx_1L_int,integer(0))){
                                              length(dce_design_ls$choice_cards_ls)
                                            }else{
                                              set_idx_1L_int
                                            }}]]$block_idxs_ls,
                                            choice_sets_ls = dce_design_ls$choice_sets_ls,
                                            design_mat = dce_design_ls$efnt_dsn_ls[[{if(identical(set_idx_1L_int,integer(0))){
                                              length(dce_design_ls$efnt_dsn_ls)
                                            }else{
                                              set_idx_1L_int
                                            }}]]$design,
                                            choice_vars_pfx_1L_chr = choice_var_pfx_1L_chr)
  choice_int <- make_choice_int(pilot_ds_tb,
                                choice_sets_ls = dce_design_ls$choice_sets_ls,
                                choice_vars_pfx_1L_chr = choice_var_pfx_1L_chr)
  var_nms_chr <- make_case_choices_ds(case_choices_mat,
                                      choice_sets_ls = dce_design_ls$choice_sets_ls) %>%
    names()
  high_bounds_dbl <- rep(Inf, length(var_nms_chr))
  low_bounds_dbl <-- high_bounds_dbl
  low_bounds_dbl <- var_nms_chr %>%
    purrr::map2_dbl(low_bounds_dbl,
                    ~ifelse(.x %in% names(constraints_ls),
                            constraints_ls[[.x]][1],
                            .y))
  high_bounds_dbl <- var_nms_chr %>%
    purrr::map2_dbl(high_bounds_dbl,
                    ~ifelse(.x %in% names(constraints_ls),
                            constraints_ls[[.x]][2],
                            .y))
  pilot_analysis_ls <- idefix::ImpsampMNL(n.draws = draws_1L_int,
                                          prior.mean = dce_design_ls$priors_ls[[{if(identical(set_idx_1L_int,integer(0))){
                                            length(dce_design_ls$priors_ls)
                                          }else{
                                            set_idx_1L_int
                                          }}]]$priors_dbl,
                                          prior.covar = diag(length(dce_design_ls$priors_ls[[{if(identical(set_idx_1L_int,integer(0))){
                                            length(dce_design_ls$priors_ls)
                                          }else{
                                            set_idx_1L_int
                                          }}]]$priors_dbl)),
                                          des = case_choices_mat,
                                          n.alts = length(dce_design_ls$choice_sets_ls$alternatives_chr),
                                          y = choice_int,
                                          lower = low_bounds_dbl,
                                          upper = high_bounds_dbl,
                                          alt.cte = dce_design_ls$priors_ls[[{if(identical(set_idx_1L_int,integer(0))){
                                            length(dce_design_ls$priors_ls)
                                          }else{
                                            set_idx_1L_int
                                          }}]]$altv_con_int)
  return(pilot_analysis_ls)
}
make_priors_ls <- function(dce_design_ls,
                           priors_dbl,
                           draws_1L_int = 10L,
                           seed_1L_int = 1987){
  set.seed(seed_1L_int)
  variance_mat <- diag(length(priors_dbl))
  params_mat <- MASS::mvrnorm(n = draws_1L_int, mu = priors_dbl, Sigma = variance_mat)
  altv_con_int <- rep(0,length(dce_design_ls$choice_sets_ls$alternatives_chr))
  if(dce_design_ls$choice_sets_ls$opt_out_1L_lgl){
    params_xx <- list(matrix(params_mat[,1], ncol = 1), params_mat[,2:12])
    altv_con_int[dce_design_ls$choice_sets_ls$opt_out_idx_1L_int] <- 1
  }else{
    params_xx <- params_mat
  }
  priors_ls <- list(altv_con_int = altv_con_int,
                    params_xx = params_xx,
                    priors_dbl = priors_dbl)
  return(priors_ls)
}
make_random_params_chr <- function(att_lvls_tb,
                                   opt_out_dstr_1L_chr = character(0),
                                   opt_out_var_nm_1L_chr = "opt_out",
                                   return_1L_chr = "all"){
  random_params_tb <- att_lvls_tb %>%
    dplyr::mutate(distribution_chr = dplyr::case_when(distribution_chr == "normal" ~ "n",
                                                      distribution_chr == "log-normal" ~ "l",
                                                      distribution_chr == "truncated normal" ~ "t",
                                                      distribution_chr == "uniform" ~ "u",
                                                      T ~ distribution_chr))
  dummy_params_tb <- dplyr::filter(random_params_tb,!is.na(dummy_nm_chr))
  cont_params_tb <- dplyr::filter(random_params_tb,continuous_lgl) %>%
    dplyr::select(attribute_chr, distribution_chr) %>%
    dplyr::distinct()
  random_params_chr <- character(0)
  if(return_1L_chr %in% c("all", "cont"))
    random_params_chr <- c(random_params_chr,cont_params_tb$distribution_chr %>% stats::setNames(cont_params_tb$attribute_chr))
  if(return_1L_chr %in% c("all", "fctr"))
    random_params_chr <- c(random_params_chr,dummy_params_tb$distribution_chr %>%
                             stats::setNames(paste0(dummy_params_tb$attribute_chr,"_",dummy_params_tb$dummy_nm_chr)))
  if(!identical(opt_out_dstr_1L_chr,character(0)) & return_1L_chr %in% c("all", "opt_out"))
    random_params_chr <- c(random_params_chr,opt_out_dstr_1L_chr %>% stats::setNames(opt_out_var_nm_1L_chr))
  return(random_params_chr)
}
make_records_ls <- function(ds_tb,
                            alternative_var_nm_1L_chr = "alternative",
                            card_id_var_nm_1L_chr = "card_id",
                            case_choices_mat = matrix(),
                            choice_var_nm_1L_chr = "choice",
                            choice_vars_pfx_1L_chr = "DCE_B",
                            cost_var_nm_1L_chr = "Cost",
                            ds_dfidx = NULL,
                            flags_ls = list(),
                            opt_out_var_nm_1L_chr = "opt_out",
                            person_card_uid_var_nm_1L_chr = "PERSONcard_id",
                            person_uid_var_nm_1L_chr = "person_uid"){
  records_ls <- list()
  records_ls$ds_tb <- ds_tb
  records_ls$ds_dfidx <- ds_dfidx
  records_ls$alternative_var_nm_1L_chr <- alternative_var_nm_1L_chr
  records_ls$card_id_var_nm_1L_chr <- card_id_var_nm_1L_chr
  records_ls$case_choices_mat <- case_choices_mat
  records_ls$choice_var_nm_1L_chr <- choice_var_nm_1L_chr
  records_ls$choice_vars_pfx_1L_chr <- choice_vars_pfx_1L_chr
  records_ls$cost_var_nm_1L_chr <- cost_var_nm_1L_chr
  records_ls$flags_ls <- flags_ls
  records_ls$person_card_uid_var_nm_1L_chr <- person_card_uid_var_nm_1L_chr
  records_ls$person_uid_var_nm_1L_chr <- person_uid_var_nm_1L_chr
  records_ls$opt_out_var_nm_1L_chr <- opt_out_var_nm_1L_chr
  return(records_ls)
}
make_tfd_lvls_ls <- function(dce_design_ls){
  lvls_ls <- get_lvls(dce_design_ls$choice_sets_ls$att_lvls_tb)
  lvls_ls[dce_design_ls$cost_att_idx_1L_int] <- list(lvls_ls[[dce_design_ls$cost_att_idx_1L_int]] %>%
                                                       purrr::map_chr(~paste0(dce_design_ls$cost_pfx_1L_chr,.x,dce_design_ls$cost_sfx_1L_chr))) %>%
    stats::setNames(get_lvls(dce_design_ls$choice_sets_ls$att_lvls_tb)[dce_design_ls$cost_att_idx_1L_int] %>% names())
  return(lvls_ls)
}
make_seifa_lup <- function(url_1L_chr = character(0),
                           fl_nm_1L_chr = character(0),
                           col_nms_chr = c("postcode_dbl","score_dbl","percentile_dbl"),
                           consent_1L_chr = "",
                           keep_cols_int = c(1,3,7),
                           sheet_1L_int = 3L,
                           percentile_var_nm_1L_chr = "percentile_dbl",
                           quartile_var_nm_1L_chr = "quartile_dbl",
                           range_1L_chr = "A6:Q2635",
                           write_to_1L_chr = character(0)){
  if(identical(url_1L_chr, character(0)))
    url_1L_chr <- "https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&2033055001%20-%20poa%20indexes.xls&2033.0.55.001&Data%20Cubes&DC124D1DAC3D9FDDCA25825D000F9267&0&2016&27.03.2018&Latest"
  if(identical(fl_nm_1L_chr, character(0)))
    fl_nm_1L_chr <- "2033055001 - poa indexes.xls"
  if(identical(write_to_1L_chr, character(0)))
    write_to_1L_chr <- tempdir()
  path_1L_chr <- paste0(write_to_1L_chr,"/",fl_nm_1L_chr)
  if(!consent_1L_chr %in% c("Y", "N")){
    consent_1L_chr <- ready4::make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write the file ",
                                                               fl_nm_1L_chr,
                                                               " to ",
                                                               write_to_1L_chr),
                                          options_chr = c("Y", "N"),
                                          force_from_opts_1L_chr = T)
  }
  if(consent_1L_chr %in% c("Y")){
    utils::download.file(url_1L_chr,
                         destfile = path_1L_chr,
                         mode = 'wb')
    message(paste0("New file created in ",
                   write_to_1L_chr,
                   " :\n",
                   fl_nm_1L_chr))
  }
  seifa_lup <- readxl::read_xls(path_1L_chr,
                                sheet = sheet_1L_int,
                                range = range_1L_chr) %>%
    dplyr::select(tidyselect::all_of(keep_cols_int))
  colnames(seifa_lup) <- col_nms_chr
  seifa_lup <- seifa_lup %>% dplyr::mutate(!!rlang::sym(quartile_var_nm_1L_chr) := ceiling(!!rlang::sym(percentile_var_nm_1L_chr)/25))
  return(seifa_lup)
}
make_signft_concepts_tbl <- function(att_predn_mdls_ls,
                                     mdl_params_ls){
  signft_concepts_ls <- att_predn_mdls_ls %>%
    purrr::map(~.x$significant_predrs_chr) %>%
    purrr::discard(~is.na(.x[1]))
  atts_tb <- dce_design_ls$choice_sets_ls$att_lvls_tb %>%
    dplyr::mutate(var_nm_chr = dplyr::case_when(is.na(dummy_nm_chr) ~ attribute_chr,
                                                T ~ paste0(paste0(attribute_chr,"_"),dummy_nm_chr))) %>%
    dplyr::filter(var_nm_chr %in% names(signft_concepts_ls)) %>%
    dplyr::select(attribute_chr, var_nm_chr) %>%
    dplyr::distinct()
  if(dce_design_ls$choice_sets_ls$opt_out_1L_lgl){
    if(records_ls$opt_out_var_nm_1L_chr %in% names(signft_concepts_ls)){
      atts_tb <- tibble::add_case(atts_tb,
                                  attribute_chr = records_ls$opt_out_var_nm_1L_chr,
                                  var_nm_chr = records_ls$opt_out_var_nm_1L_chr)
    }
  }
  signft_concepts_tb <- signft_concepts_ls %>%
    purrr::map_dfr(~mdl_params_ls$candidate_predrs_tb %>%
                     dplyr::filter(short_name_chr %in% .x) %>%
                     dplyr::select(concept_chr, type_chr) %>%
                     dplyr::distinct())
  signft_concepts_tb <- signft_concepts_tb %>%
    dplyr::distinct()
  signft_concepts_tb <- signft_concepts_tb %>%
    dplyr::select(-type_chr) %>%
    dplyr::mutate(predicts_ls = list(list()),
                  total_int = NA_integer_,
                  predictors_ls = list(list()))
  signft_concepts_tb[,paste0(atts_tb$attribute_chr %>% unique(),"_predrs_chr")] <- NA_character_
  col_nms_chr <- colnames(signft_concepts_tb)
  grouped_concepts_ls <- signft_concepts_tb$concept_chr %>%
    purrr::map(~dplyr::filter(mdl_params_ls$candidate_predrs_tb,concept_chr == .x)$short_name_chr) %>%
    stats::setNames(signft_concepts_tb$concept_chr)
  grouped_predrs_ls <- atts_tb$attribute_chr %>% unique() %>% purrr::map(~signft_concepts_ls[ready4::get_from_lup_obj(atts_tb,
                                                                                                                      match_value_xx = .x,
                                                                                                                      match_var_nm_1L_chr = "attribute_chr",
                                                                                                                      target_var_nm_1L_chr = "var_nm_chr")] %>%
                                                                           purrr::flatten_chr() %>% unique()) %>%
    stats::setNames(atts_tb$attribute_chr %>% unique())
  signft_concepts_tb <- grouped_concepts_ls %>%
    purrr::map2_dfr(names(grouped_concepts_ls),
                    ~{
                      concept_vars_chr <- .x
                      predrs_ls <- purrr::map(grouped_predrs_ls,
                                              ~ paste0(.x[.x %in% concept_vars_chr]
                                              )
                      ) %>%
                        purrr::compact()
                      slice_tb <- signft_concepts_tb %>%
                        dplyr::filter(concept_chr == .y) %>%
                        dplyr::mutate(total_int = length(predrs_ls),
                                      predictors_ls = predrs_ls %>% purrr::flatten_chr() %>% unique() %>% sort() %>% list())
                      if(!is.null(predrs_ls)){
                        slice_tb <- slice_tb %>%
                          dplyr::mutate(predicts_ls = list(names(predrs_ls)))
                        names(predrs_ls) <- paste0(names(predrs_ls),"_predrs_chr")
                        slice_tb <- dplyr::bind_cols(slice_tb %>%
                                                       dplyr::select(-names(predrs_ls)),
                                                     predrs_ls %>% stringi::stri_list2matrix() %>% tibble::as_tibble(.name_repair = ~ names(predrs_ls)) %>% dplyr::summarise(dplyr::across(dplyr::everything(),~paste0(sort(unique(.x)), collapse = " "))))  %>%
                          dplyr::select(col_nms_chr)
                      }
                      slice_tb
                    })
  signft_concepts_tb <- signft_concepts_tb %>%
    dplyr::arrange(dplyr::desc(total_int), concept_chr)
  return(signft_concepts_tb)
}
make_smry_tb <- function(ds_tb,
                         var_metadata_tb,
                         digits_1L_int = 2L){
  summary_tb <- summarytools::dfSummary(ds_tb %>% dplyr::select(var_metadata_tb$short_name_chr),
                                        round.digits = 3)
  summary_tb <- summary_tb %>% tibble::as_tibble() %>% dplyr::select(-c(No, Valid, Graph, text.graph))
  summary_tb <- summary_tb %>%
    dplyr::mutate(Variable = Variable %>% purrr::map_chr(~sub(pattern = "\\\\\n.*","",.x)),
                  Concept = Variable %>% purrr::map_chr(~ready4::get_from_lup_obj(var_metadata_tb,
                                                                                  match_value_xx = .x,
                                                                                  match_var_nm_1L_chr = "short_name_chr",
                                                                                  target_var_nm_1L_chr = "concept_chr")),
                  Type = Variable %>% purrr::map_chr(~ready4::get_from_lup_obj(var_metadata_tb,
                                                                               match_value_xx = .x,
                                                                               match_var_nm_1L_chr = "short_name_chr",
                                                                               target_var_nm_1L_chr = "type_chr"))) %>%
    dplyr::select(Concept,Variable,Type, `Stats / Values`, `Freqs (% of Valid)`, `Missing`) %>%
    dplyr::filter(Type != "dummy")
  summary_tb <- summary_tb$Concept %>% unique() %>%
    purrr::map_dfr(~{
      concept_1L_chr <- .x
      slimmed_tb <- summary_tb %>% dplyr::filter(Concept == concept_1L_chr)
      purrr::pmap_dfr(slimmed_tb,
                      ~{
                        stat_1L_chr <- ..4
                        stats_chr <- strsplit(stat_1L_chr,"\\\\")[[1]]
                        if(..3 == "continuous"){
                          stats_chr <- c(stats_chr[1],stats_chr[3:4])
                          mmm_chr <- strsplit(stats_chr[2] %>% stringr::str_sub(start = 2)," < ")[[1]]
                          measures_chr <- c("Mean (SD)",
                                            "Median",
                                            "Minimum - Maximum",
                                            "IQR (CV)")
                          values_chr <- c(stats_chr[1] %>% stringr::str_sub(start = 13),
                                          mmm_chr[2],
                                          paste0(mmm_chr[c(1,3)], collapse  = " - "),
                                          stats_chr[3] %>% stringr::str_sub(start = 13))
                          if(values_chr[1]=="alue"){
                            summary_dbl <- summary(ds_tb %>% dplyr::pull(!!rlang::sym(..2))) %>% as.numeric()
                            values_chr <- c(paste0(summary_dbl[4],
                                                   " (",
                                                   sd(ds_tb %>% dplyr::pull(!!rlang::sym(..2))),
                                                   ")"),
                                            summary_dbl[3],
                                            paste0(summary_dbl[1],
                                                   " - ",
                                                   summary_dbl[6]),
                                            paste0(IQR(ds_tb %>% dplyr::pull(!!rlang::sym(..2))),
                                                   " (-)"))
                          }
                        }else{
                          if(..3 %in% c("factor","logical")){
                            indices_int <- seq(2,length(stats_chr),by=2)
                            measures_chr <- stats_chr[indices_int]  %>%
                              purrr::map_chr(~stringr::str_remove(.x,". "))
                            values_chr <- strsplit(..5,"\\\\")[[1]][indices_int]
                            if(..3 == "logical"){
                              measures_chr <- measures_chr %>%
                                stringr::str_replace_all("FALSE",ready4::get_from_lup_obj(var_metadata_tb,
                                                                                          match_value_xx = ..2,
                                                                                          match_var_nm_1L_chr = "short_name_chr",
                                                                                          target_var_nm_1L_chr = "false_value_chr") %>%
                                                           Hmisc::capitalize()) %>%
                                stringr::str_replace_all("TRUE",ready4::get_from_lup_obj(var_metadata_tb,
                                                                                         match_value_xx = ..2,
                                                                                         match_var_nm_1L_chr = "short_name_chr",
                                                                                         target_var_nm_1L_chr = "long_name_chr") %>%
                                                           Hmisc::capitalize())
                            }
                          }
                        }
                        tibble::tibble(Concept = concept_1L_chr %>% Hmisc::capitalize(),
                                       Measure = c(measures_chr, "Missing"),
                                       !!(paste0("n=", nrow(ds_tb))) := c(values_chr,stringr::str_replace(..6,"\\\\\n"," ")),
                                       Units = c(rep(ready4::get_from_lup_obj(var_metadata_tb,
                                                                              match_value_xx = ..2,
                                                                              match_var_nm_1L_chr = "short_name_chr",
                                                                              target_var_nm_1L_chr = "units_chr"),
                                                     length(measures_chr)),
                                                 NA_character_))
                      })

    }) %>%
    dplyr::mutate(Units = dplyr::case_when(is.na(Units) ~ "",
                                           T ~ Units))
  return(summary_tb)
}
make_sos_lup <- function(area_var_nm_1L_chr = "SOS_CODE_2016",
                         fl_nm_1L_chr = "sos_data.zip",
                         path_1L_chr = character(0),
                         url_1L_chr = character(0),
                         write_to_1L_chr = character(0)){
  if(identical(path_1L_chr, character(0)))
    path_1L_chr <- "/2016 Census GCP Section of State for AUST/2016Census_G01_AUS_SOS.csv"
  if(identical(write_to_1L_chr, character(0)))
    write_to_1L_chr <- tempdir()
  if(identical(url_1L_chr, character(0)))
    url_1L_chr <- "https://www.abs.gov.au/census/find-census-data/datapacks/download/2016_GCP_SOS_for_AUS_short-header.zip"
  download.file(url_1L_chr, destfile = paste0(write_to_1L_chr,"/",fl_nm_1L_chr))
  unzip(paste0(write_to_1L_chr,"/",fl_nm_1L_chr), exdir = write_to_1L_chr)
  sos_popl_tb <- read.csv(paste0(write_to_1L_chr,"/",path_1L_chr))
  sos_lup <- sos_popl_tb %>% dplyr::select(!!rlang::sym(area_var_nm_1L_chr),Age_15_19_yr_P,Age_20_24_yr_P, Age_25_34_yr_P) %>%
    dplyr::mutate(der_Age_25_yr_P = Age_25_34_yr_P/10) %>%
    dplyr::select(-Age_25_34_yr_P) %>%
    dplyr::mutate(youth_popl_dbl = rowSums(dplyr::across(where(is.numeric))),
                  der_urban_lgl = !!rlang::sym(area_var_nm_1L_chr) %>% purrr::map_lgl(~{
                    code_1L_chr <- stringr::str_sub(.x,start = -1)
                    ifelse(code_1L_chr %in% c("0","1"),
                           T,
                           ifelse(code_1L_chr %in% c("2","3"),
                                  F,
                                  NA))
                  })) %>%
    dplyr::group_by(der_urban_lgl) %>%
    dplyr::summarise(youth_popl_dbl = sum(youth_popl_dbl)) %>%
    dplyr::mutate(total_youth_popl_dbl = sum(youth_popl_dbl),
                  share_dbl = youth_popl_dbl/total_youth_popl_dbl) %>%
    dplyr::select(der_urban_lgl, share_dbl)
  return(sos_lup)
}
make_wtp_mat <- function(mnl_mdl,
                         cost_var_nm_1L_chr = "Cost",
                         digits_1L_int = 3L,
                         return_1L_chr = "mat"){
  wtp_mat <- gmnl::wtp.gmnl(mnl_mdl,wrt = "Cost")
  wtp_mat[,1] <- wtp_mat[,1]*-1
  if(return_1L_chr == "printed"){
    wtp_xx <- stats::printCoefmat(wtp_mat, digits = digits_1L_int)
  }else{
    wtp_xx <- wtp_mat
  }
  return(wtp_xx)
}
