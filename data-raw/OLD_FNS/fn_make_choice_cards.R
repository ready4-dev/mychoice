## Choice Card Functions
make_block_choice_tbs_ls <- function(block_ind,
                                     choices_tb){
  purrr::map(block_ind,
             ~ dplyr::filter(choices_tb, startsWith(Choice, paste0("set",.x,"."))))
}
make_choice_card <- function(choice_card_sng_tb){
  formatted_tb <- t(choice_card_sng_tb) %>%
    tibble::as_tibble(rownames = "Attribute") %>%
    dplyr::filter(Attribute != "Choice") %>%
    dplyr::rename(`Social Anxiety App 1` = V1,
                  `Social Anxiety App 2` = V2)
  row_names <- formatted_tb %>% dplyr::pull(Attribute)
  formatted_tb <- formatted_tb %>% dplyr::select(-Attribute)
  formatted_tb <- formatted_tb %>%
    as.data.frame()
  rownames(formatted_tb) <- row_names

  formatted_tb %>%
    knitr::kable(escape = F) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "left") %>%
    kableExtra::column_spec(1,bold = T, border_right = T) %>%
    kableExtra::column_spec(2:3,
                            # width = "40em",
                            color = "black", border_right = T)
}
make_one_block_choice_cards_ls <- function(block_choice_tbs_ls){
  purrr::map(1:length(block_choice_tbs_ls),
             ~ make_choice_card(block_choice_tbs_ls %>% purrr::pluck(.x)))
}
save_one_block_choice_cards <- function(block_choice_cards_ls,
                                        save_path_stub,
                                        output_type = ".png"){

  purrr::walk2(block_choice_cards_ls,
               1:length(block_choice_cards_ls),
               ~ save_choice_card_as(.x,
                                     .y,
                                     save_path_stub = save_path_stub,
                                     output_type = output_type)
  )
}
save_choice_card_as <- function(choice_kab,
                                choice_nbr,
                                save_path_stub,
                                output_type){
  file_path = paste0(save_path_stub,
                     "/choice_",
                     choice_nbr,
                     output_type)
  if(output_type==".png"){
    kableExtra::as_image(choice_kab,
                         file = file_path)
  }else{
    kableExtra::save_kable(choice_kab, file = file_path, self_contained = F)
  }
}
export_choice_cards <- function(survey_features_ls,
                                no_app_optout_ls,
                                output_dir){
  ## 12. Translate design matrix into survey choices
  survey_ls <- idefix::Decode(des = no_app_optout_ls$design,
                              lvl.names = survey_features_ls$lvl_names,
                              coding = survey_features_ls$c_type,
                              c.lvls = survey_features_ls$con_lvls,
                              alt.cte = survey_features_ls$alt_cte,
                              n.alts = survey_features_ls$n_alts,
                              no.choice = survey_features_ls$no_choice_idx)
  saveRDS(survey_ls,paste0(output_dir,"/survey_ls.rds"))
  ## 13. Reformat table of survey choice sets, dropping opt-out rows
  ## Note: This needs to be abstracted before this function can be used for other survey designs.
  choices_tb <- tibble::as_tibble(survey_ls$design,
                                  rownames = "Choice") %>%
    dplyr::rename(Outcomes = V1,
                  `Information sharing` = V2,
                  Social = V3,
                  Endorsers = V4,
                  Cost = V5) %>%
    dplyr::filter(!startsWith(Choice, "no"))
  saveRDS(choices_tb,paste0(output_dir,"/choices_tb.rds"))
  ## 14. Randomly sample from choice set table to create index numbers for two blocks
  block_1_ind <- sample(1:survey_features_ls$n_sets,survey_features_ls$n_sets/survey_features_ls$n_blocks) %>% sort()
  block_2_ind <- setdiff(1:survey_features_ls$n_sets,block_1_ind)
  ## 15. Create list of choice sets for each block
  blocks_choice_tbs_ls_ls <- purrr::map(list(block_1_ind,
                                             block_2_ind),
                                        ~ make_block_choice_tbs_ls(.x,choices_tb))
  ## 16. Create choice cards for each block
  choice_cards_by_block_ls <- purrr::map(blocks_choice_tbs_ls_ls,
                                         ~ make_one_block_choice_cards_ls(.x))
  ## 17. Save choice cards
  purrr::walk2(choice_cards_by_block_ls,
               paste0(output_dir,"/block_",c(1:length(choice_cards_by_block_ls))),
               ~ save_one_block_choice_cards(.x,
                                             .y,
                                             output_type = ".html"))
  list(survey_ls = survey_ls,
       choices_tb = choices_tb,
       blocks_choice_tbs_ls_ls = blocks_choice_tbs_ls_ls,
       choice_cards_by_block_ls = choice_cards_by_block_ls)

}
