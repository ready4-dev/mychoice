launch_survey_preview <- function(dce_design_ls,
                                  block_1L_int = integer(0),
                                  button_txt_1L_chr = character(0),
                                  end_txt_1L_chr = character(0),
                                  intro_text_1L_chr = character(0),
                                  set_idx_1L_int = integer(0),
                                  transform_att_nms_1L_lgl = T){
  if(identical(set_idx_1L_int, integer(0))){
    set_idx_1L_int <- length(dce_design_ls$choice_cards_ls)
  }
  atts_chr <- get_atts(dce_design_ls$choice_sets_ls$att_lvls_tb, return_1L_chr = "all")
  if(transform_att_nms_1L_lgl)
    atts_chr <- atts_chr %>% stringr::str_replace_all("_"," ")
  des_mat <- dce_design_ls$efnt_dsn_ls[[set_idx_1L_int]]$design
  if(!identical(block_1L_int, integer(0))){
    intro_sfx_1L_chr <- paste0("(Choice cards from  block ", block_1L_int,")")
    indcs_int <- dce_design_ls$choice_cards_ls[[set_idx_1L_int]]$block_indcs_ls[[block_1L_int]]
    des_mat <- des_mat[cut(1:nrow(des_mat),
                           dce_design_ls$choice_sets_ls$nbr_of_sets_1L_int, labels=F) %in% indcs_int ,]
    total_1L_int <- dce_design_ls$choice_sets_ls$nbr_of_sets_1L_int / dce_design_ls$choice_sets_ls$nbr_of_blocks_1L_int
  }else{
    intro_sfx_1L_chr <- "(All choice cards from all blocks)"
    total_1L_int <- dce_design_ls$choice_sets_ls$nbr_of_sets_1L_int
  }
  if(identical(intro_text_1L_chr, character(0)))
    intro_text_1L_chr <- paste0("Survey Preview ",intro_sfx_1L_chr)
  if(identical(button_txt_1L_chr, character(0)))
    button_txt_1L_chr = "Please choose the alternative you prefer"
  if(identical(end_txt_1L_chr, character(0)))
    end_txt_1L_chr <- "Thanks for taking the survey"
  idefix::SurveyApp(des = des_mat,
                    n.total = total_1L_int,
                    alts = dce_design_ls$choice_sets_ls$alternatives_chr,
                    atts = atts_chr,
                    lvl.names = make_tfd_lvls_ls(dce_design_ls) %>% unname(),
                    c.lvls = get_lvls(dce_design_ls$choice_sets_ls$att_lvls_tb, return_1L_chr = "cont") %>% unname() %>% purrr::map(~as.numeric(.x)),
                    coding = get_att_smrys(dce_design_ls,return_1L_chr = "type"),
                    buttons.text = button_txt_1L_chr,
                    intro.text = intro_text_1L_chr,
                    end.text = end_txt_1L_chr,
                    no.choice = dce_design_ls$choice_sets_ls$opt_out_idx_1L_int,
                    alt.cte = dce_design_ls$priors_ls[[min(set_idx_1L_int,length(dce_design_ls$priors_ls))]]$altv_con_int)

}
