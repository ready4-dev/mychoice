library(ready4use)
X <- ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                               gh_tag_1L_chr = "Documentation_0.0")
Y <- ingest(X)
# prototype_lup <- procure(procureSlot(Y,
#                                      "b_Ready4useIngest"),
#                          "prototype_lup")
# prototype_lup <- prototype_lup %>%
#   tibble::add_case(type_chr = "dfidx",
#                    val_chr = "dfidx::dfidx(data.frame(character(0),character(0)))",
#                    pt_ns_chr = "dfidx",
#                    fn_to_call_chr ="dfidx",
#                    default_val_chr = "dfidx()",
#                    old_class_lgl = F) %>%
#   dplyr::arrange(pt_ns_chr,type_chr)
# abbreviations_lup <- procure(procureSlot(y,
#                                          "b_Ready4useIngest"),
#                              "abbreviations_lup")
# Y <- renewSlot(Y,
#                new_val_xx = Ready4useIngest(objects_ls = list(prototype_lup = prototype_lup)),
#                slot_nm_1L_chr = "b_Ready4useIngest")
# Z <- share(Y, type_1L_chr = "prefer_gh")
Y@b_Ready4useIngest@objects_ls$treat_as_words_chr <- c(Y@b_Ready4useIngest@objects_ls$treat_as_words_chr,"halton") %>% sort()
Y@b_Ready4useIngest@objects_ls$abbreviations_lup <- ready4fun::renew.ready4fun_abbreviations(Y@b_Ready4useIngest@objects_ls$abbreviations_lup,
                                         short_name_chr = z$x_ready4fun_manifest$problems_ls$missing_abbrs_chr[z$x_ready4fun_manifest$problems_ls$missing_abbrs_chr != "halton"],
                                         long_name_chr = c("alternative",
                                                           "attribute",
                                                           "attributes",
                                                           "assessment",
                                                           "consecutive",
                                                           "continuous",
                                                           "discrete choice experiment",
                                                           "date of birth",
                                                           "derived",
                                                           "duration",
                                                           "formula",
                                                           "generalised mutinomial logit model",
                                                           #####,
                                                           "indices",
                                                           "logit",
                                                           "longitude",
                                                           "market",
                                                           "mlogit package",
                                                           "multinomial logit",
                                                           "variables that enter the mean of random parameters",
                                                           "points",
                                                           "qualitative",
                                                           "Socio-Economic Indices for Areas",
                                                           "Social Interaction Anxiety Scale",
                                                           "Scaled Multinomial Logit Model",
                                                           "total")) %>% dplyr::distinct()
Y <- share(Y, type_1L_chr = "prefer_gh")
