library(ready4)
#library(generics)
library(ready4show)
library(ready4use)
library(ready4fun)
# library(youthvars)
# library(scorz)
# library(specific)
ready4fun::write_fn_type_dirs()
# MANUAL STEP. Write all your functions to R files in the new "fns" directory.
fns_env_ls <- ready4fun::read_fns(c("data-raw/fns/","data-raw/mthds/"),
                                  fns_env = new.env(parent = globalenv()))
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Model Youth Choice Behaviours" %>% tools::toTitleCase(),
                                 pkg_desc_1L_chr = "Tools for modelling choice behaviours in youth mental health economic models.
                                 Supports standardised workflows for the design and analysis of Discrete Choice Experiments.
                                 This development version of the mychoice package has been made available as part of the process of testing and documenting the package.
                                 If you have any questions, please contact the authors (matthew.hamilton1@monash.edu).",
                                 authors_prsn = c(utils::person(given = "Matthew",family = "Hamilton", email = "matthew.hamilton1@monash.edu",
                                                                role = c("aut", "cre", "fnd", "cph"),comment = c(ORCID = "0000-0001-7407-9194")),
                                                  #utils::person(given = "Caroline",family = "Gao",email = "caroline.gao@orygen.org.au", role = c("aut"),comment = c(ORCID = "0000-0002-0987-2759")),
                                                  utils::person("Orygen", role = c("cph", "fnd"))#,
                                                  # utils::person("Headspace", role = c( "fnd")),
                                                  # utils::person("National Health and Medical Research Council", role = c( "fnd"))
                                                  ),
                                 urls_chr = c("https://ready4-dev.github.io/mychoice/",
                                              "https://github.com/ready4-dev/mychoice",
                                              "https://www.ready4-dev.com/")) %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(#depends_chr = "TTU",#c("eq5d","ggfortify"),
    suggests_chr = c("knitr","rmarkdown")),
    build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
    check_type_1L_chr = "ready4",
    copyright_holders_chr = "Matthew Hamilton and Orygen",
    custom_dmt_ls = ready4fun::make_custom_dmt_ls(user_manual_fns_chr = c(#"add_nothing"
      "add_age_and_area_cmprsns"#,
                                                                          # "add_analysis",
                                                                          # "add_choice_mdls",
                                                                          # "add_cost_comparison",
                                                                          # "add_cut_pnts_cmprsn",
                                                                          # "add_new_choice_cmprsn",
                                                                          # "add_flags",
                                                                          # "make_case_choices_mat",
                                                                          # "make_choice_mdlng_ds",
                                                                          # "make_choice_smrys",
                                                                          # "make_flags_smry_ls",
                                                                          # "make_mdl_params_ls",
                                                                          # "make_smry_tb",
                                                                          # "remove_no_choice_responses",
                                                                          # "remove_red_flag_cases",
                                                                          # "transform_repln_ds_for_analysis",
                                                                          # "write_choice_mdlng_ws",
                                                                          # "write_preprocessing_outp"
      )),##
    dev_pkgs_chr = c(#"ready4",#"ready4fun",
                     "ready4use","ready4show"),
    lifecycle_stage_1L_chr = "experimental",
    path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/mychoice-logo/default.png",
    piggyback_to_1L_chr = "ready4-dev/ready4",
    ready4_type_1L_chr = "modelling",
    zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7213799.svg)](https://doi.org/10.5281/zenodo.7213799)"
    )
z <- ready4pack::make_pt_ready4pack_manifest(x) %>%
  ready4pack::ready4pack_manifest()
z <- ready4::author(z)
ready4::write_extra_pkgs_to_actions(path_to_dir_1L_chr = ".github/workflows", consent_1L_chr = "Y")
write_to_edit_workflow("pkgdown.yaml", consent_1L_chr = "Y") # In other packages, run for "test-coverage.yaml" as well.
write_to_tidy_pkg(z$x_ready4fun_manifest, build_vignettes_1L_lgl = TRUE,
                  clean_license_1L_lgl = TRUE, consent_1L_chr = "Y",
                  examples_chr = character(0),
                  suggest_chr = "pkgload")
# readLines("_pkgdown.yml") %>%
#   stringr::str_replace_all("  - text: Model", "  - text: Framework & Model") %>%
#   writeLines(con = "_pkgdown.yml")
# devtools::build_vignettes()
# devtools::build_vignettes()
# ready4::write_citation_cff(packageDescription("mychoice"),
#                            citation_chr = readLines("inst/CITATION"))
# readLines(".github/workflows/R-CMD-check.yaml") %>%
#   stringr::str_replace_all("r-lib/actions/setup-r@master","r-lib/actions/setup-r@v2") %>%
#   stringr::str_replace_all("r-lib/actions/setup-pandoc@master","r-lib/actions/setup-pandoc@v2") %>%
#   writeLines(con = ".github/workflows/R-CMD-check.yaml")
# May need to ad lwgeom pkg
# usethis::use_dev_package("specific",
#                          remote = "ready4-dev/specific")
# Note, on initial setup ran commands from: https://usethis.r-lib.org/reference/use_pkgdown.html
# May need to link to this help text for Mac users: https://stackoverflow.com/questions/70638118/configuring-compilers-on-mac-m1-big-sur-monterey-for-rcpp-and-other-tools
