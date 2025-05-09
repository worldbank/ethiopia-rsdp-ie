# Market Access Sensitivity 

# CONF INT IS NOT CORRECT !!!!

# 1. Exclude Sparse Regions
# 2. Varying fixed effects
# 3. Varying doughnut sizes
# 4. Varying values of theta
# 5. Varying baseline nighttime light groups
# 6. Logs instead of ihs
# 7. Woredas as unit of analysis

# Load data --------------------------------------------------------------------
ma_df <- file.path(panel_rsdp_imp_dir, "ma_results", "individual_files") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  str_subset("ntlgroups4") %>%
  map_df(readRDS)

ma_df <- ma_df %>%
  dplyr::mutate(term = term %>% str_replace_all("fit_", "")) %>%
  dplyr::mutate(dv = case_when(
    dv == "dmspols_harmon_ihs" ~ "NTL",
    dv == "globcover_urban_sum_ihs" ~ "Urban",
    dv == "globcover_cropland_sum_ihs" ~ "Cropland"
  )) %>%
  dplyr::filter(term %>% str_detect("MA_")) %>%
  dplyr::filter(term != "MA_var_exc_1996",
                term != "MA_var_1996") %>%
  dplyr::mutate(term = case_when(
    term == "MA_var" ~ "MA",
    term == "MA_varXdmspols_harmon_1996_bin4_2" ~ "MA X Low NTL",
    term == "MA_varXdmspols_harmon_1996_bin4_3" ~ "MA X Med NTL",
    term == "MA_varXdmspols_harmon_1996_bin4_4" ~ "MA X High NTL",
    TRUE ~ term
  )) %>%
  dplyr::mutate(term = term %>% factor(levels = rev(c("MA",
                                                  "MA X Low NTL",
                                                  "MA X Med NTL",
                                                  "MA X High NTL"))))

make_result_df <- function(ols_iv_i,
                           MA_interact_i,
                           rsdp){
  
  if(rsdp == "i_iv"){
    param_start_year_i = "1996"
    param_end_year_i = "2016"
  }
  
  if(rsdp == "i_iii"){
    param_start_year_i = "1996"
    param_end_year_i = "2009"
  }
  
  if(rsdp == "iv"){
    param_start_year_i = "2012"
    param_end_year_i = "2016"
  }
  
  result_df <- bind_rows(
    ma_df %>%
      dplyr::filter(param_log == "_log",
                    param_theta == "3_8",
                    param_exclude == "_exclude50km",
                    param_start_year == param_start_year_i,
                    param_end_year == param_end_year_i,
                    param_trans_type == "ihs",
                    param_fe_var_i == "Z_CODE",
                    param_remove_sparse_regions == F,
                    ntl_groups == 4,
                    MA_interact == MA_interact_i,
                    ols_iv == ols_iv_i,
                    unit == "kebele") %>%
      dplyr::mutate(name = "Main Result"),
    
    ma_df %>%
      dplyr::filter(param_log == "_log",
                    param_theta == "3_8",
                    param_exclude == "_exclude50km",
                    param_start_year == param_start_year_i,
                    param_end_year == param_end_year_i,
                    param_trans_type == "ihs",
                    param_fe_var_i == "Z_CODE",
                    param_remove_sparse_regions == T,
                    ntl_groups == 4,
                    MA_interact == MA_interact_i,
                    ols_iv == ols_iv_i,
                    unit == "kebele") %>%
      dplyr::mutate(name = "Remove Sparse Regions"),
    
    ma_df %>%
      dplyr::filter(param_log == "_log",
                    param_theta == "3_8",
                    param_exclude == "_exclude50km",
                    param_start_year == param_start_year_i,
                    param_end_year == param_end_year_i,
                    param_trans_type == "ihs",
                    param_fe_var_i == "W_CODE",
                    param_remove_sparse_regions == F,
                    ntl_groups == 4,
                    MA_interact == MA_interact_i,
                    ols_iv == ols_iv_i,
                    unit == "kebele") %>%
      dplyr::mutate(name = "Fixed Effects: Woredas"),
    
    ma_df %>%
      dplyr::filter(param_log == "_log",
                    param_theta == "3_8",
                    param_exclude == "_exclude50km",
                    param_start_year == param_start_year_i,
                    param_end_year == param_end_year_i,
                    param_trans_type == "ihs",
                    param_fe_var_i == "R_CODE",
                    param_remove_sparse_regions == F,
                    ntl_groups == 4,
                    MA_interact == MA_interact_i,
                    ols_iv == ols_iv_i,
                    unit == "kebele") %>%
      dplyr::mutate(name = "Fixed Effects: Regions"),
    
    ma_df %>%
      dplyr::filter(param_log == "_log",
                    param_theta == "3_8",
                    param_exclude == "_exclude20km",
                    param_start_year == param_start_year_i,
                    param_end_year == param_end_year_i,
                    param_trans_type == "ihs",
                    param_fe_var_i == "Z_CODE",
                    param_remove_sparse_regions == F,
                    ntl_groups == 4,
                    MA_interact == MA_interact_i,
                    ols_iv == ols_iv_i,
                    unit == "kebele") %>%
      dplyr::mutate(name = "Doughtnut: Exclude 20km"),
    
    ma_df %>%
      dplyr::filter(param_log == "_log",
                    param_theta == "3_8",
                    param_exclude == "_exclude20km",
                    param_start_year == param_start_year_i,
                    param_end_year == param_end_year_i,
                    param_trans_type == "ihs",
                    param_fe_var_i == "Z_CODE",
                    param_remove_sparse_regions == F,
                    ntl_groups == 4,
                    MA_interact == MA_interact_i,
                    ols_iv == ols_iv_i,
                    unit == "kebele") %>%
      dplyr::mutate(name = "Doughtnut: Exclude 100km"),
    
    ma_df %>%
      dplyr::filter(param_log == "_log",
                    param_theta == "2",
                    param_exclude == "_exclude50km",
                    param_start_year == param_start_year_i,
                    param_end_year == param_end_year_i,
                    param_trans_type == "ihs",
                    param_fe_var_i == "Z_CODE",
                    param_remove_sparse_regions == F,
                    ntl_groups == 4,
                    MA_interact == MA_interact_i,
                    ols_iv == ols_iv_i,
                    unit == "kebele") %>%
      dplyr::mutate(name = "Theta: 2"),
    
    ma_df %>%
      dplyr::filter(param_log == "_log",
                    param_theta == "5",
                    param_exclude == "_exclude50km",
                    param_start_year == param_start_year_i,
                    param_end_year == param_end_year_i,
                    param_trans_type == "ihs",
                    param_fe_var_i == "Z_CODE",
                    param_remove_sparse_regions == F,
                    ntl_groups == 4,
                    MA_interact == MA_interact_i,
                    ols_iv == ols_iv_i,
                    unit == "kebele") %>%
      dplyr::mutate(name = "Theta: 5"),
    
    ma_df %>%
      dplyr::filter(param_log == "_log",
                    param_theta == "8",
                    param_exclude == "_exclude50km",
                    param_start_year == param_start_year_i,
                    param_end_year == param_end_year_i,
                    param_trans_type == "ihs",
                    param_fe_var_i == "Z_CODE",
                    param_remove_sparse_regions == F,
                    ntl_groups == 4,
                    MA_interact == MA_interact_i,
                    ols_iv == ols_iv_i,
                    unit == "kebele") %>%
      dplyr::mutate(name = "Theta: 8"),
    
    ma_df %>%
      dplyr::filter(param_log == "_log",
                    param_theta == "3_8",
                    param_exclude == "_exclude50km",
                    param_start_year == param_start_year_i,
                    param_end_year == param_end_year_i,
                    param_trans_type == "ihs",
                    param_fe_var_i == "Z_CODE",
                    param_remove_sparse_regions == F,
                    ntl_groups == 4,
                    MA_interact == MA_interact_i,
                    ols_iv == ols_iv_i,
                    unit == "kebele") %>%
      dplyr::mutate(name = "D.V. Transform: Log")#,
    
    # ma_df %>%
    #   dplyr::filter(param_log == "_log",
    #                 param_theta == "3_8",
    #                 param_exclude == "_exclude50km",
    #                 param_start_year == param_start_year_i,
    #                 param_end_year == param_end_year_i,
    #                 param_trans_type == "ihs",
    #                 param_fe_var_i == "Z_CODE",
    #                 param_remove_sparse_regions == F,
    #                 ntl_groups == 4,
    #                 MA_interact == MA_interact_i,
    #                 ols_iv == ols_iv_i,
    #                 unit == "woreda") %>%
    #   dplyr::mutate(name = "Unit of Analysis: Woreda")
    
  )
  
  name_other <- unique(result_df$name)[unique(result_df$name) != "Main Result"]
  
  result_df <- result_df %>%
    dplyr::mutate(name = name %>%
                    factor(levels = rev(c("Main Result", name_other)))) %>%
    dplyr::mutate(dv = dv %>% factor(levels = rev(c("Cropland", 
                                                    "Urban",
                                                    "NTL")))) %>%
    dplyr::mutate(sig = case_when(
      p_value <= 0.01 ~ "p-value < 0.01",
      p_value <= 0.05 ~ "p-value < 0.05",
      p_value <= 0.1 ~ "p-value < 0.1",
      p_value > 0.1 ~ "p-value > 0.1"
    ))
  
  return(result_df)
  
}

## All
r_ols_rsdp_all_df       <- make_result_df("ols", F, "i_iv")
r_ols_inter_rsdp_all_df <- make_result_df("ols", T, "i_iv")

r_iv_rsdp_all_df       <- make_result_df("iv", F, "i_iv")
r_iv_inter_rsdp_all_df <- make_result_df("iv", T, "i_iv")

## I - III
r_ols_rsdp_iii_df       <- make_result_df("ols", F, "i_iii")
r_ols_inter_rsdp_iii_df <- make_result_df("ols", T, "i_iii")

r_iv_rsdp_iii_df       <- make_result_df("iv", F, "i_iii")
r_iv_inter_rsdp_iii_df <- make_result_df("iv", T, "i_iii")

## IV
r_ols_rsdp_iv_df       <- make_result_df("ols", F, "iv")
r_ols_inter_rsdp_iv_df <- make_result_df("ols", T, "iv")

r_iv_rsdp_iv_df       <- make_result_df("iv", F, "iv")
r_iv_inter_rsdp_iv_df <- make_result_df("iv", T, "iv")

make_fig_one <- function(df, title_i){
  
  df %>%
    ggplot(aes(x = estimate,
               xmin = conf_low,
               xmax = conf_high,
               y = name,
               shape = sig)) +
    geom_vline(xintercept = 0, color = "gray") +
    geom_point() +
    geom_linerange() +
    facet_wrap(~dv, scales = "free_x") +
    labs(x = "Coef (+/- 95% CI)",
         y = NULL,
         shape = "Significance",
         title = title_i) +
    theme_classic2() +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.text = element_text(color = "black"),
          plot.title = element_text(face = "bold"),
          axis.text.x = element_text(size = 8)) +
    guides(
      shape = guide_legend(reverse = TRUE)
    )
}

make_fig_inter <- function(df, title_i){
  
  df %>%
    ggplot(aes(x = estimate,
               xmin = conf_low,
               xmax = conf_high,
               y = name,
               color = term,
               shape = sig)) +
    geom_vline(xintercept = 0, color = "gray") +
    geom_point(position = position_dodge(width = 0.7)) +
    geom_linerange(position = position_dodge(width = 0.7)) +
    facet_wrap(~dv, scales = "free_x") +
    labs(x = "Coef (+/- 95% CI)",
         y = NULL,
         color = "Coefficient",
         shape = "Significance",
         title = title_i) +
    scale_color_manual(values = c("red",
                                  "darkorange2",
                                  "dodgerblue2",
                                  "black")) +
    theme_classic2() +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.text = element_text(color = "black"),
          plot.title = element_text(face = "bold"),
          axis.text.x = element_text(size = 8)) +
    guides(
      color = guide_legend(reverse = TRUE),
      shape = guide_legend(reverse = TRUE)
    )
  
}

#### RSDP: All
p_ols_rsdp_all <- make_fig_one(r_ols_rsdp_all_df, "Model: OLS")
p_iv_rsdp_all <- make_fig_one(r_iv_rsdp_all_df, "Model: IV")

p_ols_inter_rsdp_all <- make_fig_inter(r_ols_inter_rsdp_all_df, "Model: OLS")
p_iv_inter_rsdp_all  <- make_fig_inter(r_iv_inter_rsdp_all_df, "Model: IV")

p_rsdp_all <- ggarrange(
  ggarrange(p_ols_rsdp_all,       p_iv_rsdp_all,       common.legend = T, nrow = 1, legend = "right"),
  ggarrange(p_ols_inter_rsdp_all, p_iv_inter_rsdp_all, common.legend = T, nrow = 1, legend = "right"),
  ncol = 1,
  heights = c(0.3, 0.7)
)

p_rsdp_all <- annotate_figure(
  p_rsdp_all,
  top = text_grob("RSDP I-IV", face = "bold", size = 14)
)

ggsave(p_rsdp_all, filename = file.path(paper_figures,
                                        "ma_rsdp_all.png"),
       height = 10, width = 12)

#### RSDP: III
p_ols_rsdp_iii <- make_fig_one(r_ols_rsdp_iii_df, "Model: OLS") + scale_shape_manual(values = c(15, 3))
p_iv_rsdp_iii <- make_fig_one(r_iv_rsdp_iii_df, "Model: IV") + scale_shape_manual(values = c(17, 15, 3))

p_ols_inter_rsdp_iii <- make_fig_inter(r_ols_inter_rsdp_iii_df, "Model: OLS")
p_iv_inter_rsdp_iii  <- make_fig_inter(r_iv_inter_rsdp_iii_df, "Model: IV")

p_rsdp_iii <- ggarrange(
  ggarrange(p_ols_rsdp_iii, p_iv_rsdp_iii,       common.legend = T, nrow = 1, legend = "right"),
  ggarrange(p_ols_inter_rsdp_iii, p_iv_inter_rsdp_iii, common.legend = T, nrow = 1, legend = "right"),
  ncol = 1,
  heights = c(0.3, 0.7)
)

p_rsdp_iii <- annotate_figure(
  p_rsdp_iii,
  top = text_grob("RSDP I-III", face = "bold", size = 14)
)

ggsave(p_rsdp_iii, filename = file.path(paper_figures,
                                        "ma_rsdp_iii.png"),
       height = 10, width = 12)

#### RSDP: IV
p_ols_rsdp_iv <- make_fig_one(r_ols_rsdp_iv_df, "Model: OLS")
p_iv_rsdp_iv <- make_fig_one(r_iv_rsdp_iv_df, "Model: IV")

p_ols_inter_rsdp_iv <- make_fig_inter(r_ols_inter_rsdp_iv_df, "Model: OLS")
p_iv_inter_rsdp_iv  <- make_fig_inter(r_iv_inter_rsdp_iv_df, "Model: IV")

p_rsdp_iv <- ggarrange(
  ggarrange(p_ols_rsdp_iv,       p_iv_rsdp_iv,       common.legend = T, nrow = 1, legend = "right"),
  ggarrange(p_ols_inter_rsdp_iv, p_iv_inter_rsdp_iv, common.legend = T, nrow = 1, legend = "right"),
  ncol = 1,
  heights = c(0.3, 0.7)
)

p_rsdp_iv <- annotate_figure(
  p_rsdp_iv,
  top = text_grob("RSDP IV", face = "bold", size = 14)
)

ggsave(p_rsdp_iv, filename = file.path(paper_figures,
                                       "ma_rsdp_iv.png"),
       height = 10, width = 12)

