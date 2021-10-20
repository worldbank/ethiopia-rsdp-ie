
# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "long_diff_data.Rds"))
data$woreda_hdx_w_uid <- data$woreda_hdx_w_uid %>% as.factor()
data <- data[!is.na(data$woreda_hdx_w_uid),]

# Regressions ------------------------------------------------------------------

## First Stage
lm_s1_anyimproved <- felm(near_anyimproved_ever ~ near_mst | woreda_hdx_w_uid | 0 | woreda_hdx_w_uid, data=data)
lm_s2_improved_bf2013 <- felm(near_improved_bf2013_ever ~ near_mst | woreda_hdx_w_uid | 0 | woreda_hdx_w_uid, data=data)

## Second Stage
iv_dmspols <- ivreg(dmspols_2013diff ~ near_anyimproved_ever + temp_avg_2013diff + precipitation_2013diff | 
        near_mst + temp_avg_2013diff + precipitation_2013diff,
      data = data) 

iv_dmspols_cl <- cluster.im.ivreg(mod=iv_dmspols, dat=data, cluster = ~woreda_hdx_w_uid)



iv_robust(dmspols_2013diff ~ near_anyimproved_ever + temp_avg_2013diff + precipitation_2013diff | 
            near_mst + temp_avg_2013diff + precipitation_2013diff,
          data = data,
          clusters = woreda_hdx_w_uid,
          return_vcov = F) %>%
  summary()




data_all$precipitation_2013diff


fit_iv <- ivreg(wage ~ educ + age + I(age^2) + factor(year) |
                  age + I(age^2) + factor(year) * factor(quarter),
                data = AK)





lm1 <- lm_robust(near_anyimproved_ever ~ near_mst, data = data_all, fixed_effects = ~woreda_hdx_w_uid)

data_all$woreda_hdx_w_uid

lm1 <- lm_robust(near_anyimproved_ever ~ near_mst, data = data_all, fixed_effects = "woreda_hdx_w_uid")

head(data_all)







