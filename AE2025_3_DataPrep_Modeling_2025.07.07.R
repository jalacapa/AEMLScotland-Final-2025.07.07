####################################################
# R CODE 3: DATA PREPARATION PRIOR TO MODELING
####################################################

# ==================================================
# SETUP: Package Installation & Loading
# ==================================================
packages <- c("readr", "dplyr", "janitor", "tidyr", "car", "tibble")
installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed)) install.packages(pkg)
}
lapply(packages, library, character.only = TRUE)

# ==================================================
# SETUP: Working Directory & Dataset
# ==================================================
setwd("/cloud/project")
if (!dir.exists("outputs")) dir.create("outputs")

AEdata_model <- read_csv("wideMonthlyAE_Merged_ForModeling.csv") %>%
  janitor::clean_names()

# ==================================================
# Drop Unknown & Raw Columns
# ==================================================
AEdata_model <- AEdata_model %>%
  select(-matches("unknown")) %>%
  select(-adherence_0, -adherence_1, -year_month)

# ==================================================
# Collapse Referral Sources
# ==================================================
AEdata_model <- AEdata_model %>%
  mutate(
    referral_ambulance = rowSums(select(., starts_with("referral_age_ambulance")), na.rm = TRUE),
    referral_fnc       = rowSums(select(., starts_with("referral_age_flow_navigation_centre_virtual_clinic")), na.rm = TRUE),
    referral_gp        = rowSums(select(., starts_with("referral_age_gp_practice")), na.rm = TRUE),
    referral_nhs24     = rowSums(select(., starts_with("referral_age_nhs_24")), na.rm = TRUE),
    referral_other     = rowSums(select(., starts_with("referral_age_other")), na.rm = TRUE),
    referral_pcooh     = rowSums(select(., starts_with("referral_age_primary_care_out_of_hours_services")), na.rm = TRUE),
    referral_self      = rowSums(select(., starts_with("referral_age_self_referral")), na.rm = TRUE)
  ) %>%
  select(-starts_with("referral_age_"))

# ==================================================
# Collapse Discharge Destinations
# ==================================================
AEdata_model <- AEdata_model %>%
  mutate(
    discharge_same_hospital = rowSums(select(., starts_with("discharge_age_admission_to_same_hospital")), na.rm = TRUE),
    discharge_home          = rowSums(select(., starts_with("discharge_age_discharged_home_or_to_usual_place_of_residence")), na.rm = TRUE),
    discharge_other         = rowSums(select(., starts_with("discharge_age_other")), na.rm = TRUE),
    discharge_transferred   = rowSums(select(., starts_with("discharge_age_transferred_to_other_hospital_service")), na.rm = TRUE)
  ) %>%
  select(-starts_with("discharge_age_"))

# ==================================================
# Convert Counts to Proportions
# ==================================================
AEdata_model <- AEdata_model %>%
  mutate(age_total = age_under_18 + age_18_24 + age_25_39 + age_40_64 + age_65_74 + age_75_plus) %>%
  mutate(across(
    c(age_under_18, age_18_24, age_25_39, age_40_64, age_65_74, age_75_plus),
    ~ ifelse(age_total == 0, 0, .x / age_total), .names = "{.col}_prop"
  )) %>%
  mutate(day_total = rowSums(select(., starts_with("day_")), na.rm = TRUE)) %>%
  mutate(across(
    starts_with("day_"),
    ~ ifelse(day_total == 0, 0, .x / day_total), .names = "{.col}_prop"
  )) %>%
  mutate(deprivation_total = rowSums(select(., starts_with("deprivation_")), na.rm = TRUE)) %>%
  mutate(across(
    starts_with("deprivation_"),
    ~ ifelse(deprivation_total == 0, 0, .x / deprivation_total), .names = "{.col}_prop"
  )) %>%
  mutate(
    weekday_hour_total = rowSums(select(., matches("hour_week_\\d+_weekday$")), na.rm = TRUE),
    weekend_hour_total = rowSums(select(., matches("hour_week_\\d+_weekend$")), na.rm = TRUE)
  ) %>%
  mutate(across(matches("hour_week_\\d+_weekday$"), ~ ifelse(weekday_hour_total == 0, 0, .x / weekday_hour_total), .names = "{.col}_prop")) %>%
  mutate(across(matches("hour_week_\\d+_weekend$"), ~ ifelse(weekend_hour_total == 0, 0, .x / weekend_hour_total), .names = "{.col}_prop")) %>%
  mutate(sex_total = sex_female + sex_male) %>%
  mutate(
    sex_female_prop = ifelse(sex_total == 0, 0, sex_female / sex_total),
    sex_male_prop   = ifelse(sex_total == 0, 0, sex_male / sex_total)
  ) %>%
  mutate(referral_total = referral_ambulance + referral_fnc + referral_gp + referral_nhs24 + referral_other + referral_pcooh + referral_self) %>%
  mutate(across(
    c(referral_ambulance, referral_fnc, referral_gp, referral_nhs24, referral_other, referral_pcooh, referral_self),
    ~ ifelse(referral_total == 0, 0, .x / referral_total), .names = "{.col}_prop"
  )) %>%
  mutate(discharge_total = discharge_same_hospital + discharge_home + discharge_other + discharge_transferred) %>%
  mutate(across(
    c(discharge_same_hospital, discharge_home, discharge_other, discharge_transferred),
    ~ ifelse(discharge_total == 0, 0, .x / discharge_total), .names = "{.col}_prop"
  )) %>%
  mutate(inout_total = in_out_in + in_out_out) %>%
  mutate(
    in_out_in_prop  = ifelse(inout_total == 0, 0, in_out_in / inout_total),
    in_out_out_prop = ifelse(inout_total == 0, 0, in_out_out / inout_total)
  )
names(AEdata_model)
# ==================================================
# Drop Counts AFTER Proportion Creation
# ==================================================
AEdata_model <- AEdata_model %>%
  select(
    health_board, year, month,
    matches("_prop$"),  # retain all proportion variables
    covid_phase, adherence_binary
  )
AEdata_model <- AEdata_model %>%
  select(-deprivation_total_prop)
names(AEdata_model)

# ==================================================
# Drop Reference Levels
# ==================================================
AEdata_model <- AEdata_model %>%
  select(
    -age_18_24_prop,                      # age reference
    -day_monday_prop,                    # day of week reference
    -deprivation_1_prop,                 # deprivation reference
    -hour_week_00_weekday_prop,          # weekday hour reference
    -hour_week_00_weekend_prop,          # weekend hour reference
    -sex_female_prop,                    # sex reference
    -referral_ambulance_prop,            # referral reference
    -discharge_same_hospital_prop,       # discharge reference
    -in_out_in_prop                      # in/out of hours reference
  )

# ==================================================
# Export Pre-VIF Dataset
# ==================================================
write_csv(AEdata_model, "outputs/wideMonthlyAE_ForModeling_novif.csv")

# ==================================================
# ALIAS DETECTION
# ==================================================
alias_check <- alias(lm(as.numeric(adherence_binary) ~ ., data = AEdata_model))
aliased_vars <- names(unlist(alias_check$Complete))

if (length(aliased_vars) > 0) {
  AEdata_model_clean <- AEdata_model %>% select(-all_of(aliased_vars))
  aliased_log <- tibble(Variable = aliased_vars, Reason = "Aliased coefficient (alias())")
  write_csv(aliased_log, "outputs/Dropped_Variables_Aliased_Log.csv")
} else {
  AEdata_model_clean <- AEdata_model
  cat("No aliased variables detected.\n")
}

# Manual Removal of Potentially Aliased Variables
AEdata_model_clean <- AEdata_model_clean %>%
  select(
    -starts_with("day_"),                # remove all day_*_prop variables
    -in_out_out_prop,                    # in/out of hours as potentially aliased
    )
names(AEdata_model_clean)

# ==================================================
# VIF MULTICOLLINEARITY FILTERING 
# ==================================================
vif_data <- AEdata_model_clean
vif_log <- tibble(Step = integer(), Variable = character(), GVIF_Adj = numeric())
step <- 1

repeat {
  vif_model <- lm(as.numeric(adherence_binary) ~ ., data = vif_data)
  vif_vals <- car::vif(vif_model)
  
  if (is.matrix(vif_vals)) {
    vif_df <- as.data.frame(vif_vals)
    vif_df$Variable <- rownames(vif_df)
    vif_df$GVIF_Adj <- sqrt(vif_df$GVIF^(1 / (2 * vif_df$Df)))
  } else {
    vif_df <- tibble(Variable = names(vif_vals), GVIF_Adj = vif_vals)
  }
  
  high_vif <- vif_df %>% filter(GVIF_Adj > 5)
  
  if (nrow(high_vif) == 0) {
    cat("\n✅ VIF filtering complete. All GVIF_Adj ≤ 5.\n")
    break
  }
  
  to_drop <- high_vif %>%
    arrange(desc(GVIF_Adj)) %>%
    slice_head(n = 1)  # Robust fix
  
  message(sprintf("[STEP %d] Dropping %s (GVIF_Adj = %.3f)", step, to_drop$Variable, to_drop$GVIF_Adj))
  
  vif_log <- bind_rows(vif_log, tibble(
    Step = step,
    Variable = to_drop$Variable,
    GVIF_Adj = to_drop$GVIF_Adj
  ))
  
  vif_data <- vif_data %>% select(-all_of(to_drop$Variable))
  step <- step + 1
}


# Final VIF Table
vif_model_final <- lm(as.numeric(adherence_binary) ~ ., data = vif_data)
vif_final <- car::vif(vif_model_final)

if (is.matrix(vif_final)) {
  vif_final_df <- as.data.frame(vif_final)
  vif_final_df$Variable <- rownames(vif_final_df)
  vif_final_df$GVIF_Adj <- sqrt(vif_final_df$GVIF^(1 / (2 * vif_final_df$Df)))
  vif_final_df <- vif_final_df %>% arrange(desc(GVIF_Adj))
} else {
  vif_final_df <- tibble(Variable = names(vif_final), GVIF_Adj = vif_final) %>%
    arrange(desc(GVIF_Adj))
}

cat("\n===== FINAL VIF TABLE (All GVIF_Adj ≤ 5) =====\n")
print(vif_final_df)

# ==================================================
# Export Final VIF-Filtered Dataset
# ==================================================
write_csv(vif_data, "outputs/wideMonthlyAE_ForModeling_vif.csv")
write_csv(vif_log, "outputs/Dropped_Variables_VIF_Log.csv")
write_csv(vif_final_df, "outputs/Final_VIF_Table.csv")

#==================================================
# --- DONE ---
#==================================================

