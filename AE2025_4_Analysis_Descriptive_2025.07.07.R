# ==================================================
# R CODE 4: DESCRIPTIVE DATA ANALYSIS
# ==================================================

# --- Setup: Install and load required packages
packages <- c("tidyverse", "janitor", "lubridate", "skimr", "gt", "webshot")
installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed)) install.packages(pkg)
}
lapply(packages, library, character.only = TRUE)

# Optional: PhantomJS setup (may error but safe to ignore on Posit Cloud)
try(webshot::install_phantomjs(), silent = TRUE)

# --- Setup: Working directory and load dataset
setwd("/cloud/project")
if (!dir.exists("outputs")) dir.create("outputs")

AEdata <- read_csv("wideMonthlyAE_Merged_ForModeling.csv") %>%
  clean_names() %>%
  mutate(year_month = ym(year_month))

# ==================================================
# Descriptive: Missing data summary
# ==================================================
missing_summary <- AEdata %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(missing_percent = round(100 * missing_count / nrow(AEdata), 2)) %>%
  arrange(desc(missing_percent))
write_csv(missing_summary, "outputs/missing_summary.csv")

# ==================================================
# Monthly Adherence (% met target)
# ==================================================
monthly_adherence <- AEdata %>%
  group_by(year_month) %>%
  summarise(adherence_percent = mean((adherence_1)/(adherence_1 + adherence_0), na.rm = TRUE))
#derived the proportion of adherent cases from total attendances (adherence_1 + adherence_0)

write_csv(monthly_adherence, "outputs/monthly_adherence_percent.csv")

ggplot(monthly_adherence, aes(x = year_month, y = adherence_percent)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  labs(x = "Month", y = "Mean Adherence (%)") +
  scale_y_continuous(name="Mean Adherence", labels = percent) +
  theme_minimal()

ggsave("outputs/monthly_adherence_percent.png", dpi = 320, width = 8, height = 5)

# ==================================================
# COVID Phase Tagging and Adherence (%)
# ==================================================
AEdata <- AEdata %>%
  mutate(covid_phase = case_when(
    year_month >= ymd("2018-01-01") & year_month <= ymd("2020-02-01") ~ 0,
    year_month >= ymd("2020-03-01") & year_month <= ymd("2020-05-01") ~ 1,
    year_month >= ymd("2020-06-01") & year_month <= ymd("2020-09-01") ~ 2,
    year_month >= ymd("2020-10-01") & year_month <= ymd("2021-02-01") ~ 3,
    year_month >= ymd("2021-03-01") & year_month <= ymd("2021-05-01") ~ 4,
    year_month >= ymd("2021-06-01") & year_month <= ymd("2021-09-01") ~ 5,
    year_month >= ymd("2021-10-01") & year_month <= ymd("2021-11-01") ~ 6,
    year_month >= ymd("2021-12-01") & year_month <= ymd("2022-02-01") ~ 7,
    year_month >= ymd("2022-03-01") & year_month <= ymd("2022-09-01") ~ 8,
    year_month >= ymd("2022-10-01") & year_month <= ymd("2023-12-01") ~ 9
  ))

adherence_by_covid <- AEdata %>%
  group_by(covid_phase) %>%
  summarise(adherence_percent = mean((adherence_1)/(adherence_1 + adherence_0), na.rm = TRUE))
#derived the proportion of adherent cases from total attendances (adherence_1 + adherence_0)
write_csv(adherence_by_covid, "outputs/adherence_by_covid_phase_percent.csv")

ggplot(adherence_by_covid, aes(x = factor(covid_phase), y = adherence_percent)) +
  geom_col(fill = "darkred") +
  labs(title = "Adherence by COVID-19 Phase",
       x = "COVID Phase", y = "Mean Adherence (%)") +
  scale_y_continuous(name="Mean Adherence", labels = percent)
  theme_minimal()

ggsave("outputs/adherence_by_covid_phase_percent.png", dpi = 320, width = 7, height = 5)

# ==================================================
# DONE
# ==================================================

