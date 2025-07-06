####################################################
# R CODE 1: EXPLORATORY DATA ANALYSIS (EDA
# FOR THE LITERATURE REVIEW SECTION (2018–2023)
####################################################

# ==================================================
# SETUP: Package Installation & Loading
# ==================================================

# Install and load required packages for analysis and plotting
packages <- c("ggplot2", "dplyr", "scales", "readxl", "zoo", "ggsci", "forcats")
installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) install.packages(pkg)
}
lapply(packages, library, character.only = TRUE)

# ==================================================
# SETUP: Working Directory and Output Folder
# ==================================================

# Set working directory to project root and create an output folder for plots
setwd("/cloud/project")
if (!dir.exists("outputs")) dir.create("outputs")

# ==================================================
# 1. Monthly A&E Attendances Trend (2018–2023)
# ==================================================

# Read and filter monthly A&E attendance data
monthlyAE1823 <- read.csv("2025-05-06-ae-monthly-attendance-and-waiting-times-data.csv") %>%
  filter(as.Date(MonthEndingDate) >= as.Date("2018-01-01"))

# Aggregate attendances per NHS board per month
monthlyAE1823 <- aggregate(NumberOfAttendancesAll ~ NHSBoardName + MonthEndingDate, data = monthlyAE1823, sum)

# Line plot of attendance trends with COVID-19 reference line
p1 <- ggplot(monthlyAE1823, aes(x = as.Date(MonthEndingDate), y = NumberOfAttendancesAll,
                                group = NHSBoardName, color = NHSBoardName)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "9 months", date_labels = "%Y %b") +
  scale_color_viridis_d(option = "D") +
  geom_vline(xintercept = as.Date("2020-04-30"), color = "red", linetype = "dashed", size = 0.5) +
  labs(x = "Date", y = "Attendances") +
  theme_minimal()
ggsave("outputs/plot_monthly_trend.png", p1, width = 10, height = 6, dpi = 300)

# ==================================================
# 2. Referral Source – Pie Chart
# ==================================================

referralSource1823a <- read_excel("2025-05-06-referralsource.xlsx", sheet = "Health Board") %>%
  filter(Month >= as.Date("2018-01-01"))

referralSource1823a <- aggregate(Attendances ~ Referral, data = referralSource1823a, sum) %>%
  mutate(
    fraction = Attendances / sum(Attendances),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, -1))
  )

p2 <- ggplot(referralSource1823a, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Referral)) +
  geom_rect() +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Blues") +
  xlim(c(2, 4)) +
  labs(title = "5-Year Attendances by Referral Source", fill = "Referral Source") +
  theme_void()
ggsave("outputs/plot_referral_source_pie.png", p2, width = 6, height = 6, dpi = 300)

# ==================================================
# 2.1 Referral Source – Horizontal Bar Chart
# ==================================================
p2.1 <- ggplot(referralSource1823a) +
  geom_col(aes(y = reorder(Referral, Attendances), x = Attendances, fill = Referral)) +
  scale_fill_brewer(palette = "Blues") +
  scale_x_continuous(label = label_comma())+
  labs(x = "Attendances", y = "Referral Source", fill = "Referral Source",) +
  guides(fill = FALSE) +
  theme_minimal()

ggsave("outputs/plot_referral_source_bar.png", p2.1, width = 10, height = 6, dpi = 300)

# ==================================================
# 3. Discharge Destination – Pie Chart
# ==================================================
dischargeDestination1823a <- read_excel("2025-05-06-dischargedestination.xlsx", sheet = "Health Board") %>%
  filter(Month >= as.Date("2018-01-01"))

dischargeDestination1823a <- aggregate(Attendances ~ Discharge, data = dischargeDestination1823a, sum) %>%
  mutate(
    fraction = Attendances / sum(Attendances),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, -1))
  )

p3 <- ggplot(dischargeDestination1823a, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Discharge)) +
  geom_rect() +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  xlim(c(2, 4)) +
  labs(title = "5-Year Attendances by Discharge Destination", fill = "Discharge") +
  theme_void()
ggsave("outputs/plot_discharge_destination_pie.png", p3, width = 6, height = 6, dpi = 300)

# ==================================================
# 3.1 Discharge Destination – Horizontal Chart
# ==================================================
p3.1 <- ggplot(dischargeDestination1823a) +
  geom_col(aes(y = reorder(Discharge, Attendances), x = Attendances, fill = Discharge)) +
  scale_fill_brewer(palette = "Blues") +
  scale_x_continuous(label = label_comma())+
  labs(x = "Attendances", y = "Discharge Destination", fill = "Discharge Destination",) +
  guides(fill = FALSE) +
  theme_minimal()

ggsave("outputs/plot_discharge_destination_bar.png", p3.1, width = 10, height = 6, dpi = 300)

# ==================================================
# 4. In vs Out of Hours – Pie Chart
# ==================================================

inOut1823a <- read_excel("2025-05-06-whenpeopleattend-inoutofhours.xlsx", sheet = "Health Board") %>%
  filter(Month >= as.Date("2018-01-01"))

inOut1823a <- aggregate(Attendances ~ InOut, data = inOut1823a, sum) %>%
  mutate(
    fraction = Attendances / sum(Attendances),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, -1))
  )

p4 <- ggplot(inOut1823a, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = InOut)) +
  geom_rect() +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Blues") +
  xlim(c(2, 4)) +
  labs(title = "5-Year Attendances (In vs Out of Hours)", fill = "In/Out Hours") +
  theme_void()
ggsave("outputs/plot_inout_pie.png", p4, width = 6, height = 6, dpi = 300)

# ==================================================
# 4.1 In vs Out of Hours – Horizontal Bar Chart
# ==================================================
p4.1 <- ggplot(inOut1823a) +
  geom_col(aes(y = reorder(InOut, Attendances), x = Attendances, fill = InOut)) +
  scale_fill_brewer(palette = "Blues") +
  scale_x_continuous(label = label_comma())+
  labs(x = "Attendances", y = "In/Out Hours", fill = "In/Out Hours",) +
  guides(fill = FALSE) +
  theme_minimal()

ggsave("outputs/plot_inout_bar.png", p4.1, width = 10, height = 6, dpi = 300)

# ==================================================
# 5. Day of Week – Heatmap
# ==================================================

dayOfWeek1823 <- read_excel("2025-05-06-whenpeopleattend-dayofweek.xlsx", sheet = "Health Board") %>%
  filter(Month >= as.Date("2018-01-01")) %>%
  rename(Attendances = Average) %>%
  mutate(Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

dayOfWeek1823 <- aggregate(Attendances ~ HealthBoard + Day, data = dayOfWeek1823, sum)

p5 <- ggplot(dayOfWeek1823, aes(x = HealthBoard, y = fct_rev(Day), fill = Attendances)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(high = "red") +
  labs(x = "Health Board", y = "Day", fill = "Attendances") +
  theme_minimal()
ggsave("outputs/plot_dayofweek_heatmap.png", p5, width = 8, height = 6, dpi = 300)

# ==================================================
# 6. Arrival Hour – Heatmap with Week Facets
# ==================================================

arrivalHour1823 <- read_excel("2025-05-06-whenpeopleattend-arrivalhour.xlsx", sheet = "Health Board") %>%
  filter(Month >= as.Date("2018-01-01"))

arrivalHour1823 <- aggregate(Attendances ~ HealthBoard + Hour + Week, data = arrivalHour1823, sum)

# Reorder Hour for plotting and factor Week
arrivalHour1823$Hour <- factor(arrivalHour1823$Hour, levels = rev(sort(unique(arrivalHour1823$Hour))))
arrivalHour1823$Week <- factor(arrivalHour1823$Week, levels = c("Weekday", "Weekend"))

p6 <- ggplot(arrivalHour1823, aes(x = HealthBoard, y = Hour, fill = Attendances)) +
  geom_tile(color = "white") +
  facet_grid(. ~ Week, scales = "free_x", space = "free_x") +
  scale_fill_gradient2(high = "red") +
  labs(
    x = "Health Board",
    y = "Hour",
    fill = "Attendances"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(hjust = 1, size = 7),
    strip.placement = "outside",
    strip.background = element_blank()
  )
ggsave("outputs/plot_arrival_hour_facet.png", p6, width = 12, height = 6, dpi = 300)

# ==================================================
# 7. Age Group – Heatmap
# ==================================================

ageGroup1823 <- read_excel("2025-05-06-whoattends-agegroup.xlsx", sheet = "Health Board") %>%
  filter(Month >= as.Date("2018-01-01")) %>%
  mutate(Age = factor(Age, levels = c("Under 18", "18-24", "25-39", "40-64", "65-74", "75 plus")))

ageGroup1823 <- aggregate(Attendances ~ HealthBoard + Age, data = ageGroup1823, sum)

p7 <- ggplot(ageGroup1823, aes(x = HealthBoard, y = fct_rev(Age), fill = Attendances)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(high = "red", labels = label_comma()) +
  labs(title = "Attendances by Age Group and Health Board", x = "Health Board", y = "Age Group", fill = "Attendances") +
  theme_minimal()
ggsave("outputs/plot_agegroup_heatmap.png", p7, width = 8, height = 6, dpi = 300)

#Alternative presentation of heatmap with flipped axes
p7_inverse <-  ggplot(ageGroup1823, aes(x = fct_rev(Age), y = HealthBoard , fill = Attendances)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(high = "red", labels = label_comma()) +
  labs(title = "Attendances by Age Group and Health Board", x = "Health Board", y = "Age Group", fill = "Attendances") +
  theme_minimal()
ggsave("outputs/plot_agegroup_heatmap_inverse.png", p7_inverse, width = 8, height = 6, dpi = 300)

# ==================================================
# 8. Sex – Pie Chart
# ==================================================

sex1823 <- read_excel("2025-05-06-whoattends-sex.xlsx", sheet = "Health Board") %>%
  filter(Month >= as.Date("2018-01-01"))

sex_summary <- aggregate(Attendances ~ Sex, data = sex1823, sum) %>%
  mutate(
    fraction = Attendances / sum(Attendances),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, -1))
  )

p8 <- ggplot(sex_summary, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Sex)) +
  geom_rect() +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  xlim(c(2, 4)) +
  labs(title = "5-Year Attendances by Sex", fill = "Sex") +
  theme_void()
ggsave("outputs/plot_sex_pie.png", p8, width = 6, height = 6, dpi = 300)

# ==================================================
# 8.1 Sex – Horizontal Bar Chart
# ==================================================
p8.1 <- ggplot(sex1823) +
  geom_col(aes(y = reorder(Sex, Attendances), x = Attendances, fill = Sex)) +
  scale_fill_brewer(palette = "Blues") +
  scale_x_continuous(label = label_comma())+
  labs(x = "Attendances", y = "Sex", fill = "Sex",) +
  guides(fill = FALSE) +
  theme_minimal()

ggsave("outputs/plot_sex.png", p8.1, width = 10, height = 6, dpi = 300)

# ==================================================
# 9. Deprivation – Heatmap
# ==================================================

deprivation1823 <- read_excel("2025-05-06-whoattends-deprivation.xlsx", sheet = "Health Board") %>%
  filter(Month >= as.Date("2018-01-01"))

deprivation1823 <- aggregate(Attendances ~ HealthBoard + Deprivation, data = deprivation1823, sum)

p9 <- ggplot(deprivation1823, aes(x = HealthBoard, y = fct_rev(Deprivation), fill = Attendances)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(high = "red", labels = label_comma()) +
  labs(x = "Health Board", y = "Deprivation Level", fill = "Attendances") +
  theme_minimal()
ggsave("outputs/plot_deprivation_heatmap.png", p9, width = 8, height = 6, dpi = 300)

# ==================================================
# --- DONE ---
# ==================================================