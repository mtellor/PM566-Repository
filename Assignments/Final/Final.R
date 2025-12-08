# Set-up ----
# Packages
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(leaflet)
library(knitr)
library(tidyr)
library(plotly)

# Read the data (adjust the filename/path as needed)
ed <- read_csv("~/Library/CloudStorage/GoogleDrive-tellorin@usc.edu/.shortcut-targets-by-id/10yI1Vp2x44iBX7T-_NfWNeL7go8kwnUH/2. College - USC/1. Degree/1. Courses/Y4 Senior/Fall 2025/PM 566/PM566-Repository/Assignments/Final/data/data.csv")

# EDA 1 & 2 ----
dim(ed)
names(ed)[1:10]

# EDA 3 & 4 ----
# structure
str(ed)
names(ed)
head(ed)

# Clean Data ----
colSums(is.na(ed))

ed_clean <- ed %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>%

colSums(is.na(ed_clean))

numeric_vars <- sapply(ed_clean, is.numeric)
colSums(ed_clean[, numeric_vars] < 0, na.rm = TRUE)

names(ed)

ed_slim <- ed_clean %>%
  select(
    lat, long, FACILITY_NAME, DBA_CITY, DBA_ZIP_CODE,
    LICENSED_BED_SIZE, SENATE_DISTRICT_DESC, ASSEMBLY_DISTRICT_DESC,
    MSSA_NAME, MSSA_DESIGNATION,
    # Dispositions
    disp_Acute_Care, disp_Against_Medical_Advice, disp_Childrens_or_Cancer,
    disp_Died, disp_Home_Health_Service, disp_Not_Defined_Elsewhere,
    disp_Prison_Jail, disp_Psychiatric_Care, disp_Routine, disp_SN_IC_Care,
    disp_Hospice_Care, disp_Residential_Care, disp_CAH, disp_Rehab,
    disp_Other, disp_Disaster_Care_Site,
    # Zip categories
    Zip_CA_Resident, Zip_Homeless, Zip_Out_of_State, Zip_Unknown, Zip_Foreign,
    # Sex
    Sex_Female, Sex_Male, Sex_Other_Unknown,
    # Age
    Age_0_09, Age_10_19, Age_20_29, Age_30_39, Age_40_49,
    Age_50_59, Age_60_69, Age_70_79, Age_80_,
    # Payer
    Payer_All_Other_Payers, Payer_MediCal, Payer_Medicare,
    Payer_Other_Government, Payer_Other_Unknown,
    Payer_Private_Health_Insurance, Payer_Self_Pay_or_Uninsured,
    # Race/Ethnicity
    racegrp_aman, racegrp_asian, racegrp_black, racegrp_multirace,
    racegrp_nhpi, racegrp_other, racegrp_unknown, racegrp_white,
    eth_Hispanic, eth_NonHispanic, eth_Other_Unknown,
    # Language
    All_Other_Languages, English, Spanish,
    # Diagnosis groups
    dx_Circulatory, dx_Congenital, dx_Digestive, dx_Diseases_of_the_Blood,
    dx_Ear, dx_Endocrine, dx_Eye, dx_Factors_Influencing_Health,
    dx_Genitourinary, dx_Infectious, dx_Injury_Poisoning, dx_MentalHealth,
    dx_Musculoskeletal, dx_Neoplasms, dx_Nervous_System, dx_Other_Unknown,
    dx_Pregnancy_Childbirth, dx_Respiratory, dx_Skin, dx_Symptoms_Signs_NEC,
    dx_Certain_Perinatal_Conditions,
    # Emergency treatment stations
    EMER_MED_TREAT_STATIONS)

dim(ed_slim)
names(ed_slim)
summary(ed_slim)

ed_slim <- ed_slim %>%
  clean_names()

names(ed_slim)

ed_slim <- ed_slim %>%
  rename(
    facility = facility_name,
    city = dba_city,
    zip = dba_zip_code,
    ipt_beds = licensed_bed_size,
    disp_ama = disp_against_medical_advice,
    disp_routine = disp_routine,
    disp_acute = disp_acute_care,
    disp_psych = disp_psychiatric_care,
    disp_snf = disp_sn_ic_care,
    disp_death = disp_died,
    payer_medical = payer_medi_cal,
    payer_medicare = payer_medicare,
    payer_private = payer_private_health_insurance,
    payer_uninsured = payer_self_pay_or_uninsured,
    dx_injury = dx_injury_poisoning,
    dx_mental = dx_mental_health,
    dx_resp = dx_respiratory,
    dx_cardio = dx_circulatory,
    dx_sx = dx_symptoms_signs_nec,
    dx_gu = dx_genitourinary,
    dx_gi = dx_digestive,
    dx_ortho = dx_musculoskeletal,
    dx_nervous = dx_nervous_system,
    dx_ob = dx_pregnancy_childbirth,
    dx_nicu = dx_certain_perinatal_conditions,
    ed_beds = emer_med_treat_stations)

names(ed_slim)

ed_slim <- ed_slim %>%
  select(-disp_cah, -disp_rehab, -disp_disaster_care_site)

names(ed_slim)

# total dispos col
ed_slim <- ed_slim %>%
  mutate(total_disp = disp_acute + disp_ama + disp_childrens_or_cancer +
           disp_death + disp_home_health_service + disp_not_defined_elsewhere +
           disp_prison_jail + disp_psych + disp_routine + disp_snf +
           disp_hospice_care + disp_residential_care + disp_other)

names(ed_slim)

# NO key dispo props
ed_slim <- ed_slim %>%
  mutate(ama_rate   = disp_ama / total_disp,
    psych_rate = disp_psych / total_disp,
    snf_rate   = disp_snf / total_disp,
    death_rate = disp_death / total_disp)


# NO payer mix proportions
ed_slim <- ed_slim %>%
  mutate(
    total_payer = payer_all_other_payers + payer_medical + payer_medicare +
      payer_other_government + payer_other_unknown +
      payer_private + payer_uninsured,
    payer_medical_prop   = payer_medical / total_payer,
    payer_medicare_prop  = payer_medicare / total_payer,
    payer_private_prop   = payer_private / total_payer,
    payer_uninsured_prop = payer_uninsured / total_payer)

# NO dx group props
ed_slim <- ed_slim %>%
  mutate(
    dx_cardio_group = dx_cardio,
    dx_resp_group   = dx_resp,
    dx_injury_group = dx_injury,
    dx_mental_group = dx_mental,
    dx_other_group  = dx_gi + dx_diseases_of_the_blood + dx_ear + dx_endocrine +
      dx_eye + dx_factors_influencing_health + dx_gu +
      dx_infectious + dx_ortho + dx_neoplasms + dx_nervous +
      dx_other_unknown + dx_ob + dx_skin + dx_sx +
      dx_congenital + dx_nicu,
    total_dx = dx_cardio_group + dx_resp_group + dx_injury_group +
      dx_mental_group + dx_other_group,
    dx_cardio_prop = dx_cardio_group / total_dx,
    dx_resp_prop   = dx_resp_group / total_dx,
    dx_injury_prop = dx_injury_group / total_dx,
    dx_mental_prop = dx_mental_group / total_dx,
    dx_other_prop  = dx_other_group / total_dx)


str(ed_slim)
names(ed_slim)
head(ed_slim)

colSums(is.na(ed_slim))



# Exploration ----

# Location of hosp (map)
leaflet(data = ed_slim) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addAwesomeMarkers(
    ~long, ~lat,
    popup = ~facility,
    label = ~facility)

hospital_colors <- c(
  "PIH Health Downey Hospital" = "darkred",
  "PIH Health Good Samaritan Hospital" = "dodgerblue4",
  "PIH Health Whittier Hospital" = "gold3")


# ipt_beds per hosp
ipt_summary <- ed_slim %>%
  select(facility, ipt_beds)

ggplot(ed_slim, aes(x = facility, y = ipt_beds, fill = facility)) +
  geom_col() +
  geom_text(aes(label = ipt_beds), vjust = -0.4, color = "black", size = 3.5) +
  scale_fill_manual(values = hospital_colors) +
  labs(title = "Licensed Inpatient Beds per Hospital",
       x = "Hospital",
       y = "Number of Licensed Inpatient Beds") +
  theme_bw()


# ed_beds per hosp
ed_summary <- ed_slim %>%
  select(facility, ed_beds)

ggplot(ed_slim, aes(x = facility, y = ed_beds, fill = facility)) +
  geom_col() +
  geom_text(aes(label = ed_beds), vjust = -0.4, color = "black", size = 3.5) +
  scale_fill_manual(values = hospital_colors) +
  labs(title = "Licensed ED Beds per Hospital",
       x = "Hospital",
       y = "Number of Licensed ED Beds") +
  theme_bw()


# USING total dispos per hosp
ggplot(ed_slim, aes(x = facility, y = total_disp, fill = facility)) +
  geom_col() +
  geom_text(aes(label = total_disp), vjust = -0.4, color = "black", size = 3.5) +
  scale_fill_manual(values = c(
    "PIH Health Downey Hospital" = "darkred",
    "PIH Health Good Samaritan Hospital" = "dodgerblue4",
    "PIH Health Whittier Hospital" = "gold3"
  )) +
  labs(title = "Total Dispositions per Hospital",
       x = "Hospital",
       y = "Total Dispositions") +
  theme_bw()


# NO key dispo: ama
ggplot(ed_slim, aes(x = facility, y = disp_ama)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = disp_ama), vjust = -0.4, color = "black", size = 3.5) +
  labs(title = "AMA Dispositions per Hospital",
       x = "Hospital",
       y = "Number of AMA Dispositions") +
  theme_bw()


# NO key dispo: psych
ggplot(ed_slim, aes(x = facility, y = disp_psych)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = disp_psych), vjust = -0.4, color = "black", size = 3.5) +
  labs(title = "Psychiatric Dispositions per Hospital",
       x = "Hospital",
       y = "Number of Psychiatric Dispositions") +
  theme_bw()


# NO key dispo: SNF
ggplot(ed_slim, aes(x = facility, y = disp_snf)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = disp_snf), vjust = -0.4, color = "black", size = 3.5) +
  labs(title = "SNF Dispositions per Hospital",
       x = "Hospital",
       y = "Number of SNF Dispositions") +
  theme_bw()


# NO key dispo: death
ggplot(ed_slim, aes(x = facility, y = disp_death)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = disp_death), vjust = -0.4, color = "black", size = 3.5) +
  labs(title = "Death Dispositions per Hospital",
       x = "Hospital",
       y = "Number of Death Dispositions") +
  theme_bw()


# USING key dispos combo: grouped bar chart
dispo_counts <- ed_slim %>%
  select(facility,
         disp_ama,
         disp_psych,
         disp_snf,
         disp_death) %>%
  rename(
    AMA   = disp_ama,
    Psych = disp_psych,
    SNF   = disp_snf,
    Death = disp_death
  ) %>%
  pivot_longer(cols = -facility,
               names_to = "disposition",
               values_to = "count")

plot_ly(dispo_counts,
        x = ~disposition,
        y = ~count,
        color = ~facility,
        colors = hospital_colors,
        type = 'bar') %>%
  layout(barmode = 'group',
         title = "Key Dispositions per Hospital",
         xaxis = list(title = "Disposition Type"),
         yaxis = list(title = "Number of Dispositions"))


# key dispos: summary table
disp_summary <- ed_slim %>%
  mutate(total_disp = disp_acute + disp_ama + disp_childrens_or_cancer + disp_death +
      disp_home_health_service + disp_not_defined_elsewhere +
      disp_prison_jail + disp_psych + disp_routine + disp_snf +
      disp_hospice_care + disp_residential_care + disp_other,
    ama_rate   = disp_ama / total_disp,
    psych_rate = disp_psych / total_disp,
    snf_rate   = disp_snf / total_disp,
    death_rate = disp_death / total_disp) %>%
  select(facility, total_disp, ama_rate, psych_rate, snf_rate, death_rate)

kable(disp_summary, digits = 3,
      col.names = c("Hospital", "Total Dispositions",
                    "AMA Rate", "Psych Rate", "SNF Rate", "Death Rate"))


# payer mix: downey
downey_payer_counts <- ed_slim %>%
  filter(facility == "PIH Health Downey Hospital") %>%
  select(facility,
         payer_medical,
         payer_medicare,
         payer_private,
         payer_uninsured,
         payer_all_other_payers,
         payer_other_government,
         payer_other_unknown) %>%
  rename(
    Medical   = payer_medical,
    Medicare  = payer_medicare,
    Private   = payer_private,
    Uninsured = payer_uninsured,
    All_Other = payer_all_other_payers,
    Other_Gov = payer_other_government,
    Unknown   = payer_other_unknown
  ) %>%
  pivot_longer(cols = -facility,
               names_to = "payer_type",
               values_to = "count")

ggplot(downey_payer_counts, aes(x = payer_type, y = count)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = count), vjust = -0.4, color = "black", size = 3.5) +
  labs(title = "Payer Mix – PIH Health Downey Hospital",
       x = "Payer Type",
       y = "Number of Dispositions") +
  theme_bw()


# payer mix: good sam
goodsam_payer_counts <- ed_slim %>%
  filter(facility == "PIH Health Good Samaritan Hospital") %>%
  select(facility,
         payer_medical,
         payer_medicare,
         payer_private,
         payer_uninsured,
         payer_all_other_payers,
         payer_other_government,
         payer_other_unknown) %>%
  rename(
    Medical   = payer_medical,
    Medicare  = payer_medicare,
    Private   = payer_private,
    Uninsured = payer_uninsured,
    All_Other = payer_all_other_payers,
    Other_Gov = payer_other_government,
    Unknown   = payer_other_unknown
  ) %>%
  pivot_longer(cols = -facility,
               names_to = "payer_type",
               values_to = "count")

ggplot(goodsam_payer_counts, aes(x = payer_type, y = count)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = count), vjust = -0.4, color = "black", size = 3.5) +
  labs(title = "Payer Mix – PIH Health Good Samaritan Hospital",
       x = "Payer Type",
       y = "Number of Dispositions") +
  theme_bw()


# payer mix: whittier
whittier_payer_counts <- ed_slim %>%
  filter(facility == "PIH Health Whittier Hospital") %>%
  select(facility,
         payer_medical,
         payer_medicare,
         payer_private,
         payer_uninsured,
         payer_all_other_payers,
         payer_other_government,
         payer_other_unknown) %>%
  rename(
    Medical   = payer_medical,
    Medicare  = payer_medicare,
    Private   = payer_private,
    Uninsured = payer_uninsured,
    All_Other = payer_all_other_payers,
    Other_Gov = payer_other_government,
    Unknown   = payer_other_unknown
  ) %>%
  pivot_longer(cols = -facility,
               names_to = "payer_type",
               values_to = "count")

ggplot(whittier_payer_counts, aes(x = payer_type, y = count)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = count), vjust = -0.4, color = "black", size = 3.5) +
  labs(title = "Payer Mix – PIH Health Whittier Hospital",
       x = "Payer Type",
       y = "Number of Dispositions") +
  theme_bw()


# payer mix combo: table
payer_table <- ed_slim %>%
  select(facility,
         payer_medical,
         payer_medicare,
         payer_private,
         payer_uninsured,
         payer_all_other_payers,
         payer_other_government,
         payer_other_unknown) %>%
  rename(
    Medical   = payer_medical,
    Medicare  = payer_medicare,
    Private   = payer_private,
    Uninsured = payer_uninsured,
    All_Other = payer_all_other_payers,
    Other_Gov = payer_other_government,
    Unknown   = payer_other_unknown)

kable(payer_table,
      col.names = c("Hospital", "Medical", "Medicare", "Private", "Uninsured",
                    "All Other", "Other Gov", "Unknown"),
      align = c("l", rep("c", 7)))


# USING payer mix: combo grouped bar chart
payer_counts <- ed_slim %>%
  select(facility,
         payer_medical,
         payer_medicare,
         payer_private,
         payer_uninsured,
         payer_all_other_payers,
         payer_other_government,
         payer_other_unknown) %>%
  rename(
    Medical   = payer_medical,
    Medicare  = payer_medicare,
    Private   = payer_private,
    Uninsured = payer_uninsured,
    All_Other = payer_all_other_payers,
    Other_Gov = payer_other_government,
    Unknown   = payer_other_unknown
  ) %>%
  pivot_longer(cols = -facility,
               names_to = "payer_type",
               values_to = "count")

plot_ly(payer_counts,
        x = ~payer_type,
        y = ~count,
        color = ~facility,
        colors = hospital_colors,
        type = 'bar') %>%
  layout(barmode = 'group',
         title = "Payer Mix – All Hospitals",
         xaxis = list(title = "Payer Type", tickangle = 45),
         yaxis = list(title = "Number of Dispositions"))


# dx cat: downey
downey_dx_counts <- ed_slim %>%
  filter(facility == "PIH Health Downey Hospital") %>%
  select(facility,
         dx_cardio,
         dx_resp,
         dx_injury,
         dx_mental,
         dx_other_unknown) %>%
  rename(
    Cardio       = dx_cardio,
    Respiratory  = dx_resp,
    Injury       = dx_injury,
    Mental       = dx_mental,
    Other_Unknown = dx_other_unknown
  ) %>%
  pivot_longer(cols = -facility,
               names_to = "dx_category",
               values_to = "count")

ggplot(downey_dx_counts, aes(x = dx_category, y = count)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = count), vjust = -0.4, color = "black", size = 3.5) +
  labs(title = "Diagnosis Categories – PIH Health Downey Hospital",
       x = "Diagnosis Category",
       y = "Number of Dispositions") +
  theme_bw()


# dx cat: good sam
goodsam_dx_counts <- ed_slim %>%
  filter(facility == "PIH Health Good Samaritan Hospital") %>%
  select(facility,
         dx_cardio,
         dx_resp,
         dx_injury,
         dx_mental,
         dx_other_unknown) %>%
  rename(
    Cardio       = dx_cardio,
    Respiratory  = dx_resp,
    Injury       = dx_injury,
    Mental       = dx_mental,
    Other_Unknown = dx_other_unknown
  ) %>%
  pivot_longer(cols = -facility,
               names_to = "dx_category",
               values_to = "count")

ggplot(goodsam_dx_counts, aes(x = dx_category, y = count)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = count), vjust = -0.4, color = "black", size = 3.5) +
  labs(title = "Diagnosis Categories – PIH Health Good Samaritan Hospital",
       x = "Diagnosis Category",
       y = "Number of Dispositions") +
  theme_bw()


# dx cat: whittier
whittier_dx_counts <- ed_slim %>%
  filter(facility == "PIH Health Whittier Hospital") %>%
  select(facility,
         dx_cardio,
         dx_resp,
         dx_injury,
         dx_mental,
         dx_other_unknown) %>%
  rename(
    Cardio       = dx_cardio,
    Respiratory  = dx_resp,
    Injury       = dx_injury,
    Mental       = dx_mental,
    Other_Unknown = dx_other_unknown
  ) %>%
  pivot_longer(cols = -facility,
               names_to = "dx_category",
               values_to = "count")

ggplot(whittier_dx_counts, aes(x = dx_category, y = count)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = count), vjust = -0.4, color = "black", size = 3.5) +
  labs(title = "Diagnosis Categories – PIH Health Whittier Hospital",
       x = "Diagnosis Category",
       y = "Number of Dispositions") +
  theme_bw()


# dx cat combo: stacked bar chart
dx_counts <- ed_slim %>%
  select(facility,
         dx_cardio,
         dx_resp,
         dx_injury,
         dx_mental,
         dx_other_unknown) %>%
  rename(
    Cardio       = dx_cardio,
    Respiratory  = dx_resp,
    Injury       = dx_injury,
    Mental       = dx_mental,
    Other_Unknown = dx_other_unknown
  ) %>%
  pivot_longer(cols = -facility,
               names_to = "dx_category",
               values_to = "count")

ggplot(dx_counts, aes(x = facility, y = count, fill = dx_category)) +
  geom_col() +
  labs(title = "Diagnosis Categories – All Hospitals",
       x = "Hospital",
       y = "Number of Dispositions") +
  scale_fill_manual(values = c(
    "Cardio" = "red3",
    "Respiratory" = "dodgerblue3",
    "Injury" = "gold2",
    "Mental" = "green3",
    "Other_Unknown" = "purple2"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


# USING dx cat combo: grouped bar chart
dx_counts <- ed_slim %>%
  select(facility,
         dx_cardio,
         dx_resp,
         dx_injury,
         dx_mental,
         dx_other_unknown) %>%
  rename(
    Cardio       = dx_cardio,
    Respiratory  = dx_resp,
    Injury       = dx_injury,
    Mental       = dx_mental,
    Other_Unknown = dx_other_unknown
  ) %>%
  pivot_longer(cols = -facility,
               names_to = "dx_category",
               values_to = "count")

plot_ly(dx_counts,
        x = ~dx_category,
        y = ~count,
        color = ~facility,
        colors = hospital_colors,
        type = 'bar') %>%
  layout(barmode = 'group',
         title = "Diagnosis Categories – All Hospitals",
         xaxis = list(title = "Diagnosis Category", tickangle = 45),
         yaxis = list(title = "Number of Dispositions"))


# age distribution combo
age_counts <- ed_slim %>%
  select(facility,
         age_0_09,
         age_10_19,
         age_20_29,
         age_30_39,
         age_40_49,
         age_50_59,
         age_60_69,
         age_70_79,
         age_80) %>%
  rename(
    `0-9`   = age_0_09,
    `10-19` = age_10_19,
    `20-29` = age_20_29,
    `30-39` = age_30_39,
    `40-49` = age_40_49,
    `50-59` = age_50_59,
    `60-69` = age_60_69,
    `70-79` = age_70_79,
    `80+`   = age_80
  ) %>%
  pivot_longer(cols = -facility,
               names_to = "age_group",
               values_to = "count")

plot_ly(age_counts,
        x = ~age_group,
        y = ~count,
        color = ~facility,
        colors = hospital_colors,
        type = 'scatter',
        mode = 'lines+markers',
        marker = list(size = 8),
        line = list(width = 3)) %>%
  layout(title = "Age Distribution – All Hospitals",
         xaxis = list(title = "Age Group", tickangle = 45),
         yaxis = list(title = "Count"))


# normalize dispos
dispo_props <- ed_slim %>%
  mutate(
    AMA_prop   = disp_ama   / total_disp,
    Psych_prop = disp_psych / total_disp,
    SNF_prop   = disp_snf   / total_disp,
    Death_prop = disp_death / total_disp
  ) %>%
  select(facility, AMA_prop, Psych_prop, SNF_prop, Death_prop)

# normolize payer mix
payer_props <- ed_slim %>%
  mutate(total_payer = payer_medical + payer_medicare + payer_private +
           payer_uninsured + payer_all_other_payers +
           payer_other_government + payer_other_unknown) %>%
  mutate(
    Medical_prop   = payer_medical   / total_payer,
    Medicare_prop  = payer_medicare  / total_payer,
    Private_prop   = payer_private   / total_payer,
    Uninsured_prop = payer_uninsured / total_payer,
    AllOther_prop  = payer_all_other_payers / total_payer,
    OtherGov_prop  = payer_other_government / total_payer,
    Unknown_prop   = payer_other_unknown / total_payer
  ) %>%
  select(facility, Medical_prop, Medicare_prop, Private_prop,
         Uninsured_prop, AllOther_prop, OtherGov_prop, Unknown_prop)

# normalize dx cats
dx_props <- ed_slim %>%
  mutate(total_dx = dx_cardio + dx_resp + dx_injury + dx_mental + dx_other_unknown) %>%
  mutate(
    Cardio_prop      = dx_cardio / total_dx,
    Respiratory_prop = dx_resp   / total_dx,
    Injury_prop      = dx_injury / total_dx,
    Mental_prop      = dx_mental / total_dx,
    Other_prop       = dx_other_unknown / total_dx
  ) %>%
  select(facility, Cardio_prop, Respiratory_prop, Injury_prop, Mental_prop, Other_prop)

# normalize age
age_props <- ed_slim %>%
  mutate(total_age = age_0_09 + age_10_19 + age_20_29 + age_30_39 +
           age_40_49 + age_50_59 + age_60_69 + age_70_79 + age_80) %>%
  mutate(
    age_0_09_prop  = age_0_09  / total_age,
    age_10_19_prop = age_10_19 / total_age,
    age_20_29_prop = age_20_29 / total_age,
    age_30_39_prop = age_30_39 / total_age,
    age_40_49_prop = age_40_49 / total_age,
    age_50_59_prop = age_50_59 / total_age,
    age_60_69_prop = age_60_69 / total_age,
    age_70_79_prop = age_70_79 / total_age,
    age_80_prop    = age_80    / total_age
  ) %>%
  select(facility, age_0_09_prop, age_10_19_prop, age_20_29_prop,
         age_30_39_prop, age_40_49_prop, age_50_59_prop,
         age_60_69_prop, age_70_79_prop, age_80_prop)



# USING Disposition proportions
dispo_long <- dispo_props %>%
  rename(
    `AMA` = AMA_prop,
    `Psych Transfer`   = Psych_prop,
    `SNF`        = SNF_prop,
    `Death`                  = Death_prop
  ) %>%
  pivot_longer(cols = c(`AMA`, 
                        `Psych Transfer`, 
                        `SNF`, 
                        `Death`),
               names_to = "disposition",
               values_to = "proportion")

plot_ly(dispo_long,
        x = ~disposition,
        y = ~proportion,
        color = ~facility,
        colors = hospital_colors,
        type = 'bar') %>%
  layout(barmode = 'group',
         title = "Disposition Proportions – All Hospitals",
         xaxis = list(title = "Disposition Type", tickangle = 45),
         yaxis = list(title = "Proportion of Total Dispositions"))


# USING payer mix prop
payer_long <- payer_props %>%
  rename(
    `Medi-Cal`          = Medical_prop,
    `Medicare`          = Medicare_prop,
    `Private Insurance` = Private_prop,
    `Uninsured`         = Uninsured_prop,
    `All Other Payers`  = AllOther_prop,
    `Other Government`  = OtherGov_prop,
    `Unknown`           = Unknown_prop
  ) %>%
  pivot_longer(cols = c(`Medi-Cal`, `Medicare`, `Private Insurance`,
                        `Uninsured`, `All Other Payers`,
                        `Other Government`, `Unknown`),
               names_to = "payer_type",
               values_to = "proportion")

plot_ly(payer_long,
        x = ~payer_type,
        y = ~proportion,
        color = ~facility,
        colors = hospital_colors,
        type = 'bar') %>%
  layout(barmode = 'group',
         title = "Payer Mix Proportions – All Hospitals",
         xaxis = list(title = "Payer Type", tickangle = 45),
         yaxis = list(title = "Proportion of Total Payer Mix"))


# USING dx cat prop
dx_long <- dx_props %>%
  rename(
    `Cardiovascular`     = Cardio_prop,
    `Respiratory`        = Respiratory_prop,
    `Injury/Trauma`      = Injury_prop,
    `Mental Health`      = Mental_prop,
    `Other/Unknown`      = Other_prop
  ) %>%
  pivot_longer(cols = c(`Cardiovascular`, `Respiratory`,
                        `Injury/Trauma`, `Mental Health`,
                        `Other/Unknown`),
               names_to = "dx_category",
               values_to = "proportion")

plot_ly(dx_long,
        x = ~dx_category,
        y = ~proportion,
        color = ~facility,
        colors = hospital_colors,
        type = 'bar') %>%
  layout(barmode = 'group',
         title = "Diagnosis Category Proportions – All Hospitals",
         xaxis = list(title = "Diagnosis Category", tickangle = 45),
         yaxis = list(title = "Proportion of Total Diagnoses"))


# USING age distribition prop
age_long <- age_props %>%
  rename(
    `0–9 years`   = age_0_09_prop,
    `10–19 years` = age_10_19_prop,
    `20–29 years` = age_20_29_prop,
    `30–39 years` = age_30_39_prop,
    `40–49 years` = age_40_49_prop,
    `50–59 years` = age_50_59_prop,
    `60–69 years` = age_60_69_prop,
    `70–79 years` = age_70_79_prop,
    `80+ years`   = age_80_prop
  ) %>%
  pivot_longer(cols = c(`0–9 years`, `10–19 years`, `20–29 years`,
                        `30–39 years`, `40–49 years`, `50–59 years`,
                        `60–69 years`, `70–79 years`, `80+ years`),
               names_to = "age_group",
               values_to = "proportion")

plot_ly(age_long,
        x = ~age_group,
        y = ~proportion,
        color = ~facility,
        colors = hospital_colors,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 3)) %>%
  layout(title = "Age Distribution Proportions – All Hospitals",
         xaxis = list(title = "Age Group", tickangle = 45),
         yaxis = list(title = "Proportion of Total Patients"))


