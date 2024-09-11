# Load necessary libraries
library(tidyverse)
library(janitor)
library(readxl)  # For reading Excel files
library(glue)

color_scheme <- c('#66000080', '#022B3A80','#66666680')


rnd_like_xl <- function(x, y) {
  round2 <- ifelse(x >= 0, round(x + 1e-09, y), round(x - 1e-09, y))
  return(round2)
}

`%out%` <- Negate(`%in%`)


metro_districts <-
  c(
    "Cherokee County",
    "Cobb County",
    "DeKalb County",
    "Douglas County",
    "Fayette County",
    "Clayton County",
    "Forsyth County",
    "Fulton County",
    "Gwinnett County",
    "Rockdale County",
    "Atlanta Public Schools",
    "Buford City",
    "Decatur City",
    "Marietta City"
  )

# Define the URLs of the datasets
url1 <- "https://download.gosa.ga.gov/2023/EOG_by_GRADE_2022-23__GA_TST_AGGR_2023-12-15_18_54_21.csv"
url2 <- "https://download.gosa.ga.gov/2023/Enrollment_by_Subgroup_Metrics_2022-23_2023-12-15_18_54_53.csv"
url3 <- "https://download.gosa.ga.gov/2023/Attendance_2022-23_2023-12-15_18_55_15.csv"
url4 <- "https://download.gosa.ga.gov/2023/2023_directly_certified_school.xls"
url5 <- "https://download.gosa.ga.gov/2023/2023_School_Mobility_for_Display.xls"
url6 <- "https://download.gosa.ga.gov/2023/2023_directly_certified_district.xls"
url7 <- "https://download.gosa.ga.gov/2023/2023_District_Mobility_for_Display.xls"

# Download the datasets
download.file(url1, destfile = "EOG_by_GRADE_2022-23.csv")
download.file(url2, destfile = "Enrollment_by_Subgroup_Metrics_2022-23.csv")
download.file(url3, destfile = "Attendance_2022-23.csv")
download.file(url4, destfile = "Directly_Certified_School_2023.xls")
download.file(url5, destfile = "School_Mobility.xls")
download.file(url6, destfile = "Directly_Certified_District_2023.xls")
download.file(url7, destfile = "District_Mobility.xls")

# Read the datasets into R
data1 <- read_csv("EOG_by_GRADE_2022-23.csv")
data2 <- read_csv("Enrollment_by_Subgroup_Metrics_2022-23.csv")
data3 <- read_csv("Attendance_2022-23.csv")
data4 <- read_excel("Directly_Certified_School_2023.xls")
data5 <- read_excel("School_Mobility.xls")
data6 <- read_excel("Directly_Certified_District_2023.xls")
data7 <- read_excel("District_Mobility.xls")

# Display the first few rows of each dataset
head(data1)
head(data2)
head(data3)
head(data4)
head(data5)
head(data6)
head(data7)

# Basic data cleaning
# Clean column names and convert relevant columns to numeric where appropriate
cleaned_data1 <- data1 |> 
  clean_names() |> 
  mutate(across(ends_with('_pct'), ~as.double(.x))) |> 
  mutate(across(ends_with('_cnt'), ~as.double(.x)))

cleaned_data2 <- data2 |> 
  clean_names() |> 
  mutate(across(contains(c('_pct', '_cnt', '_count', '_percent')), ~as.double(.x)))

cleaned_data3 <- data3 |>
  select(
    DETAIL_LVL_DESC,
    SCHOOL_DSTRCT_CD,
    SCHOOL_DSTRCT_NM,
    INSTN_NAME,
    INSTN_NUMBER,
    STUDENT_COUNT_ALL,
    CHRONIC_ABSENT_PERC_ALL
  ) |>
  clean_names() |> 
  mutate(across(contains(c('_pct', '_cnt', '_count', '_perc')), ~as.double(.x)))

cleaned_data4 <- data4 |> 
  clean_names() |> 
  mutate(
    sys_sch = str_trim(as.character(sys_sch), "both"),
    school_id = str_trim(sprintf("%04i",school_id),"both"),
    system_id = str_trim(as.character(system_id),"both"),
  ) |> 
  mutate(across(contains(c('_pct', '_ct', '_count', '_perc')), ~as.double(.x)))



cleaned_data5 <- data5 |> 
  clean_names() |>
  mutate(sys_sch = str_trim(as.character(sys_sch), "both")) |> 
  mutate(mobility = as.double(mobility))

cleaned_data6 <- data6 |> 
  clean_names() |> 
  mutate(system_id   = str_trim(sprintf("%03i",system_id ),"both")) |> 
  mutate(across(contains(c('_pct', '_cnt', '_count', '_perc')), ~as.double(.x)))

cleaned_data7 <- data7 |> 
  clean_names() |> 
  mutate(school_district_cd  = str_trim(sprintf("%03i",school_district_cd),"both")) 
# mutate(across(contains(c('_pct', '_cnt', '_count', '_perc')), ~as.double(.x)))

# Display the structure of the cleaned datasets
str(cleaned_data1)
str(cleaned_data2)
str(cleaned_data3)
str(cleaned_data4)
str(cleaned_data5)
str(cleaned_data6)
str(cleaned_data7)

# Merge the datasets
merged_data <- cleaned_data1 |> 
  inner_join(
    cleaned_data2,
    by = join_by(
      school_distrct_cd == school_dstrct_cd,
      instn_number == instn_number,
      school_dstrct_nm == school_dstrct_nm,
      instn_name == instn_name,
      long_school_year == long_school_year
    )
  ) |> 
  inner_join(
    cleaned_data3,
    by = join_by(
      school_distrct_cd == school_dstrct_cd,
      instn_number == instn_number,
      school_dstrct_nm == school_dstrct_nm,
      instn_name == instn_name
    )
  ) |> 
  inner_join(
    cleaned_data4,
    by = join_by(
      school_distrct_cd == system_id,
      instn_number == school_id,
      school_dstrct_nm == system_name,
      instn_name == school_name
    )
  ) |> 
  inner_join(
    cleaned_data5,
    by = join_by(
      sys_sch == sys_sch,
      school_dstrct_nm == school_district_nm,
      instn_name == school_name
    )
  )


# create school-level dataset for graphing 
graph_data <-  merged_data |>
  filter(
    str_detect(school_distrct_cd, "^\\d{3}$") &
      school_distrct_cd %out% c("799", "891") &
      # Remove State & Charter Schools
      subgroup_name == "All Students" &
      test_cmpnt_typ_nm == "English Language Arts"
  ) |>
  select(
    school_distrct_cd,
    school_dstrct_nm,
    instn_number,
    instn_name,
    acdmc_lvl,
    subgroup_name,
    test_cmpnt_typ_nm,
    num_tested_cnt,
    begin_pct, 
    developing_pct,
    proficient_pct,
    distinguished_pct,
    enroll_pct_asian,
    enroll_pct_native,
    enroll_pct_black,
    enroll_pct_hispanic,
    enroll_pct_multiracial,
    enroll_pct_white,
    enroll_pct_migrant,
    enroll_pct_ed,
    enroll_pct_swd,
    enroll_pct_lep,
    enroll_pct_gifted,
    direct_cert_perc,
    chronic_absent_perc_all, 
    mobility
  ) 

# calculate % Proficient/Distinguished (levels 3 & 4): 
# create metro district flag

graph_data <- graph_data |>
  mutate(pct_prof_dis = proficient_pct + distinguished_pct) |>
  relocate(pct_prof_dis, .after = distinguished_pct) |>
  mutate(
    sys_group = case_when(
      school_distrct_cd == "667" ~ "GCPS",
      school_dstrct_nm %in% metro_districts & school_distrct_cd != "667" ~ "Metro Atlanta",
      TRUE ~ "Other GA System"
    )
  ) |> 
  mutate(sys_group = as.factor(sys_group))


# Round values for graphical display (tooltip values)
graph_data <- graph_data |>
  mutate(across(
    c(
      distinguished_pct,
      enroll_pct_asian,
      enroll_pct_native,
      enroll_pct_black,
      enroll_pct_hispanic,
      enroll_pct_multiracial,
      enroll_pct_white,
      enroll_pct_migrant,
      enroll_pct_ed,
      enroll_pct_swd,
      enroll_pct_lep,
      enroll_pct_gifted,
      direct_cert_perc,
      chronic_absent_perc_all,
      mobility
    ),
    ~ rnd_like_xl(replace_na(.x, 0), 1)
  ))


# Create column for tooltip: 
graph_data <- graph_data |> 
  mutate(
    tooltip_lbl = paste(
      instn_name,
      school_dstrct_nm,
      num_tested_cnt,
      enroll_pct_asian,
      enroll_pct_native,
      enroll_pct_black,
      enroll_pct_hispanic,
      enroll_pct_multiracial,
      enroll_pct_white,
      enroll_pct_migrant,
      enroll_pct_ed,
      enroll_pct_swd,
      enroll_pct_lep,
      enroll_pct_gifted,
      direct_cert_perc,
      chronic_absent_perc_all,
      mobility, 
      sep = ","
    )
  )

# Generate tooltip and other graphing options

graph_data <- graph_data |>
  mutate(
    grade_input = glue('Grade {sprintf("%1i",as.integer( acdmc_lvl))}'), 
    plt_title = glue(
      'Grade {sprintf("%1i",as.integer( acdmc_lvl))} {test_cmpnt_typ_nm} Performance'
    ),
    plt_subtitle = glue('{subgroup_name} School Year 2023-24'),
    txt_subj = glue(
      'School: {instn_name}',
      '\n',
      'System: {school_dstrct_nm}',
      '\n',
      'Performance Area: {test_cmpnt_typ_nm}',
      '\n',
      '# Tested: {prettyNum(num_tested_cnt, big.mark = ",")}',
      '\n',
      'Mobility Rate: {mobility}',
      '\n'
    ),
    color = case_when(
      sys_group == "GCPS" ~"#66000080", 
      sys_group == "Metro Atlanta" ~"#022B3A80", 
      TRUE ~ "#66666680")
  )



# Merge district-level datasets with the ela_sys dataframe
#create district-level dataset for graphing


# Get ELA District Results: 

sys_ela <- cleaned_data1 |>
  filter(
    (
      str_detect(school_distrct_cd, "^\\d{3}$") |
        school_distrct_cd == "ALL"
    ) &
      school_distrct_cd %out% c("799", "891") & 
      subgroup_name == "All Students" &
      test_cmpnt_typ_nm == "English Language Arts" &
      instn_number == "ALL" &
      instn_name == "ALL"
  ) |>
  mutate(across(
    c(school_dstrct_nm, instn_name),
    ~ case_when(
      school_distrct_cd  == "ALL" ~ "State of Georgia",
      TRUE ~ as.character(.x)
    )
  ))

sys_enr <-   cleaned_data2 |> 
  filter(
    detail_lvl_desc %in% c("District","State"), 
    str_detect(school_dstrct_cd, "^\\d{3}$") | school_dstrct_cd == "ALL" &
      school_dstrct_cd %out% c("799", "891") & 
      instn_number == "ALL" 
  ) |> 
  mutate(
    school_dstrct_nm = case_when(
      school_dstrct_cd == "999" ~ "State of Georgia",
      TRUE~ as.character(school_dstrct_nm))
  ) |> 
  mutate(instn_name = school_dstrct_nm)


sys_att <- cleaned_data3 |>
  filter(school_dstrct_cd %out% c("799", "891")) |> 
  filter(detail_lvl_desc %in% c("District", "State") &
           str_detect(school_dstrct_cd, "^\\d{3}$") | school_dstrct_cd == "ALL" &
           instn_number == "ALL" 
  ) |> 
  mutate(
    school_dstrct_nm = case_when(
      school_dstrct_cd == "999" ~ "State of Georgia",
      TRUE~ as.character(school_dstrct_nm))
  ) |> 
  mutate(instn_name = school_dstrct_nm)



sys_dc <- cleaned_data6 |>
  mutate(
    school_dstrct_cd = case_when(system_id == "999" ~ "ALL",
                                 TRUE ~ as.character(system_id)),
    instn_number = "ALL"
  ) |>
  mutate(
    school_dstrct_nm = system_name,
    school_dstrct_cd = system_id ,
    instn_name = system_name
  )

sys_dc <- sys_dc |>
  filter(system_id %out% c("799", "891")) |> 
  filter(
    str_detect(school_dstrct_cd, "^\\d{3}$") |
      school_dstrct_cd == "ALL" &
      instn_number == "ALL"
  )

sys_mob <- cleaned_data7 |> add_row(school_district_cd = "ALL", school_district_nm = "State of Georgia")



merged_data_sys <- sys_ela |>
  left_join(
    sys_enr,
    by = join_by(
      school_distrct_cd == school_dstrct_cd,
      # instn_name == instn_name,
      # instn_number == instn_number,
      school_dstrct_nm == school_dstrct_nm, 
      long_school_year == long_school_year )
  ) |> 
  left_join(
    sys_att,
    by = join_by(
      school_distrct_cd == school_dstrct_cd,
      # instn_number == instn_number,
      # instn_name == instn_name,
      school_dstrct_nm == school_dstrct_nm
    )
  ) |> 
  left_join(
    sys_dc, 
    by = join_by(
      school_distrct_cd == school_dstrct_cd, 
      school_dstrct_nm == system_name, 
      instn_number == instn_number
    )
  ) |> 
  left_join(
    sys_mob,
    by = join_by(
      school_distrct_cd == school_district_cd,
      school_dstrct_nm == school_district_nm
    )
  ) |>
  select(
    school_distrct_cd,
    school_dstrct_nm,
    acdmc_lvl,
    subgroup_name,
    test_cmpnt_typ_nm,
    num_tested_cnt,
    begin_pct, 
    developing_pct,
    proficient_pct,
    distinguished_pct,
    enroll_pct_asian,
    enroll_pct_native,
    enroll_pct_black,
    enroll_pct_hispanic,
    enroll_pct_multiracial,
    enroll_pct_white,
    enroll_pct_migrant,
    enroll_pct_ed,
    enroll_pct_swd,
    enroll_pct_lep,
    enroll_pct_gifted,
    direct_cert_perc,
    chronic_absent_perc_all, 
    mobility
  )

graph_data_sys <- merged_data_sys |>
  mutate(pct_prof_dis = proficient_pct + distinguished_pct) |>
  relocate(pct_prof_dis, .after = distinguished_pct) |>
  mutate(
    sys_group = case_when(
      school_distrct_cd == "667" ~ "GCPS",
      school_dstrct_nm %in% metro_districts &
        school_distrct_cd != "667" ~ "Metro Atlanta",
      TRUE ~ "Other GA System"
    )
  ) |>
  mutate(
    sys_group = as.factor(sys_group),
    across(where(is.double),  ~ rnd_like_xl(replace_na(.x, 0), 1))
  ) |> 
  mutate(
    tooltip_lbl = paste(
      school_dstrct_nm,
      num_tested_cnt,
      enroll_pct_asian,
      enroll_pct_native,
      enroll_pct_black,
      enroll_pct_hispanic,
      enroll_pct_multiracial,
      enroll_pct_white,
      enroll_pct_migrant,
      enroll_pct_ed,
      enroll_pct_swd,
      enroll_pct_lep,
      enroll_pct_gifted,
      direct_cert_perc,
      chronic_absent_perc_all,
      mobility, 
      sep = ","
    )
  )


graph_data_sys <- graph_data_sys |> 
  mutate(
    grade_input = glue('Grade {sprintf("%1i",as.integer(acdmc_lvl))}'), 
    plt_title = glue('Grade {sprintf("%1i",as.integer( acdmc_lvl))} {test_cmpnt_typ_nm} Performance'),
    plt_subtitle = glue('{subgroup_name} School Year 2023-24'),
    txt_subj = glue(
      'System: {school_dstrct_nm}',
      '\n',
      'Performance Area: {test_cmpnt_typ_nm}',
      '\n',
      '# Tested: {prettyNum(num_tested_cnt, big.mark = ",")}',
      '\n',
      'Mobility Rate: {mobility}',
      '\n'
    ), 
    color = case_when(
      sys_group == "GCPS" ~"#66000080", 
      sys_group == "Metro Atlanta" ~"#022B3A80", 
      TRUE ~ "#66666680")
  )

save.image(file = "gosa_data.RData")
save.image(file = "../Dashboard/gosa_data.RData")

# # Example visualization: Scatter plot of two numeric columns
# ggplot(graph_data, aes(x = enroll_pct_ed, y = distinguished_pct)) +
#   geom_point() +
#   labs(title = "Scatter Plot of FRPL vs ELA Performance", x = "% FRPL", y = "% Distinguished")
