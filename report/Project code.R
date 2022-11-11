
################ Library #################
library(tidyverse)
library(lubridate)
library(stringr)
library(stringi)
library(readr)


################ Import data  #############
DNB_par_etablissement <- read_delim("Data/DNB-par-etablissement.csv", ";", quote = "'", escape_double = FALSE, trim_ws = TRUE)
Hebergement_eleves_etablissements_2d <- read_delim("Data/Hebergement-eleves-etablissements-2d.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Etablissements_labellises_generation_2024 <- read_delim("Data/Etablissements-labellises-generation-2024.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Covid_sp_dep_7j_cage_scol_2022_10_10_19h02 <- read_delim("Data/Covid-sp-dep-7j-cage-scol-2022-10-10-19h02.csv", ";", escape_double = FALSE, trim_ws = TRUE)

################ Data Wrangling #################

# For all data sets, we have to translate them (#1), make sure that all data are of the right type (#2), 
# as well as making sure that the time reference (year) are all aligned with the exam session (#3),
# add a column department_fr that with department names matching the ones in ggplot.

######### Function #########
#create a function to rename the column of all our df -> input df and character vector
rename_df <- function(df, x){
  if (ncol(df) == length(x)){
    names(df) <- c(x)
    df <- as_tibble(df)
  } else {
    stop("Vector is not the right length")
  }
}

######## dnb_results #######
#1
dnb_colnames <- c("session", "school_id", "school_type", "establishment_name", "education_sector", "municipality_code", "municipality", "department_code", "department", "academy_code", "academy_name", "region_code", "region", "registered", "present", "admitted", "admitted_whitout", "admitted_AB", "admitted_B", "admitted_TB", "success_rate_pct"
)
dnb_results <- rename_df(DNB_par_etablissement, dnb_colnames)
#2
# success_rate is of the form **,**% we want it as a double of the form **.**
dnb_results[["success_rate_pct"]] <- as.double(gsub("%","",
                                               gsub(",",".", dnb_results[["success_rate_pct"]])))
#3 We need to harmonize the year variables of all the other data sets to match the logic of this one. The year is the year of the exam session (e.g academic period "2020-2021" is represented as 2021)
#4 We need to add a column department_fr that with department names matching the ones in ggplot.
dnb_results$department_fr <- stri_trans_general(dnb_results$department, "Latin-ASCII") %>%
  str_to_title(.) %>% 
  gsub("Du", "du", .) %>% 
  gsub("De", "de", .) %>% 
  gsub("D'", "D", .) %>%
  gsub("Et", "et", .) %>%
  gsub(" ", "-", .) %>%
  str_replace_all("Corse-du-Sud", "Corse du Sud") %>% 
  str_replace_all("deux-Sevres", "Deux-Sevres") %>% 
  str_replace_all("Alpes-de-Hte-Provence", "Alpes-de-Haute-Provence") %>%       
  str_replace_all("Territoire-de-Belfort", "Territoire de Belfort") %>% 
  str_replace_all("Seine-Saint-denis", "Seine-Saint-Denis")
#5 No need for further data wrangling for this data set

######## establishment_24 ########
#1
est_24_names <- c("region", "academy", "department", "municipality", "establishment", "school_id", "school_type", "education_sector", "postcode", "adress", "adress_2", "mail", "students", "priority_education", "city_school", "QPV", "ULIS", "SEGPA", "sport_section", "agricultural_high_school", "military_high_school", "vocational_high_school", "establishment_web", "SIREN_SIRET", "district", "ministry", "label_start_date", "label_end_date", "y_coordinate", "x_coordinate", "epsg", "precision_on_localisation", "latitude", "longitude", "position", "engaging_30_sport")
establishment_24 <- rename_df(Etablissements_labellises_generation_2024, est_24_names)
#2 No problem for this data set
#3 Need to add two variable session_started and session_ended
#  Most labellisations start and end in January but a few start and end in middle of the year. Exams take place end of June, beginning of July therefore, we will consider labellisation done in August and after as done for the next academic year
establishment_24 <- establishment_24 %>% 
  mutate(session_started = case_when(month(label_start_date) <= 7 ~ year(label_start_date),
                                     month(label_start_date) >  7 ~ year(label_start_date)+1),
         session_ended = case_when(month(label_end_date) <= 7 ~ year(label_end_date),
                                   month(label_end_date) >  7 ~ year(label_end_date)+1)
         )
#4 We need to add a column department_fr that with department names matching the ones in ggplot.
establishment_24$department_fr <- stri_trans_general(establishment_24$department, "Latin-ASCII") %>%
  str_to_title(.) %>% 
  gsub("Du", "du", .) %>% 
  gsub("De", "de", .) %>% 
  gsub("D'", "D", .) %>%
  gsub("Et", "et", .) %>%
  gsub(" ", "-", .) %>%
  str_replace_all("Corse-du-Sud", "Corse du Sud") %>% 
  str_replace_all("deux-Sevres", "Deux-Sevres") %>% 
  str_replace_all("Territoire-de-Belfort", "Territoire de Belfort") %>% 
  str_replace_all("Seine-Saint-denis", "Seine-Saint-Denis")

#5 We can create a high_school_type variable instead of agricultural_high_school, military_high_school, vocational_high_school and drop those variables
establishment_24 <- establishment_24 %>% 
  mutate(high_school_type = case_when(agricultural_high_school == 1 ~ "agricultural high school",
                                      military_high_school == 1 ~ "military high school",
                                      vocational_high_school == 1 ~ "vocational high school")) %>% 
    select(region:sport_section, high_school_type, establishment_web:session_ended)

######## student_housing #########
#1
housing_names <- c("year_back_to_school", "Academic_region", "academy", "department", "municipality", "school_id", "establishment_main_name", "establishment_name", "school_type", "education_sector", "students_secondary_education", "students_higher_education", "external_students_secondary_education", "half_boarders_students_secondary_education", "boarding_students_secondary_education", "external_students_higher_education", "half_board_students_higher_education", "boarding_students_higher_education")
student_housing <- rename_df(Hebergement_eleves_etablissements_2d, housing_names)
#2 No Problem for this data set
#3 We need to create a session variable as year_back_to_school refers to the beginning of the school year and not the exam session
student_housing <- student_housing %>% 
  mutate(session = year_back_to_school + 1) %>% 
    select(year_back_to_school,session, everything()) #here just to order variable
#4 We need to add a column department_fr that with department names matching the ones in ggplot.
student_housing$department_fr <- stri_trans_general(student_housing$department, "Latin-ASCII") %>%
  str_to_title(.) %>% 
  gsub("Du", "du", .) %>% 
  gsub("De", "de", .) %>% 
  gsub("D'", "D", .) %>%
  gsub("Et", "et", .) %>%
  gsub(" ", "-", .) %>%
  str_replace_all("Corse-du-Sud", "Corse du Sud") %>% 
  str_replace_all("deux-Sevres", "Deux-Sevres") %>% 
  str_replace_all("Alpes-de-Hte-Provence", "Alpes-de-Haute-Provence") %>%       
  str_replace_all("Territoire-de-Belfort", "Territoire de Belfort") %>% 
  str_replace_all("Seine-Saint-denis", "Seine-Saint-Denis")

#5 No need for further data wrangling for this data set

####### single_parent #########
#1
sg_parent_names <- c("geocode", "municipality", "year","sing_par")


####### covid_in_schools #########
# can try to implement API (further idea)
#1
covide_names <- c("department_code", "test_week", "educational_level", "age_group", "pop", "positive", "tested", "incidence_rate", "positivity_rate", "screening_rate")
covid_in_schools <- rename_df(Covid_sp_dep_7j_cage_scol_2022_10_10_19h02,covide_names)
#2 test_week will be treated in (#3). positive, incidence_rate and positivity_rate need to be doubles
covid_in_schools[["positive"]] <- as.double(gsub(",",".", covid_in_schools[["positive"]]))
covid_in_schools[["incidence_rate"]] <- as.double(gsub(",",".", covid_in_schools[["incidence_rate"]]))
covid_in_schools[["positivity_rate"]] <- as.double(gsub(",",".", covid_in_schools[["positivity_rate"]]))

#3 need to create two new variables the first will be the date categorizing each week. We choose the first date (Monday). The test for a session will be those from August to July of the next year. 
#  As our argument will be set on the month, we might some test done the first days of august counted towards the "wrong" session. The number of Covid cases in August are relatively low compared
#  to the rest of the year and it represents at maximum 6 days of tests. Therefore we consider this margin of error to be satisfactory.

covid_in_schools <- covid_in_schools %>% 
  mutate(test_date = ymd(substr(test_week,1,10)),
         session = case_when(month(test_date) <= 7 ~ year(test_date),
                             month(test_date) >  7 ~ year(test_date)+1))
#4 Only department code need to input the name of the department




######### auxiliary data sets ##########

map <- map_data("france")
#The region variable is in fact the departments. A quick renaming is necessary.
colnames(map)[5]<- "department_fr"

  

################ Exploratory analysis #################
ggplot(dnb_results)+
  geom_bar(mapping = aes(x = registered, fill = department))

# number of admitted by region 
dnb_results %>% 
  select(admitted, region) %>% 
    group_by(region) %>% 
      summarise(sum = sum(admitted)) %>% 
         ggplot(aes(x = region, y = sum, fill = region)) + 
           geom_col() +
             theme(axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())

#average success rate by department
dnb_results %>% 
  select(success_rate_pct, department) %>% 
  group_by(department) %>% 
  summarise(mean = mean(success_rate_pct)) %>% 
  ggplot(aes(x = department, y = mean, fill = department)) + 
  geom_col() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



############# esssai carte #########
# creation of the map theme
map_theme <- theme(title=element_text(),
                   plot.title=element_text(margin=margin(20,20,20,20), size=18, hjust = 0.5),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   panel.grid.major= element_blank(), 
                   panel.background= element_blank()) 

#Creation of the dataset used for the map
result <- dnb_results %>% 
  select(department_fr, success_rate_pct) %>% 
  group_by(department_fr) %>% 
  summarise(mean = mean(success_rate_pct))

#join the map and our new dataset
result_map <- left_join(x = map[,-6], y = result)

#plot, 
ggplot(result_map, aes(long,lat, group = group, fill = mean)) +
  geom_polygon() +
  coord_map() +
  scale_fill_gradient(name = "Average sucess  rate") +
  labs(x = "", 
       y = "", 
       title = "Average success rate, xxxx-2021") +
  map_theme  
#for other examples to change colours https://colinfay.me/mapping-the-french-second-round-results-with-r/
