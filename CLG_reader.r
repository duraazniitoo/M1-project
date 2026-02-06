library(tidyverse)
library(wooldridge)
library(readr)
library(readxl)
getwd()
entireCLGdataset <- CLG <- read_excel("/Users/ferdurazo/Desktop/ENS /Data_mémoire/CLG/CLG_LM_1811/AidDatas_CLG_LMIC_Dataset_v1.0.xlsx", 
                                      sheet = "CLG-LMIC 1.0_Records") 
CLG <- read_excel("/Users/ferdurazo/Desktop/ENS /Data_mémoire/CLG/CLG_LM_1811/AidDatas_CLG_LMIC_Dataset_v1.0.xlsx", 
                                             sheet = "CLG-LMIC 1.0_Records")

View(CLG)

#Geographical restriction to LAC countries
CLG <- CLG %>% filter(Region_of_Activity == "America")
View(CLG)
CLG <- CLG %>% filter(Country_of_Activity != c("United States","Canada"))
unique(CLG$Country_of_Activity)

#Geographic repartition
table(CLG$Country_of_Activity_ISO3)
table(CLG$Intent)

#Intent 
#In mexico
MEX_CLG <- CLG %>%
  filter(Country_of_Activity == "Mexico")
View(MEX_CLG)
table(MEX_CLG$Intent)
###################TELECOM_KEYWORDS#################################
keyword

library(dplyr)
library(stringr)

pattern <- regex(paste(keywords, collapse = "|"), ignore_case = TRUE)

Large_txtmin_CLG <- CLG %>%
  filter(
    if_any(
      everything(),
      ~ str_detect(as.character(.), pattern)
    )
  )
View(Large_txtmin_CLG)


install.packages("writexl")
library(writexl)
write_xlsx(Large_txtmin_CLG, "Large_txtminCLG.xlsx")

######### KEYWORDS ############
pattern <- regex(paste(telecom_keywords, collapse = "|"), ignore_case = TRUE)

clean_txtmin_CLG <- CLG %>%
  filter(
    if_any(
      everything(),
      ~ str_detect(as.character(.), pattern)
    )
  )
View(clean_txtmin_CLG)


write_xlsx(clean_txtmin_CLG, "Clean_txtminCLG.xlsx")

############# HUAWEI ###############
pattern <- regex(paste(telecom_keywords, collapse = "|"), ignore_case = TRUE)

clean_txtmin_CLG <- CLG %>%
  filter(
    if_any(
      everything(),
      ~ str_detect(as.character(.), pattern)
    )
  )
View(clean_txtmin_CLG)


write_xlsx(clean_txtmin_CLG, "Clean_txtminCLG.xlsx")

#####################   Subsets of private entreprises    ######################
pattern_priv_ict <- regex(paste(priv_ict, collapse = "|"), ignore_case = TRUE)
pattern_huawei <- regex(paste(huawei_ict, collapse = "|"), ignore_case = TRUE)

Priv_ICT_CLG <- CLG %>%
  filter(
    if_any(
      everything(),
      ~ str_detect(as.character(.), pattern_priv_ict)
    )
  )

Huawei_CLG <- CLG %>%
  filter(
    if_any(
      everything(),
      ~ str_detect(as.character(.), huawei_ict)
    )
  )

View(Priv_ICT_CLG)
View(Huawei_CLG)



############################                      ##############################