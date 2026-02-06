library(tidyverse)
library(wooldridge)
library(readr)
library(readxl)
CLG_1 <- read_excel("CLG_LM_1811/AidDatas_CLG_LMIC_Dataset_v1.0.xlsx", 
                  sheet = "CLG-LMIC 1.0_Records")
View(CLG)

#Geographical restriction to LAC countries
CLG <- CLG %>% filter(Region_of_Activity == "America")
View(CLG)
CLG <- CLG %>% filter(Country_of_Activity != c("United States","Canada"))
unique(CLG$Country_of_Activity)


##########################BIG LIST###############################
pattern <- regex(paste(keywords, collapse = "|"), ignore_case = TRUE)

filtered_CLG <- CLG %>%
  filter(
    if_any(
      everything(),
      ~ str_detect(as.character(.), pattern)
    )
  )
View(filtered_CLG)

keywords <- c(
  # Telecom & network infrastructure
  "telecom infrastructure",
  "digital",
  "telecom", 
  "telecommunications",
  "core network",
  "radio access network",
  "RAN",
  "base stations",
  "telecom towers",
  "cell towers",
  "optical networks",
  "fiber optic cables",
  "fiber networks",
  "backbone networks",
  "subsea cables",
  "satellite internet",
  "internet exchange points",
  "network interconnection",
  
  # Broadband & internet
  "broadband infrastructure",
  "fixed broadband",
  "wireless broadband",
  "FTTH",
  "FTTP",
  "last mile",
  "middle mile",
  "gigabit broadband",
  "internet backbone",
  
  # Data centers & compute
  "data center",
  "data centers",
  "hyperscale data centers",
  "edge data centers",
  "edge computing",
  "colocation",
  "high performance computing",
  "HPC clusters",
  "cloud infrastructure",
  
  # AI & advanced digital tech
  "artificial intelligence",
  "AI computing",
  "machine learning",
  "deep learning",
  "neural networks",
  "AI accelerators",
  "inference infrastructure",
  "training infrastructure",
  
  # Smart cities & surveillance
  "smart city",
  "smart cities",
  "urban surveillance",
  "video surveillance",
  "video analytics",
  "IP cameras",
  "facial recognition",
  "license plate recognition",
  "traffic management systems",
  "public security systems",
  
  # Software (restricted to infrastructure use)
  "network management software",
  "surveillance software",
  "platform software",
  
  # Vendors & geopolitics
  "Huawei",
  "ZTE",
  "Chinese telecom equipment",
  "Digital Silk Road",
  
  # Indigenous Chinese hardware
  "Chinese servers",
  "indigenous servers",
  "domestic servers",
  "Chinese semiconductors"
)

##############################TELECOMMUNICATIONS################################
telecom_keywords <- c("telecom", "telecommunications", "computers", 
                      "Huawei", "ZTE", "Uniview", "fiber optic cables", "surveillance",
                      "cables", "submarine cables", "smartphone", "phone", "broadband", 
                      "internet of things", " IoT ", "internet", "laptop", "computer", 
                      "semiconductor", "recognition", " ICT ", " broadband connectivity", 
                      "Uniview", "21Vianet", "VNET", "Yangtze", "Hengtong", "Tencent", "Alibaba", 
                      "Baidu", "Tsinghua")

pattern <- regex(paste(telecom_keywords, collapse = "|"), ignore_case = TRUE)

smallfilter_CLG <- CLG %>%
  filter(
    if_any(
      everything(),
      ~ str_detect(as.character(.), pattern)
    )
  )
View(smallfilter_CLG)

###### HUAWEI ZTE #######
priv_ict <- c("Huawei", "ZTE", "Tencent", "Alibaba", "Baidu", "Xiaiomi",
              "Ant Group", "BGI", "BeiDou", "ByteDance", "CETC", 
              "China Mobile", "China Telecom", "China Unicom", "CloudWalk", 
              "DJI", "Dahua", "Hikvision", "Inspur", "Megvii", "Meiya Pico", 
              "Nuctech", "Ping An Technology", "SenseTime", "Tencent", "Uniview", 
              "WuXi AppTec Group", "YITU", "iFlytek")
huawei_ict <- "Huawei"


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
