############STRATEGIC PARTNERSHIPS CHINA################
library(rvest)
library(dplyr)

# Read the Wikipedia page
url <- "https://en.wikipedia.org/wiki/List_of_diplomatic_partnerships_of_China"
page <- read_html(url)

# Extract all tables from the page
tables <- page %>% html_table(fill = TRUE)

# See how many tables there are
length(tables)

# View the third table
tables[[3]]
Partners <- tables[[4]]
Special_partnership <- tables[[5]]

View(Special_partnership)

# Or view all tables to find the one you want
for(i in 1:length(tables)) {
  print(paste("Table", i))
  print(head(tables[[i]]))
}

# Data frame of strategic partnerships (accessed thurday 5th 2026, 17h35 CST)
Strategic_partners <- tables[[3]][,-4]  # Change the number to the table you want
View(Strategic_partners)

All_partnerships <- merge(df, Special_partnership,all.y = TRUE, all.x = TRUE )
All_partnerships <- All_partnerships[,-2]
View(All_partnerships)

############## Coding partnerships  ################
All_partnerships$Name
#Taking from Churen and Yaying's categorization : 

##Grouping variables for those who have the same names
df_merged <- All_partnerships %>%
  group_by(Name) %>%
  summarise(across(everything(), ~paste(., collapse = ", ")))

View(df_merged)

##Creating a new variable for the category they are in

df_merged <- df_merged %>% mutate(
  Category = 
    case_when(
      Name %in% c("New-type cooperative partnership",
                "Friendly relations", 
                "All-round friendly and cooperative partnership", 
                "Closer development partnership") ~ "Partnership",
      Name %in% c("Comprehensive, friendly and cooperative partnership", 
                  "Innovative comprehensive partnership", "Comprehensive partnership", 
                  "New strategic partnership", 
                  "Traditional friendly relations", 
                  "Friendly cooperative relations", 
                  "All-round strategic partnership", 
                  "All round high quality future oriented partnership", 
                  "Open and pragmatic comprehensive cooperative partnership"
                  ) ~"Comprehensive partnership", 
      Name %in% c("Strategic partnership", 
                  "Strategic cooperative partnership",
                  "Strategic partnership",
                  "Strategic partnership for development and prosperity",
                  "Strategic partnership for innovation",
                  "Strategic partnership of sincere mutual assistance and everlasting friendship"
                  ,"Mutually beneficial strategic partnership") ~ "Strategic partnership", 
      Name %in% c("All-weather comprehensive strategic partnership for a new era"
                   ,"All-weather strategic cooperative partnership"
                   ,"All-weather strategic partnership",
                  "Comprehensive strategic cooperative partnership"
                  ,"Comprehensive strategic partnership",
                  "Comprehensive strategic partnership in the new Era",
                  "Comprehensive strategic partnership in the new era",
                  "Comprehensive strategic partnership of coordination for a new era",
                  "Comprehensive strategic partnership of mutual respect and common development",
                  "Comprehensive strategic partnership of mutual respect and common development in the new era",
                  "Comprehensive, friendly and cooperative partnership",
                  "Special friendship in the new era",
                  "Permanent comprehensive strategic partnership", 
                  "All-weather comprehensive strategic partnership for a new era", 
                  "Long-term, consistent comprehensive strategic partnership", 
                  "Traditional friendly and cooperative relations", 
                  "All-weather comprehensive strategic partnership") ~ "Comprehensive strategic partnership", 
      Name == TRUE ~ Name
    ))


#### Giving it a number 
df_merged <- df_merged %>%
  mutate(
    Categ_numeric =
      case_when(
        Category == "Partnership"~ 1L, 
        Category == "Strategic pertnership"~3L,
        Category == "Comprehensive partnership" ~2L, 
        Category == "Comprehensive strategic partnership"~4L
      )
  )

df_merged %>% filter(is.na(Categ_numeric))
hist(df_merged$Categ_numeric, breaks = 16)
df_merged$Name
View(df_merged)

df_merged <- df_merged[,-3]


df_merged 

library(tidyr)

df_trial <- df_merged %>%
  separate_rows(Country, sep = ", ")

View(df_trial)


###Creating a subset for only LAC
df_trial$Countries
df_trial <- df_trial %>%
  mutate(
    Region = case_when(
      # East Asia & Pacific
      Countries %in% c("China", "Japan", "South Korea", "North Korea", "Mongolia",
                     "Singapore", "Malaysia", "Indonesia", "Thailand", "Vietnam",
                     "Philippines", "Myanmar", "Cambodia", "Brunei", "Timor-Leste",
                     "Australia", "New Zealand", "Papua New Guinea", "Fiji",
                     "Solomon Islands", "Vanuatu", "Samoa", "Tonga", "Cook Islands",
                     "Micronesia", "Niue") ~ "East Asia & Pacific",
      
      # South Asia
      Countries %in% c("India", "Pakistan", "Bangladesh", "Sri Lanka", "Nepal",
                     "Afghanistan", "Maldives") ~ "South Asia",
      
      # Central Asia
      Countries %in% c("Kazakhstan", "Uzbekistan", "Turkmenistan", "Kyrgyzstan",
                     "Tajikistan") ~ "Central Asia",
      
      # Middle East & North Africa
      Countries %in% c("Saudi Arabia", "United Arab Emirates", "Iran", "Iraq",
                     "Turkey", "Egypt", "Algeria", "Morocco", "Tunisia", "Libya",
                     "Yemen", "Oman", "Qatar", "Kuwait", "Bahrain", "Jordan",
                     "Lebanon", "Syria", "Palestine", "Israel") ~ "Middle East & North Africa",
      
      # Sub-Saharan Africa
      Countries %in% c("South Africa", "Nigeria", "Kenya", "Ethiopia", "Ghana",
                     "Tanzania", "Uganda", "Angola", "Mozambique", "Zimbabwe",
                     "Zambia", "Namibia", "Botswana", "Rwanda", "Senegal",
                     "Ivory Coast", "Democratic Republic of the Congo",
                     "Republic of the Congo", "Gabon", "Cameroon", "Mali",
                     "Burkina Faso", "Niger", "Chad", "Guinea", "Sierra Leone",
                     "Liberia", "Mauritania", "Madagascar", "Malawi", "Burundi",
                     "Togo", "Benin", "Central African Republic", "Equatorial Guinea",
                     "Eritrea", "Djibouti", "Somalia", "Sudan", "Guinea-Bissau",
                     "Lesotho", "Mauritius", "Seychelles", "Comoros",
                     "São Tomé and Príncipe", "Cape Verde") ~ "Sub-Saharan Africa",
      
      # Europe
      Countries %in% c("Russia", "Germany", "United Kingdom", "France", "Italy",
                     "Spain", "Poland", "Romania", "Netherlands", "Belgium",
                     "Greece", "Portugal", "Czech Republic", "Hungary", "Sweden",
                     "Austria", "Belarus", "Bulgaria", "Serbia", "Denmark",
                     "Finland", "Slovakia", "Norway", "Ireland", "Croatia",
                     "Bosnia and Herzegovina", "Albania", "Lithuania", "Slovenia",
                     "Latvia", "Estonia", "North Macedonia", "Iceland", "Malta",
                     "Cyprus", "Andorra", "San Marino", "Switzerland",
                     "Ukraine", "Georgia", "Armenia", "Azerbaijan") ~ "Europe",
      
      # Latin America & Caribbean
      Countries %in% c("Brazil", "Mexico", "Argentina", "Colombia", "Chile",
                     "Peru", "Venezuela", "Ecuador", "Bolivia", "Uruguay",
                     "Paraguay", "Cuba", "Dominican Republic", "Haiti",
                     "Costa Rica", "Panama", "Nicaragua", "Honduras",
                     "El Salvador", "Guatemala", "Jamaica", "Trinidad and Tobago",
                     "Guyana", "Suriname", "Belize", "Barbados", "Bahamas") ~ "Latin America & Caribbean",
      
      # North America
      Countries %in% c("United States", "Canada") ~ "North America",
      
      TRUE ~ "Other"
    )
  )

df_LAC <- df_trial %>%
  filter(Region == "Latin America & Caribbean")
View(df_LAC)

library(writexl)
write_xlsx(df_LAC, "LAC_partnerships.xlsx")

hist(df_LAC$Categ_numeric, breaks = 16)
