
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

# Or view all tables to find the one you want
for(i in 1:length(tables)) {
  print(paste("Table", i))
  print(head(tables[[i]]))
}

# Once you find the right table, save it as a data frame
Strategic_partners <- tables[[3]][,-4]  # Change the number to the table you want
View(Strategic_partners)
