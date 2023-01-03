# Dataset
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-DV0151EN-SkillsNetwork/labs/module_4/starter_code/adult.csv"
download.file(url, destfile = "adult.csv")
adult <- read_csv("adult.csv")
