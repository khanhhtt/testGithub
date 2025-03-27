
setwd("C:\\Users\\httk\\Desktop\\Demo")

source("LHR - dependencies.R")

df <- read_sav("TEST - PersonalInfor Detect.sav") 

df_out <- GDPR_check_personalInfor(df,
                         respid_var = "Respondent_Serial",
                         date_var = "ActualStartTime")

write.xlsx(df_out, "Open Answer - Personal Infor check.xlsx", sheetName = "OAs - Personal Infor check") 
