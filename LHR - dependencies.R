
pacman::p_load(tidyverse,
               haven,
               openxlsx)

detect_personal_infor <- function(text) {
  check_personal_infor <- c()  
  
  # Regular expression to match email addresses
  regex_mail <- "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"
  # Regular expression to match potential phone numbers
  regex_phone <- "\\d{6,7}"
  # Regular expression to match capital letters
  regex_capital_letter <- "[A-Z]"
  
  # Check for personal information
  if (grepl(regex_mail, text)) {
    check_personal_infor <- c(check_personal_infor, "Might contain email address")
  }
  if (grepl(regex_phone, text)) {
    check_personal_infor <- c(check_personal_infor, "Might contain phone number")
  }
  
  if (!grepl("^[[:upper:]][[:lower:]0-9’'.,!?()\";: -]*$", text)) {
    if (grepl(regex_capital_letter, text)) {
      check_personal_infor <- c(check_personal_infor, "Might contain name")
    }
  }
  
  # Return results as a single string
  return(paste(check_personal_infor, collapse = "; "))
}

GDPR_check_personalInfor = function(df, respid_var = "Respondent_Serial",
                                    date_var = "ActualStartTime",
                                    cols = everything()) {
  
  string_vars <- df %>%
    select(where(is.character))
  
  cols_with_text <- names(string_vars)[colSums(string_vars != "") != 0]
  
  df_final <- df %>%
    mutate(Date = format(!!as.symbol(date_var), "%Y-%m-%d")) %>%
    select(., !!as.symbol(respid_var), Date, all_of(cols_with_text)) %>%
    select(!!as.symbol(respid_var), Date, !starts_with(c("Respondent_", "DataCollection_", "UserAgent_", 
                                                   "TimeTracking", "INTSTATUS", "Intro", "ExitsPage_"))) %>%
    select(!contains(c("Destination", "Flight", "Nationality", "Residence", "Postcode", "AdminInfo", "Currency"))) %>%
    select(., !!as.symbol(respid_var), Date, all_of(cols)) %>%
    pivot_longer(., cols = -c(respid_var, "Date"),
                 names_to = "Variable", values_to = "Open_Answer") %>%
    filter(Open_Answer != "") %>%
    mutate(Check_status = lapply(.$Open_Answer, detect_personal_infor)) %>%
    filter(Check_status != "") %>%
    arrange(Variable, !!as.symbol(respid_var))
  
  return(df_final)
  
}





