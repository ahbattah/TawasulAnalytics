
# Set theme
tawasul_theme <- theme_minimal() +
  theme(legend.position = "none", axis.text = element_text(size = 12))

# Read tawasul cases data
tawasul.files <- list.files("./files/tawasul_cases", pattern = "txt")
tawasul.cases <- lapply(paste0("./files/tawasul_cases/", tawasul.files), read_csv)

tawasul.cases <- do.call("rbind", tawasul.cases)

#Rename and Add some coloumns
names(tawasul.cases)[names(tawasul.cases) == "STATUS"] <- "STATUS_CODE"
names(tawasul.cases)[names(tawasul.cases) == "MINISTRY"] <- "MINISTRY_CODE"
names(tawasul.cases)[names(tawasul.cases) == "NAME_EN"] <- "MINISTRY"
names(tawasul.cases)[names(tawasul.cases) == "ESCALATION_TO_OFDPM_CHECK"] <- "ESCALATED_TO_OFDPM"
names(tawasul.cases)[names(tawasul.cases) == "IS_ESCALATED_TO_MINISTER"] <- "ESCALATED_TO_MINISTER"

# Remove (CIO, Telecommunication Regulatory Authority and Middle Area Municipality)
tawasul.cases <- tawasul.cases %>% 
  filter(!(MINISTRY_CODE %in% c(3, 10, 11)))

tawasul.cases <- tawasul.cases %>% 
  mutate(STATUS_DESC = tawasul.cases$STATUS_CODE)

# Status details
tawasul.cases$STATUS_DESC <- tawasul.cases$STATUS_DESC %>% 
  recode("1" = "Submitted", 
         "2" = "Transferred - Responsible Team",
         "3" = "Transferred - Ministry",
         "4" = "Request Additional Info Before Assignment",
         "5" = "Responsible Team",
         "6" = "Request Additional Info After Assignment",
         "15" = "Assign Other Units",
         "16" = "Request Additional Info (From Unit)",
         "18" = "Waiting Responsible Team  After responsible units",
         "19" = "Waiting CCU approval after Responsible teams",
         "20" = "Pending User Feedback",
         "21" = "Examine Feedback",
         "22" = "Closed - Satisfied Feedback",
         "23" = "Closed - Not Satisfied",
         "24" = "Reopen",
         "25" = "Closed - No Feedback",
         "31" = "Doable Suggestion",
         "32" = "Undoable Suggestion",
         "95" = "Submitted by NCC and closed immediately",
         "96" = "Force Closed",
         "97" = "Duplicate By CCU",
         "98" = "Duplicate By responsible team",
         "99" = "Irrelevant",
         "100" = "Closed - No Information")

# Status Open/Closed
tawasul.cases <- tawasul.cases %>% 
  mutate(STATUS = ifelse(STATUS_CODE %in% c(22, 23, 25,31,32, 35, 95:100), "Closed",
                         "Open"))
# Prepare dates
tawasul.cases$CREATE_DATE <- as.Date(tawasul.cases$CREATE_DATE)
tawasul.cases$LAST_UPDATE <- as.Date(tawasul.cases$LAST_UPDATE)
tawasul.cases$LAST_UPDATE[is.na(tawasul.cases$LAST_UPDATE)] <- Sys.Date()

# Customer type
tawasul.cases <- tawasul.cases %>% 
  mutate(CUSTOMER_TYPE = ifelse(CUSTOMER_TYPE == 1, "Individual",
                                ifelse(CUSTOMER_TYPE == 2, "Corporate", "Visitor")))

# Way to contact
tawasul.cases <- tawasul.cases %>% 
  mutate(WAY_TO_CONTACT = ifelse(WAY_TO_CONTACT == 1, "Phone", "Email"))

# Priority
tawasul.cases <- tawasul.cases %>% 
  mutate(PRIORITY = ifelse(PRIORITY %in% 1:2, "Critical", "Non Critical"))

# Case type
tawasul.cases <- tawasul.cases %>% 
  mutate(REQUEST_TYPE = ifelse(REQUEST_TYPE == 1, "Complaint",
                               ifelse(REQUEST_TYPE == 2, "Suggestion",
                                      "Enquiry")))
# Suggestion staus
tawasul.cases <- tawasul.cases %>% 
  mutate(SUGGESTION_STATUS = ifelse(STATUS_CODE == 31, "Doable",
                                    ifelse(STATUS_CODE == 32, "Not doable",
                                           "Other")))

# Channel
tawasul.cases$COMPLAINT_CHANNEL <- tawasul.cases$COMPLAINT_CHANNEL %>% 
  recode("1" = "Online", 
         "2" = "Mobile",
         "3" = "Kiosk",
         "4" = "EService Centers",
         "5" = "Online/Video Chat",
         "6" = "E-Mail",
         "7" = "Social Media",
         "8" = "Road Shows",
         "9" = "Contact Center",
         "10" = "Media",
         "11" = "Walk-in ",
         "12" = "Letters",
         "13" = "Telephone",
         "14" = "Fix2Go",
         .default = "Online")

# For category
#tawasul.lookups %>% filter(MINISTRY_ID == tawasul.cases$CHANNEL, LOOKUP_ID == tawasul.cases$CATEGORY, CATEGORY == 1) %>% select(DESC_EN, CATEGORY)

# For channel
#tawasul.lookups %>% filter(MINISTRY_ID == tawasul.cases$CHANNEL, LOOKUP_ID == tawasul.cases$CATEGORY, CATEGORY == 2) %>% select(DESC_EN, CATEGORY)

# Escalation
tawasul.cases$ESCALATED_TO_OFDPM  <- tawasul.cases$ESCALATED_TO_OFDPM  %>% 
  recode("0" = 0,
         "1" = 1,
         .default = 0)


# Working on SLA
# Create with/out SLA
tawasul.cases <- tawasul.cases %>% 
  mutate(SLA_STATUS = ifelse(LAST_UPDATE > (CREATE_DATE + SLA_ASSIGNED + 
                                              ((CUST_FEEDBACK_TIME_IN_MIN + 
                                                  RFI_FEEDBACK_TIME_IN_MIN) / (60 * 24))),
                             "Exceeded SLA", "Within SLA")) %>% 
  unite("SLA_DESC", c("SLA_STATUS", "PRIORITY"), sep = " - ", remove = FALSE)

# SLA Expectations
SLA_for_exp <- data.frame(
  MINISTRY_CODE = c(1:2, 4:5, 7:9, 12:33, 35),
  SLA_CRITICAL = c(13, 17, 23, 23, 23, 23, 23, 23, 20, 20, 24, 21, 23, 23,
                   17, 10, 18, 23, 23, 23, 23, 23, 9, 23, 23, 10, 23, 23, 20, 17),
  SLA_NON_CRITICAL = c(30, 24, 84, 49, 32, 84, 84, 49, 24, 24, 28, 28, 51, 35,
                       24, 24, 28, 32, 65, 34, 49, 49, 16, 27, 49, 27, 35, 30, 27, 34)
)

# Join tawasul.cases with SLA_for_exp
tawasul.cases <- tawasul.cases %>% left_join(SLA_for_exp)

# Calculate Performance
calculatePerf <- function(LAST_UPDATE, CREATE_DATE, CUST_FEEDBACK_TIME_IN_MIN, 
                          RFI_FEEDBACK_TIME_IN_MIN, PRIORITY, SLA_CRITICAL, SLA_NON_CRITICAL) {
  totalMins <- difftime(LAST_UPDATE, CREATE_DATE, 
                        units = "mins") - (CUST_FEEDBACK_TIME_IN_MIN + 
                                             RFI_FEEDBACK_TIME_IN_MIN)
  result <- vector(mode = "character", length = 1)
  if (PRIORITY == "Critical") {
    performance <-  (totalMins * 100) / (SLA_CRITICAL * 24 * 60)
    if (performance < 30)
      result <- "Exceed Exp. <30%"
    else if (performance > 30 & performance < 90)
      result <- "Within Exp. <90%"
    else
      result <- "Below Exp. >90%"
    
  } else {
    performance <-  (totalMins * 100) / (SLA_NON_CRITICAL * 24 * 60)
    if (performance < 50)
      result <- "Exceed Exp. <50%"
    else if (performance > 50 & performance < 90)
      result <- "Within Exp. <90%"
    else
      result <- "Below Exp. >90%"
  }
  
  return(result)
}

tawasul.cases <- tawasul.cases %>% 
  mutate(PERFORMANCE = mapply(calculatePerf, tawasul.cases$LAST_UPDATE, 
                              tawasul.cases$CREATE_DATE, tawasul.cases$CUST_FEEDBACK_TIME_IN_MIN, 
                              tawasul.cases$RFI_FEEDBACK_TIME_IN_MIN, tawasul.cases$PRIORITY, 
                              tawasul.cases$SLA_CRITICAL, tawasul.cases$SLA_NON_CRITICAL))


# Final DF
tawasul <- tawasul.cases %>% 
  select(-c(STATUS_CODE, MINISTRY_CODE))

varsToFactor <- c("CUSTOMER_TYPE", "WAY_TO_CONTACT", "SLA_DESC", "PRIORITY",
                  "REQUEST_TYPE", "COMPLAINT_CHANNEL", "STATUS_DESC", 
                  "STATUS", "MINISTRY", "PERFORMANCE", "ESCALATED_TO_MINISTER",
                  "ESCALATED_TO_OFDPM")

tawasul[, varsToFactor] <- lapply(tawasul[, varsToFactor], factor)

# Min and Max dates
minDate <- min(tawasul$CREATE_DATE)
maxDate <- max(tawasul$CREATE_DATE)