# LOAD DATA #
# This script will load the different datasets, combine them and then anonymises them for further processing

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# setup wd
wd <- list()
wd$data <-"C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/data/"
wd$output <- "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/output/"

# LOAD DATA; make email case comparable, remove phone number
signupconsent <- read.csv(paste0(wd$data, "signup_consent.csv")) %>% 
  mutate(RecipientEmail = paste(RecipientEmail, mail)) %>% 
  mutate(RecipientEmail = tolower(trimws(as.character(RecipientEmail)))) %>% 
  slice(-c(1:2)) %>% 
  filter(Status == 0 & Finished == 1) %>%
  mutate_at("hh_size", as.numeric) %>% 
  mutate(hh_size = hh_size+1) %>% 
  slice(-c(32))
t0 <- read.csv(paste0(wd$data, "T0_raw_final.csv")) %>% 
  mutate(RecipientEmail = tolower(trimws(as.character(RecipientEmail)))) %>% 
  slice(-c(1:2)) %>% 
  filter(Status == 0 & Finished == 1 & RecipientEmail != "") %>% 
  mutate_at("fw_g", as.numeric)
t1 <- read.csv(paste0(wd$data, "T1_raw_final.csv")) %>% 
  mutate(RecipientEmail = tolower(trimws(as.character(RecipientEmail)))) %>% 
  slice(-c(1:2)) %>% 
  filter(Status == 0 & Finished == 1 & RecipientEmail != "")
t2 <- read.csv(paste0(wd$data, "T2_raw_final.csv")) %>% 
  mutate(RecipientEmail = tolower(trimws(as.character(RecipientEmail)))) %>% 
  slice(-c(1:2)) %>% 
  filter(Status == 0 & Finished == 1 & RecipientEmail != "")
t3 <- read.csv(paste0(wd$data, "T3_raw_v1.csv")) %>% 
  mutate(RecipientEmail = tolower(trimws(as.character(RecipientEmail)))) %>% 
  slice(-c(1:2)) %>% 
  filter(Status == 0 & Finished == 1 & RecipientEmail != "")
fw_manual <- read.csv(paste0(wd$data, "manual_fw_g.csv")) %>% 
  select(-phone) %>% 
  rename(RecipientEmail = email) %>% 
  mutate(RecipientEmail = tolower(trimws(as.character(RecipientEmail))))
random <- read.csv(paste0(wd$data, "randomisation.csv")) %>% 
  rename(RecipientEmail = email) %>% 
  mutate(RecipientEmail = tolower(trimws(as.character(RecipientEmail))))

# fix the data
signupconsent$RecipientEmail[signupconsent$RecipientEmail == "annerooseikelenboomg@wur.nl"] <- "anneroos.eikelenboom@wur.nl" # fix entry error

t0$fw_g[1] <- t0$fw_g[1] + 373 # add weight of bin if person did not include it
t0$fw_g[12] <- t0$fw_g[12] + 373 # add weight of bin if person did not include it


# select only the questions, removing metadata
t0df <- t0 %>% 
  select(!c(1:11,13:17)) 
t1df <- t1 %>% 
  select(!c(1:11,13:17))
t2df <- t2 %>% 
  select(!c(1:11,13:17))
t3df <- t3 %>% 
  select(!c(1:11,13:17))

# Map email addresses to an ID
assign_participant_ids <- function(data, id_col = "id") {
  emails <- unique(t0$RecipientEmail)
  ids <- seq_along(emails)
  id_mapping <- data.frame(RecipientEmail = emails, id = ids)
  left_join(data, id_mapping, by = "RecipientEmail")
}

# Add IDs to the dfs
t0df <- assign_participant_ids(t0df) %>% select(!RecipientEmail)
t1df <- assign_participant_ids(t1df) %>% select(!RecipientEmail)
t2df <- assign_participant_ids(t2df) %>% select(!RecipientEmail)
t3df <- assign_participant_ids(t3df) %>% select(!RecipientEmail)
signupconsentdf <- assign_participant_ids(signupconsent) %>% select(!RecipientEmail)
randomdf <- assign_participant_ids(random) %>% select(!RecipientEmail)
fw_manualdf <- assign_participant_ids(fw_manual) %>% select(!RecipientEmail)

# Add the indirect weights and condition to the dfs
t0df <- t0df %>%
  left_join(select(fw_manualdf, id, fw_g_t0), by = "id") %>%
  left_join(select(randomdf, id, condition, used_tape), by = "id") %>% 
  left_join(select(signupconsentdf, id, hh_size), by = "id") %>% 
  mutate(across(c("fw_g", "hh_size", 7:26), as.numeric)) %>%
  mutate(fw_g = ifelse(!is.na(fw_g_t0), fw_g_t0, fw_g)) %>%
  #mutate(fw_g = ifelse(fw_g < 380 & fw_g > 370, 373, fw_g)) %>% 
  mutate(fw_g = ifelse(fw_g >= 373, fw_g - 373, fw_g)) %>% 
  select(-fw_g_t0) %>% 
  mutate(condition = factor(condition, levels = c(0,1), labels = c("control", "intervention"))) %>% 
  mutate(time = 0)

t1df <- t1df %>%
  left_join(select(fw_manualdf, id, fw_g_t1), by = "id") %>%
  left_join(select(randomdf, id, condition, used_tape), by = "id") %>% 
  left_join(select(signupconsentdf, id, hh_size), by = "id") %>%
  mutate(across(c("fw_g", "hh_size", 7:26), as.numeric)) %>% 
  mutate(fw_g = ifelse(!is.na(fw_g_t1), fw_g_t1, fw_g)) %>%
  #mutate(fw_g = ifelse(fw_g < 380 & fw_g > 370, 373, fw_g)) %>% 
  mutate(fw_g = ifelse(fw_g >= 373, fw_g - 373, fw_g)) %>%
  select(-fw_g_t1) %>% 
  mutate(condition = factor(condition, levels = c(0,1), labels = c("control", "intervention"))) %>% 
  mutate(time = 1)

t2df <- t2df %>%
  left_join(select(fw_manualdf, id, fw_g_t2), by = "id") %>%
  left_join(select(randomdf, id, condition, used_tape), by = "id") %>% 
  left_join(select(signupconsentdf, id, hh_size), by = "id") %>%
  mutate(across(c("fw_g", "hh_size", 7:26), as.numeric)) %>% 
  mutate(fw_g = ifelse(!is.na(fw_g_t2), fw_g_t2, fw_g)) %>%
  #mutate(fw_g = ifelse(fw_g < 380 & fw_g > 370, 373, fw_g)) %>% 
  mutate(fw_g = ifelse(fw_g >= 373, fw_g - 373, fw_g)) %>%
  select(-fw_g_t2) %>% 
  mutate(condition = factor(condition, levels = c(0,1), labels = c("control", "intervention"))) %>% 
  mutate(time = 2)

t3df <- t3df %>%
  left_join(select(signupconsentdf, id, hh_size), by = "id") %>%
  mutate(across(c(4:23), as.numeric)) %>% 
  mutate(condition = factor(condition)) %>% 
  mutate(condition = factor(condition, levels = c(0,1), labels = c("control", "intervention"))) %>% 
  mutate(time = 3)

t0df <- t0df %>% 
  mutate(hs_stock = (stock_hs_1 + stock_hs_2 + stock_hs_3 + stock_hs_4) / 4) %>% 
  mutate(hs_meal = (meal_hs_1 + meal_hs_2 + meal_hs_3 + meal_hs_4) / 4) %>% 
  mutate(hs_snack = (snack_hs_1 + snack_hs_2 + snack_hs_3 + snack_hs_4) / 4) %>% 
  mutate(hs_share = (share_hs_1 + share_hs_2 + share_hs_3 + share_hs_4) / 4)

t1df <- t1df %>% 
  mutate(hs_stock = (stock_hs_1 + stock_hs_2 + stock_hs_3 + stock_hs_4) / 4) %>% 
  mutate(hs_meal = (meal_hs_1 + meal_hs_2 + meal_hs_3 + meal_hs_4) / 4) %>% 
  mutate(hs_snack = (snack_hs_1 + snack_hs_2 + snack_hs_3 + snack_hs_4) / 4) %>% 
  mutate(hs_share = (share_hs_1 + share_hs_2 + share_hs_3 + share_hs_4) / 4)

t2df <- t2df %>% 
  mutate(hs_stock = (stock_hs_1 + stock_hs_2 + stock_hs_3 + stock_hs_4) / 4) %>% 
  mutate(hs_meal = (meal_hs_1 + meal_hs_2 + meal_hs_3 + meal_hs_4) / 4) %>% 
  mutate(hs_snack = (snack_hs_1 + snack_hs_2 + snack_hs_3 + snack_hs_4) / 4) %>% 
  mutate(hs_share = (share_hs_1 + share_hs_2 + share_hs_3 + share_hs_4) / 4)

t3df <- t3df %>% 
  mutate(hs_stock = (stock_hs_1 + stock_hs_2 + stock_hs_3 + stock_hs_4) / 4) %>% 
  mutate(hs_meal = (meal_hs_1 + meal_hs_2 + meal_hs_3 + meal_hs_4) / 4) %>% 
  mutate(hs_snack = (snack_hs_1 + snack_hs_2 + snack_hs_3 + snack_hs_4) / 4) %>% 
  mutate(hs_share = (share_hs_1 + share_hs_2 + share_hs_3 + share_hs_4) / 4)

# Save new csv per measure to github repo
write.csv(t0df, file = "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/R_tape/data/t0_anon.csv", row.names = F)
write.csv(t1df, file = "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/R_tape/data/t1_anon.csv", row.names = F)
write.csv(t2df, file = "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/R_tape/data/t2_anon.csv", row.names = F)
write.csv(t3df, file = "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/R_tape/data/t3_anon.csv", row.names = F)

####### RUN UP TO HERE TO GENERATE NEW DATA #######

# this is wrong. it needs to be longer, where the variables which are shared among the datasets stay in one column, and then the time column can differentiate between the different times. Then the descriptives will be across all times
# Need to remove the suffixes to make the colnames the same when merging so they collapse into one. Maybe suffixes can be added later, might be redundant. 
merged_data <- bind_rows(t0df, t1df, t2df, t3df)

# Calculate FW difference scores
merged_data <- merged_data %>% 
  arrange(id, time) %>% 
  group_by(id) %>% 
  mutate(
    fw_g_diff = fw_g - lag(fw_g,1)
  ) %>% 
  ungroup()

write.csv(merged_data, file = "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/R_tape/data/tall_anon.csv", row.names = F)
