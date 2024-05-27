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
t2 <- read.csv(paste0(wd$data, "T2_raw_v1.csv")) %>% 
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
signupconsentdf <- assign_participant_ids(signupconsent) %>% select(!RecipientEmail)
randomdf <- assign_participant_ids(random) %>% select(!RecipientEmail)
fw_manualdf <- assign_participant_ids(fw_manual) %>% select(!RecipientEmail)

# Add the indirect weights and condition to the dfs
t0df <- t0df %>%
  left_join(select(fw_manualdf, id, fw_g_t0), by = "id") %>%
  left_join(select(randomdf, id, condition), by = "id") %>% 
  left_join(select(signupconsentdf, id, hh_size), by = "id") %>% 
  mutate_at(c("fw_g","hh_size"), as.numeric) %>% 
  mutate(fw_g = ifelse(!is.na(fw_g_t0), fw_g_t0, fw_g)) %>%
  mutate(fw_g = ifelse(fw_g < 378, fw_g + 373, fw_g)) %>% # add the weight of the bin if the submitted weight is < than the weight of the bin (+5g for some deviations in bin weight)
  mutate(fw_g = fw_g - 373) %>% #subtract weight of bin
  select(-fw_g_t0) %>% 
  mutate(condition = factor(condition, levels = c(0,1), labels = c("control", "intervention"))) %>% 
  mutate(time = 0)

t1df <- t1df %>%
  left_join(select(fw_manualdf, id, fw_g_t1), by = "id") %>%
  left_join(select(randomdf, id, condition), by = "id") %>% 
  left_join(select(signupconsentdf, id, hh_size), by = "id") %>%
  mutate_at(c("fw_g","hh_size"), as.numeric) %>% 
  mutate(fw_g = ifelse(!is.na(fw_g_t1), fw_g_t1, fw_g)) %>%
  mutate(fw_g = ifelse(fw_g < 378, fw_g + 373, fw_g)) %>% # add the weight of the bin if the submitted weight is < than the weight of the bin (+5g for some deviations in bin weight)
  mutate(fw_g = fw_g - 373) %>% #subtract weight of bin
  select(-fw_g_t1) %>% 
  mutate(condition = factor(condition, levels = c(0,1), labels = c("control", "intervention"))) %>% 
  mutate(time = 1)

t2df <- t2df %>%
  left_join(select(fw_manualdf, id, fw_g_t2), by = "id") %>%
  left_join(select(randomdf, id, condition), by = "id") %>% 
  left_join(select(signupconsentdf, id, hh_size), by = "id") %>%
  mutate_at(c("fw_g","hh_size"), as.numeric) %>% 
  mutate(fw_g = ifelse(!is.na(fw_g_t2), fw_g_t2, fw_g)) %>%
  mutate(fw_g = ifelse(fw_g < 378, fw_g + 373, fw_g)) %>% # add the weight of the bin if the submitted weight is < than the weight of the bin (+5g for some deviations in bin weight)
  mutate(fw_g = fw_g - 373) %>% #subtract weight of bin
  select(-fw_g_t2) %>% 
  mutate(condition = factor(condition, levels = c(0,1), labels = c("control", "intervention"))) %>% 
  mutate(time = 2)


# Save new csv per measure to github repo
write.csv(t0df, file = "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/R_tape/data/t0_anon.csv", row.names = F)
write.csv(t1df, file = "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/R_tape/data/t1_anon.csv", row.names = F)
write.csv(t2df, file = "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/R_tape/data/t2_anon.csv", row.names = F)

####### RUN UP TO HERE TO GENERATE NEW DATA #######
t0df <- t0df %>% rename_with(~paste0(., "_t0"), -id)
t1df <- t1df %>% rename_with(~paste0(., "_t1"), -id)
t2df <- t2df %>% rename_with(~paste0(., "_t2"), -id)  

merged_data <- t0df %>%
  full_join(t1df, by = "id") %>%
  full_join(t2df, by = "id")

write.csv(merged_data, file = "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/R_tape/data/tall_anon.csv", row.names = F)
