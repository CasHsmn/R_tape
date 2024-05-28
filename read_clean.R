# LOAD DATA #

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# setup wd
wd <- list()
wd$data <-"C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/data/"
wd$output <- "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/output/"

# LOAD DATA
part <- read.csv(paste0(wd$data, "consent_r2.csv"))
t0 <- read.csv(paste0(wd$data, "t0_r2.csv"))
manual <- read.csv(paste0(wd$data, "final_participants.csv"))

signupconsent <- read.csv(paste0(wd$data, "signup_consent.csv"))
t0 <- read.csv(paste0(wd$data, "T0_raw_final.csv"))
t1 <- read.csv(paste0(wd$data, "T1_raw_final.csv")) %>% tolower(RecipientEmail)
t2 <- read.csv(paste0(wd$data, "T2_raw_v1.csv")) %>% tolower(RecipientEmail)
fw_manual <- read.csv(paste0(wd$data, "manual_fw_g.csv")) %>% select(-phone) %>% rename(RecipientEmail = email) %>% mutate(RecipientEmail = tolower(RecipientEmail))
random <- read.csv(paste0(wd$data, "randomisation.csv")) %>% rename(RecipientEmail = email) %>% mutate(RecipientEmail = tolower(RecipientEmail))

t0 <- t0[!is.na(t0$RecipientEmail) & t0$RecipientEmail != "", ]
t0 <- t0[-c(1:2),]
part$email <- paste(part$RecipientEmail, part$mail)
part <- part[-c(1:2),]
part$hh_size <- as.numeric(part$hh_size)
part$hh_size <- part$hh_size + 1

t0$RecipientEmail <- tolower(trimws(as.character(t0$RecipientEmail)))
part$email <- tolower(trimws(as.character(part$email)))
manual$email <- tolower(trimws(as.character(manual$email)))
part$email[part$email == "annerooseikelenboomg@wur.nl"] <- "anneroos.eikelenboom@wur.nl"

df <- merge(t0, part[,c("email", "hh_size")], by.x = "RecipientEmail", by.y = "email", all.x = TRUE)

merged_df <- merge(df, manual[, c("email", "phone", "fw")], by.x = "RecipientEmail", by.y = "email", all.x = TRUE)
merged_df$fw_g <- as.numeric(merged_df$fw_g)
merged_df$fw_g[is.na(merged_df$fw_g)] <- merged_df$fw[is.na(merged_df$fw_g)]
merged_df$fw_g[merged_df$RecipientEmail == "sajdunnink@gmail.com"] <- merged_df$fw_g[merged_df$RecipientEmail == "sajdunnink@gmail.com"] + 373
merged_df$fw_g <- ifelse(merged_df$fw_g < 373, merged_df$fw_g + 373, merged_df$fw_g)
merged_df$fw_g <- merged_df$fw_g - 373

merged_df$hh_size <- as.numeric(merged_df$hh_size)
merged_df$fw_g <- as.numeric(merged_df$fw_g)
