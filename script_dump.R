part <- read.csv(paste0(wd$data, "consent_r2.csv"))
t0 <- read.csv(paste0(wd$data, "t0_r2.csv"))
t0_r2 <- read.csv(paste0(wd$data, "t0_r2.csv"))
manual <- read.csv(paste0(wd$data, "final_participants.csv"))
t0 <- t0[!is.na(t0$RecipientEmail) & t0$RecipientEmail != "", ]
t0 <- t0[-c(1:2),]
part$email <- paste(part$RecipientEmail, part$mail)
part <- part[-c(1:2),]
part$hh_size <- as.numeric(part$hh_size)
part$hh_size <- part$hh_size + 1

# select only the rows with new round 2 participants
manual <- manual %>% 
  slice((n() - 12):n())
t0 <- t0 %>% 
  slice((n() - 11):n())
part <- part %>% 
  slice((n() - 12):n())

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

library(table1)

t0$gender_t0[t0$gender_t0 == 4] <- 3
t0$gender_t0 <- factor(t0$gender_t0, levels = c(1,2,3), labels = c("Female", "Male", "Other"))
t0$condition_t0 <- factor(t0$condition_t0, levels = c("intervention", "control"), labels = c("intervention", "control"))
label(t0$gender_t0) <- "Gender"
label(t0$age_t0) <- "Age"
label(t0$hh_size_t0) <- "Household size"
label(t0$fw_g_t0) <- "Baseline waste"
table1(~ gender_t0 + age_t0 + hh_size_t0 + fw_g_t0 | condition_t0, data = t0)

summary(t0$hs_stock)
summary(t0$stock_freq_1_t0)
hist(t0$hs_stock)

t0_desc <- t0 %>% 
  summarise(across(c(hs_stock, stock_freq_1_t0), list(
    mean = ~mean(., na.rm = T),
    sd = ~sd(., na.rm = T),
    min = ~min(., na.rm = T),
    max = ~max(., na.rm = T)
  )))

library(vtable)

st(t0, vars = c("pastweek_1_t0", "pastweek_2_t0", "pastweek_3_t0", "stock_freq_1_t0", "hs_stock", "meal_freq._1_t0", "hs_meal", "snack_freq_1_t0", "hs_snack", "share_freq_1_t0", "hs_share"), labels = c("Days at home", "Ate at home", "Prepared food", "Check if near expiry", "Habit: Check if near expired", "Use near expired food in meal", "Habit: Use near expired food in meal", "Snack near expired", "Habit: Snack near expired", "Share near expired", "Habit: Share near expired"), summ = c("mean(x)", "median(x)"), out = 'kable')

