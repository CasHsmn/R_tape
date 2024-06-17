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

# habit strength of taking stock predicted by condition and time
lme_stockhs <- lme(hs_stock ~ condition*time, random = ~1|id, data = tall)
summary(lme_stockhs)

emmeans_stockhs <- emmeans(lme_stockhs, specs = ~ condition | time)
contrasts_stockhs <- contrast(emmeans_stockhs, method = "pairwise", adjust = "tukey")
contrasts_time_stockhs <- contrast(emmeans_stockhs, method = "pairwise", by = "condition")

emmeans_df <- as.data.frame(emmeans_stockhs)

ggplot(emmeans_df, aes(x = time, y = emmean, color = condition, group = condition)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(title = "Estimated Marginal Means of habit strength of stock checking by Condition and Time",
       x = "Time",
       y = "Habit strength",
       color = "Condition") +
  theme_minimal()

# habit strength of using near expired products in meals predicted by condition and time
lme_mealhs <- lme(hs_meal ~ condition*time, random = ~1|id, data = tall)
summary(lme_sharehs) # no significant effect for using near expired foods in meals whatsoever

# habit strength of letting others know that food is near expiry so they can use it predicted by condition and time
lme_sharehs <- lme(hs_share ~ condition*time, random = ~1|id, data = tall)
summary(lme_sharehs)

# habit strength of snacking near-expired product predicted by condition and time
lme_snackhs <- lme(hs_snack ~ condition*time, random = ~1|id, data = tall)
summary(lme_snackhs)


# BAR PLOT OF STOCK CHECK FREQUENCY BY TIME
avg_hs <- tall %>%
  group_by(time, condition) %>%
  summarise(avg_hs_stock = mean(hs_stock),
            avg_hs_meal = mean(hs_meal),
            avg_hs_snack = mean(hs_snack),
            avg_hs_share = mean(hs_share),
            avg_freq_stock = mean(stock_freq_1, na.rm =T),
            avg_freq_meal = mean(meal_freq_1, na.rm =T),
            avg_freq_snack = mean(snack_freq_1, na.rm =T),
            avg_freq_share = mean(share_freq_1, na.rm =T))

ggplot(avg_hs, aes(x = time, y = avg_hs_stock, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal()

ggplot(avg_hs, aes(x = time, y = avg_hs_meal, fill = time)) +
  geom_bar(stat = "identity") +
  theme_minimal()



testfwlmestock <- lmer(fw_g ~ condition*time, random = ~1|id, data = tall)
summary(testfwlmestock)


# Ensure 'condition' is a factor
tall$condition <- as.factor(tall$condition)

# Fit the mediator model
mediator_model <- lmer(hs_stock ~ condition + time + (1|id), data = tall)

# Fit the outcome model
outcome_model <- lmer(fw_g_log ~ condition + time + hs_stock + (1|id), data = tall)

# Conduct the mediation analysis
mediation_result <- mediate(mediator_model, outcome_model, treat = "condition", mediator = "hs_stock", treat.value = "intervention", control.value = "control", sims = 1000)

# Summary of the mediation analysis
summary(mediation_result)

tall$time <- as.factor(tall$time)
talllong <- pivot_longer(tall, names_to = "time")

?pivot_longer

t2tall <- tall %>% 
  subset(time =="follow-up")



lm(fw_g_log ~ condition + hs_stock, data=t2tall)
summary(lm(fw_g_log ~ condition, data=t2tall))
?subset

# ANALYSIS WITH DIFFERENCE SCORES
library(car)
tall <- tall %>%
  mutate(
    fw_g_diff = replace_na(fw_g_diff, 0))

lm_fw_diff <- lme(fw_g_diff ~ condition * time, random = ~1 | id, data = tall)
flextable(tidy(nlme_fw)) %>% colformat_double(digits = 2)

aov_fw_diff <- Anova(dv=fw_g_diff, wid=id, within=time)
?Anova
?anova_test
plot(lm_fw_diff)
