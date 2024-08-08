library(psych)

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

str(tall$time)

lm(fw_g_log ~ condition + hs_stock, data=t2tall)
summary(lm(fw_g_log ~ condition, data=t2tall))
?subset

nlme_fwg <- lme(fw_g ~ condition * time, random = ~1 | id, data = tall)
nlme_fw <- lme(fw_g_log ~ condition * time, random = ~1 | id, data = tall)
summary(nlme_fwg)
summary(nlme_fw)


subset(t0df, condition == "control")

describeBy(t0df$fw_g_log, t0df$condition)

ggplot(t2df, aes(fw_g)) + geom_histogram(binwidth = 20)

# TESTING THE MODEL WITH FW(G) INSTEAD OF LOG TRANSFORMED TO COMPARE WITH SPSS OUTPUT
lm_fw_2_t <- lmer(fw_g ~ condition*time + (1|id), data = tall)

nlme_fw_t <- lme(fw_g ~ condition * time, random = ~1 | id, data = tall)
flextable(tidy(nlme_fw_t)) %>% colformat_double(digits = 2)
flextable(as_tibble(confint(lm_fw_2_t), rownames = "term")) %>% colformat_double(digits = 3)

# Contrasts

fw_emmeans_t <- emmeans(nlme_fw_t, specs = ~ condition | time)
contrasts_t <- contrast(fw_emmeans_t, method = "pairwise", adjust = "tukey")
contrasts_time_t <- contrast(fw_emmeans_t, method = "pairwise", by = "condition")
flextable(tidy(contrasts_time_t)) %>% colformat_double(digits = 3)

flextable(tidy(contrasts_t)) %>% colformat_double(digits = 3)

fw_emmeans_df <- as.data.frame(fw_emmeans)

ggplot(fw_emmeans_df, aes(x = time, y = emmean, color = condition, group = condition)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(title = "Estimated Marginal Means of fw_g by Condition and Time",
       x = "Time",
       y = "Log Transformed Food Waste (fw_g)",
       color = "Condition") +
  theme_minimal()

# REPEATED MEASURES ANOVA
tall$condition <- as.factor(tall$condition)
tall$time <- as.factor(tall$time)

aov_fw <- aov(fw_g ~ condition*time + Error(id/time), data = tall)
summary(aov_fw)

em_aov_fw <- emmeans(aov_fw, ~ condition * time)
con_aov_fw <- contrast(em_aov_fw, interaction = "pairwise")

summary(con_aov_fw)

ggplot(em_aov_fw, aes(x = time, y = emmean, color = condition, group = condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(title = "Interaction Plot", x = "Time", y = "fw_g", color = "Condition") +
  theme_minimal()

tall <- mutate(tall, fw_g_log = log(fw_g + 1))
aov_lme <- lme(fw_g ~ condition*time, random = ~1 | id, data = tall)
summary(aov_lme)
emlme <- emmeans(aov_lme, ~ condition*time)
conemlme <- contrast(emlme, interaction = "pairwise")
summary(conemlme)

emlme <- as.data.frame(emlme)
ggplot(emlme, aes(x = time, y = emmean, color = condition, group = condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(title = "Interaction Plot", x = "Time", y = "fw_g", color = "Condition") +
  theme_minimal()

ggplot(tall, aes(fw_g)) + geom_histogram(binwidth = 15)

lm_fw_pw <- lme(fw_g ~ hh_size, random  = ~1|id, data=tall)
summary(lm_fw_pw)

plot(aov_lme)

plot(resid(aov_lmer), tall$fw_g)

# assumptions for non transformed fw
aov_lmer <- lmer(fw_g ~ condition*time + (1|id), data = tall)
plot(aov_lmer)
plot(resid(aov_lmer), tall$fw_g) # linearity of residuals vs observed
qqmath(aov_lmer) # normality
hist(resid(aov_lmer))

# assumptions for log transformed fw
fw_log_lmer <- lmer(fw_g_log ~ condition*time + (1|id), data=tall)
plot(resid(fw_log_lmer), tall$fw_g_log)
plot(fw_log_lmer)
qqmath(fw_log_lmer)
hist(resid(fw_log_lmer))

library(robustlmm)
fw_rlmer <- robustlmm::rlmer(fw_g ~ condition*time + (1|id), data = tall)
?rlmer

# DIFFERENCE SCORE MATRIX
diffdf <- tall %>% 
  dplyr::select(fw_g, time, hs_stock, hs_meal, condition, id)

diffdf <- diffdf %>% 
  pivot_wider(names_from = time, values_from = c(hs_stock, hs_meal, fw_g), names_prefix = "t")

diffdf <- diffdf %>% 
  mutate(fw_diff = fw_g_t2 - fw_g_t0,
         hs_stock_diff = hs_stock_t2 - hs_stock_t0,
         hs_meal_diff = hs_meal_t2 - hs_meal_t0)

difflm <- lm(fw_diff ~ condition + hs_meal_diff, data = diffdf)
diffmediate <- lm(hs_meal_diff ~ condition, data = diffdf)

fw_hs_mediation <- mediate(diffmediate, difflm, treat = "condition", mediator = 'hs_meal_diff',  treat.value = "intervention", control.value = "control", boot = T)
summary(fw_hs_mediation)


fw_hs_med <- '
fw_diff ~ c * condition + b*hs_diff
hs_diff ~ a* condition

mediation := a*b

total := c + mediation'

med_res <- sem(fw_hs_med, data = diffdf)
summary(med_res, standardized = T, fit.measures = T)
library(semPlot)
semPaths(med_res, rotation = 2, whatLabels = "est", style = "lisrel")
?semPaths

colnames(tall)

tall_wide <- pivot_wider(tall, id_cols = c("id", "condition"),names_from = "time", values_from = "fw_g")
write.csv(tall_wide, file =  "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/R_tape/data/tall_wide.csv", row.names = F)

hist(fw_lmer)

resid(fw_lmer)
qqnorm(resid(fw_lmer))
library(ez)
library(robustlmm)
library(Matrix)
library(tibble)
ezANOVA(dv = fw_g, wid = id, within = time, between = condition, data = tall)
?ezANOVA
resid(fw_aov)

lm_fw_2 <- lme(fw_g ~ condition * time, random = ~1 | id, data = tall)
fwlog_lmer <- lmer(fw_g_log ~ time * condition + (1|id), data = tall)

ggplot(data.frame(eta=predict(fw_lmer,type="link"),pearson=residuals(fw_lmer,type="pearson")),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()

rlm_fw <- rlmer(fw_g ~ time * condition + (1|id), data = tall)

qqnorm(resid(fw_lmer))
library(ggpubr)
shapiro.test(tall$fw_g)
ggqqplot(tall$fw_g)
plot(fw_lmer)
hist(resid(fw_lmer))
??ols_test_normality
plot(lm_fw_2)
plot(fwlog_lmer)

plot(resid(fw_lmer))
plot(fw_lmer)

## REMOVE OUTLIERS
fw_iqr <- quantile(tall$fw_g, 0.75) - quantile(tall$fw_g, 0.25)
lower <- quantile(tall$fw_g, 0.25) - 1.5*fw_iqr
upper <- lower <- quantile(tall$fw_g, 0.75) + 1.5*fw_iqr

fw_outliers <- tall %>% 
  filter(fw_g < lower | fw_g > upper) %>% 
  pull(id)


tall_nooutlier <- tall %>% 
  filter(!(id %in% fw_outliers & (fw_g < lower | fw_g > upper)))



filt_fw_lmer <- lmer(fw_g ~ time * condition + (1|id), data = tall_nooutlier)
filt_em <- emmeans(filt_fw_lmer, specs = ~ condition * time)
filt_em_df <- data.frame(filt_em)
Anova(filt_fw_lmer, test.statistic = "F")
Anova(fw_lmer, test.statistic = "F")

ggplot(filt_em_df, aes(x = time, y = emmean, color = condition, group = condition)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(title = "Estimated Marginal Means of fw_g_log by Condition and Time",
       x = "Time",
       y = "Log Transformed Food Waste (fw_g_log)",
       color = "Condition") +
  theme_minimal()
plot(filt_fw_lmer)

plot(fw_lmer)
qqnorm(tall_nooutlier$fw_g)


tall_filter1 <- tall_nooutlier %>% 
  filter(!fw_g < 10)
filt_fw_lmer_1 <- lmer(fw_g ~ time * condition + (1|id), data = tall_filter1)
plot(filt_fw_lmer_1)
Anova(filt_fw_lmer_1)
filt_1_em <- emmeans(filt_fw_lmer_1, specs = ~ condition*time)
filt_1_em_df <- data.frame(filt_1_em)
ggplot(filt_1_em_df, aes(x = time, y = emmean, color = condition, group = condition)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(title = "Estimated Marginal Means of fw_g_log by Condition and Time",
       x = "Time",
       y = "Log Transformed Food Waste (fw_g_log)",
       color = "Condition") +
  theme_minimal()
plot(filt_fw_lmer)

install.packages("brms")



## TRYING OUT BRMS FOR CENSORED MIXED MODEL
## this might work? but is pretty complex https://bookdown.org/content/3686/tools-in-the-trunk.html#censored-data-in-jags-brms
# maybe also try just censreg with the difference score from pre to post2

lower_bound <- 2
upper_bound <- 8
tall$fw_g_log <- pmin(pmax(tall$fw_g_log, lower_bound), upper_bound)
tall$cens <- ifelse(tall$fw_g_log <= lower_bound, "left", 
                    ifelse(tall$fw_g_log >= upper_bound, "right", "none"))

tall_cens <- tall
tall_cens$fw_g <- ifelse(tall_cens$fw_g < 10, 0, tall_cens$fw_g)
tall_cens$cens <- ifelse(tall_cens$fw_g <10, "left", "none")

tobit <- brm(
  bf(fw_g | cens(cens) ~ condition * time + (1 | id)),
  data = tall_cens,
  family = student(),
  chains = 4,
  iter = 2000,
  control = list(adapt_delta = 0.95)
)


filterfwlmer <- lmer(fw_g ~ used_tape*time + (1|id), data = tall)
Anova(filterfwlmer)
str(tall$used_tape)

tall <- tall %>% 
  mutate(used_tape = ifelse(is.na(used_tape) & condition == "intervention",0, used_tape))
tall <- tall %>% 
  mutate(used_tape = ifelse(condition == "control",2, used_tape))

tall$used_tape <- factor(tall$used_tape, levels = c(0,1,2), labels = c("no", "yes", "control"))

## ANALYSIS SEPARATING PEOPLE WHO DID AND DID NOT USE TAPE ##
tall %>%
  group_by(used_tape, time) %>% 
  summarise(
    n=n(),
    mean=mean(fw_g),
    sd=sd(fw_g)
  ) %>% 
  mutate(se = sd/sqrt(n)) %>% 
  mutate(ci=se*qt((1-0.05)/2 + .5, n-1)) %>% 
  ggplot(aes(x = time, y = mean, fill = used_tape)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Food waste (g) over time by condition",
       x = "Time",
       y = "Food Waste (g)",
       fill = "Used tape")+
  theme(text = element_text(size = 16))

ggsave(paste0(wd$output, "fw_by_tape_use.png"), dpi = 300)

fw_aov <- anova_test(dv = fw_g, wid = id, within = time, between = used_tape, data = tall)
get_anova_table(fw_aov)

used_tape_lmer.1 <- lmer(fw_g ~ 1 + (1|id), REML = F, data = tall)
used_tape_lmer.2 <- update(used_tape_lmer.1, .~. + condition)
used_tape_lmer.3 <- update(used_tape_lmer.2, .~. + time)
used_tape_lmer.4 <- update(used_tape_lmer.3, .~. + condition:time)
used_tape_lmer.f <- lmer(fw_g ~ condition*time + (1|id), REML = F, data = tall)
anova(used_tape_lmer.1,used_tape_lmer.1, used_tape_lmer.2, used_tape_lmer.3,used_tape_lmer.4)

nova(used_tape_lmer.1,used_tape_lmer.f)
?anova
glht()

Anova(used_tape_lmer, test = "F")
flextable(data.frame(Anova(used_tape_lmer, test = "F"))%>% rownames_to_column(".")) %>% colformat_double(digits = 4)

fw_lmer <- lmer(fw_g ~ condition*time + (1|id), data = tall)
Anova(fw_lmer, test = "F")
flextable(data.frame(Anova(fw_lmer, test = "F"))%>% rownames_to_column(".")) %>% colformat_double(digits = 4)


fw_rmaov <- anova_test(dv = fw_g, wid = id, within = time, between = condition, data = tall)
flextable(data.frame(get_anova_table(fw_rmaov)))
aov(fw_g ~ time*condition + Error(1|id), data = tall)


ggqqplot(tall, "fw_g") + facet_grid(time ~ condition)
library(ggpubr)
tall %>% 
  group_by(condition, time) %>% 
  shapiro_test(fw_g)
ggboxplot(tall, x = "time", y = "fw_g", color = "condition")

used_tape_em <- emmeans(used_tape_lmer, specs = ~ used_tape | time)
used_tape_contrasts_time <- contrast(used_tape_em, method = "pairwise", by = "used_tape")
used_tape_contrasts <- contrast(used_tape_em, method = "pairwise")

used_tape_em_df <- as.data.frame(used_tape_em)

ggplot(used_tape_em_df, aes(x = time, y = emmean, color = used_tape, group = used_tape)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(
       x = "Time",
       y = "Food waste",
       color = "Condition") +
  ylim(0,400)+
  theme_minimal()+
  theme(text = element_text(size = 18))

ggsave(paste0(wd$output, "usedtapeem.png"))

fw_lmer_em <- emmeans(fw_lmer, specs = ~ condition | time)
fw_lmer_con <- contrast(fw_lmer_em, method = "pairwise", by = "condition")
ezANOVA(data=tall, dv = .(fw_g), wid = .(id), within = .(time), between = .(condition), detailed = T, type = 3)

fw_lmer_em_df <- as.data.frame(fw_lmer_em)

ggplot(fw_lmer_em_df, aes(x = time, y = emmean, color = condition, group = condition)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(
    x = "Time",
    y = "Food waste",
    color = "Condition") +
  ylim(0,400)+
  theme_minimal()+
  theme(text = element_text(size = 18))

ggsave(paste0(wd$output, "conditionem.png"))

test_use <- aov(hh_size ~ used_tape, data = t0)

summary(test_use)

compair <- tall %>% 
  group_by(time) %>% 
  anova_test(dv = fw_g, wid = id, between = used_tape) %>% 
  get_anova_table() %>% 
  adjust_pvalue(method = "bonferroni")

get_anova_table(compair)
tall$time <- droplevels(tall$time)

tall %>% 
  group_by(time) %>% 
  pairwise_t_test(fw_g ~ condition, paired = T, p.adjust.method = "bonferroni")

tukey_hsd(fw_aov$ANOVA, condition ~ time)
ezANOVA(dv = fw_g, wid = id, within = time, between = condition, data = tall)

library(ez)
?Anova

str(tall$time)
get_anova_table(compair)
ggqqplot(tall, "fw_g")+
  facet_grid(time ~ condition)

tall %>%
  group_by(id) %>%
  summarise(time_count = n_distinct(time)) %>%
  filter(time_count == 3)

sum(tall$condition == "intervention")

tall %>%
  filter(condition == "control") %>%
  distinct(id) %>%
  n_distinct() %>%
  print()

tall %>%
  filter(condition == "control") %>%
  group_by(id) %>%
  filter(n_distinct(time) == 3) %>%  # Ensure all 3 time points are present
  distinct(id) %>%
  n_distinct() %>%
  print()

tall_new <- tall %>% 
  mutate(fw_g = ifelse(fw_g < 10, 0, fw_g))
sum(tall_new$fw_g == 0)

hist(tall_new$fw_g)

ggplot(tall_new, aes(x = fw_g)) +
  geom_histogram(binwidth = 20)+
  labs(title = "Histogram of food waste (g)")


fw_hs_stock <- update(used_tape_lmer.4, .~. + hs_stock)
anova(used_tape_lmer.4, fw_hs_stock)

fw_hsstock_em <- emmeans(fw_hs_stock, specs = ~ condition | time)
fw_hsstock_em_df <- data.frame(fw_hsstock_em)

ggplot(fw_hsstock_em_df, aes(x = time, y = emmean, color = condition, group = condition)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(title = "Estimated Marginal Means of habit strength of share checking by Condition and Time",
       x = "Time",
       y = "Habit strength",
       color = "Condition") +
  theme_minimal()

fw_hs_stock <- lmer(fw_g ~ hs_stock*condition*time + (1|id), data = tall)
Anova(fw_hs_stock)
fw_hs_meal <- lmer(fw_g ~ hs_meal*condition*time + (1|id), data = tall)
tukey_hsd(fw_hs_meal)
Anova(fw_hs_meal, test = "F")
fw_hs_stock_em <- emtrends(fw_hs_stock, specs = ~ condition +time | hs_stock)

fw_hs_meal_em <- emtrends(fw_hs_meal, ~ time, var = "hs_meal" )
fw_hs_meal_em$contrasts

summary(fw_hs_meal_em)
?emtrends
emmip(fw_hs_meal, time ~hs_meal, cov.reduce = range)
emmip(fw_hs_stock,time ~hs_stock, cov.reduce = range)

pairs(fw_hs_meal_em)

hs_stock_pred <- lmer(hs_stock ~ time*condition + (1|id), data = tall3)
Anova(hs_stock_pred, test = "F")
hs_stock_pred_em <- data.frame(emmeans(hs_stock_pred, specs = ~ condition | time))
ggplot(hs_stock_pred_em, aes(x = time, y = emmean, color = condition, group = condition)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(title = "Estimated Marginal Means of habit strength of share checking by Condition and Time",
       x = "Time",
       y = "Habit strength",
       color = "Condition") +
  theme_minimal()

tall_wide <- tall %>% 
  pivot_wider(id_cols = c("id", "condition"), names_from = time, values_from = c(fw_g, hs_stock, hs_meal))

tall_wide <- tall_wide %>% 
  mutate(hs_stock_diff = hs_stock_post2 - hs_stock_pre,
         hs_meal_diff = hs_meal_post,
         fw_diff = fw_g_post2 - fw_g_pre)

hsstock_med <- lm(hs_diff ~ condition, data = tall_wide)
hsmeal_med <- lm(hs_meal_diff ~ condition, data = tall_wide)

lmer(fw_g ~ hs_stock*condition*time +  (1|id), data = tall)

full_mod <- lm(fw_diff ~ hs_diff*condition, data = tall_wide)
summary(full_mod)
summary(hs_med)
boxplot(tall_wide$hs_diff ~ tall_wide$condition)

mediation_check <- mediate(hsstock_med, full_mod, treat = "condition", mediator = "hs_diff", control.value = "control", treat.value = "intervention", boot = T)
mediat <- mediate(hs_med, full_mod, treat = "condition", mediator = "hs_diff", control.value = "control", treat.value = "intervention")
summary(mediation_check)


summary(mediation_check)
summary(mediat)

Anova(lmer(fw_g ~ hs_meal*condition*time + (1|id), data = tall))
