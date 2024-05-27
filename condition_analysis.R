# correct for people weighing wrong
t0df$fw_g[id = c("2", "13")] <- fw_g + 373 #unless these weights were already under 373 to begin with, then it would be double

t0df <- t0 %>% 
  filter(Status == 0, Progress == 100) %>% 
  mutate(RecipientEmail = tolower(RecipientEmail)) %>% 
  select(!c(1:11,13:17)) 

t1df <- t1 %>% 
  filter(Status == 0, Progress == 100) %>% 
  select(!c(1:11,13:17)) %>% 
  mutate(RecipientEmail = tolower(RecipientEmail))

t2df <- t2 %>% 
  filter(Status == 0, Progress == 100) %>% 
  select(!c(1:11,13:17)) %>% 
  mutate(RecipientEmail = tolower(RecipientEmail))


assign_participant_ids <- function(data, id_col = "id") {
  emails <- unique(t0df$RecipientEmail)
  ids <- seq_along(emails)
  id_mapping <- data.frame(email = emails, id = ids)
  left_join(data, id_mapping, by = c("RecipientEmail" = "email"))
}

t0df <- assign_participant_ids(t0df) #%>% select(!RecipientEmail)
t1df <- assign_participant_ids(t1df) #%>% select(!RecipientEmail)
t2df <- assign_participant_ids(t2df) #%>% select(!RecipientEmail)
random <- left_join(random, id_mapping, by = c("RecipientEmail" = "email")) #%>% select(!RecipientEmail)
fw_manual <- left_join(fw_manual, id_mapping, by = c("RecipientEmail" = "email")) %>% select(!RecipientEmail)
t0df <- left_join(t0df, id_mapping, by = c("RecipientEmail" = "email")) %>% select(!RecipientEmail)
t1df <- left_join(t1df, id_mapping, by = c("RecipientEmail" = "email")) %>% select(!RecipientEmail)
t2df <- left_join(t2df, id_mapping, by = c("RecipientEmail" = "email")) %>% select(!RecipientEmail)
fw_manual <- fw_manual %>%  rename("fw_g_t2" = "fw_g")

t0df <- t0df %>%
  left_join(select(fw_manual, id, fw_g_t0), by = "id") %>%
  left_join(select(random, id, condition), by = "id") %>% 
  mutate(fw_g = ifelse(!is.na(fw_g_t0), fw_g_t0, fw_g)) %>%
  select(-fw_g_t0)

t1df <- t1df %>%
  left_join(select(fw_manual, id, fw_g_t1), by = "id") %>%
  left_join(select(random, id, condition), by = "id") %>% 
  mutate(fw_g = ifelse(!is.na(fw_g_t1), fw_g_t1, fw_g)) %>%
  select(-fw_g_t1)

t2df <- t2df %>%
  left_join(select(fw_manual, id, fw_g_t2), by = "id") %>%
  left_join(select(random, id, condition), by = "id") %>% 
  mutate(fw_g = ifelse(!is.na(fw_g_t2), fw_g_t2, fw_g)) %>%
  select(-fw_g_t2)

merged_data <- t0df %>%
  left_join(t1df, by = "id", suffix = c("_t0", "_t1")) %>%
  left_join(t2df, by = "id")

merge_df <- merge(merged_data, fw_manual, by = "id")

merge_long <- pivot_longer(merged_data, cols = starts_with("fw_g"), names_to = "time", names_prefix = "fw_g_", values_to = "fw_g") %>% 
  select(id, fw_g, time, condition)

merge_long$time <- as.factor(merge_long$time)
merge_long$condition <- as.factor(merge_long$condition)
merge_long$fw_g <- as.numeric(merge_long$fw_g)

merge_long$fw_g <- ifelse(merge_long$fw_g < 373, merge_long$fw_g + 373, merge_long$fw_g)
merge_long$fw_g <- merge_long$fw_g - 373
merge_long <- merge_long %>%
  mutate(log_fw_g = log(fw_g + 1))
hist(merge_long$log_fw_g)

aovtime <- aov(log_fw_g ~ time + condition + time*condition, data = merge_long)
summary(aovtime)

ggplot(na.omit(merge_long), aes(x = time, y = log_fw_g, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Time", y = "log_fw_g") +
  ggtitle("log_fw_g by Time") +
  theme_minimal()


merge_long_subset <- subset(merge_long, time %in% c("t0", "t1"))
aov_t01 <- aov(log_fw_g ~ time * condition, data = merge_long_subset)
summary(aov_t01)

ggplot(na.omit(merge_long_subset), aes(x = time, y = log_fw_g, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Time", y = "log_fw_g") +
  ggtitle("log_fw_g by Time") +
  theme_minimal()


hist(merge_long$fw_g)
