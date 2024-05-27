# MATCHED PAIR RANDOMISATION #
library(nbpMatching)

set.seed(0934)

set.seed(0204) # seed randomisation round 2

X_mat <- cbind(merged_df$gender, merged_df$hh_size, merged_df$fw_g)

dist_mat_covs <- round(dist(X_mat, diag = T, upper = T), 1)
dist_mat <- as.matrix(dist_mat_covs)
nbpdist <- distancematrix(dist_mat)
nbpmatches <- nonbimatch(nbpdist)
assignment <- assign.grp(nbpmatches)

randomise <- data.frame(email = merged_df$RecipientEmail, phone = merged_df$phone, hh_size = merged_df$hh_size, gender = merged_df$gender, fw = merged_df$fw_g, condition = assignment$treatment.grp)

# export
write.csv(randomise, paste0(wd$output, "randomisation_r2.csv"), row.names = F)

# check randomisation
# table(randomise$gender, randomise$condition)
# boxplot(hh_size ~ condition, data = randomise)
# boxplot(fw ~ condition, data = randomise)


# Create a balanced sample based on gender, food waste, and household size
sampled_rows <- merged_df %>%
  group_by(gender, fw_g, hh_size) %>%
  sample_frac(size = 0.8) %>%
  ungroup() %>% 
  mutate(group_assignment = assignment)

colnames(sampled_rows)
