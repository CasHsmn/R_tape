# MATCHED PAIR RANDOMISATION #
library(MatchIt)

merged_df$condition <- as.integer((1:nrow(merged_df)) %% 2 == 0)
matched_pairs <- match.data(matchit(condition ~ hh_size + fw_g + gender, data = merged_df, method = "nearest", distance = "mahalanobis"))

