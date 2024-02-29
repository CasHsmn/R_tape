# LOAD DATA #

# libraries
library(dplyr)

# setup wd
wd <- list()
wd$data <-"C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/data/"
wd$output <- "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study 2/tapeStudy/output/"

# READ
part <- read.csv(paste0(wd$data, "consent.csv"))
t0 <- read.csv(paste0(wd$data, "t0.csv"))
manual <- read.csv(paste0(wd$data, "final_participants.csv"))
t0 <- t0[!is.na(t0$RecipientEmail) & t0$RecipientEmail != "", ]
part$email <- paste(part$RecipientEmail, part$mail)


t0$RecipientEmail <- tolower(trimws(as.character(t0$RecipientEmail)))
part$email <- tolower(trimws(as.character(part$email)))
manual$email <- tolower(trimws(as.character(manual$email)))

df <- merge(t0, part[,c("email", "hh_size")], by.x = "RecipientEmail", by.y = "email", all.x = TRUE)
df$hh_size[3] <- 25
df <- df[-40,]

merged_df <- merge(df, manual[, c("email", "fw")], by.x = "RecipientEmail", by.y = "email", all.x = TRUE)
merged_df$fw_g <- as.numeric(merged_df$fw_g)
merged_df$fw_g[is.na(merged_df$fw_g)] <- merged_df$fw[is.na(merged_df$fw_g)]
merged_df <- merged_df[-1,]

merged_df$fw_g[22] <- 1341
merged_df$fw_g[41] <- 568
merged_df$fw_g[49] <- 373
