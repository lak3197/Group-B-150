# Analysis.R
# Full pipeline for "Sports vs Other" analysis using video_games_sales
# Save this file as Analysis.R and run in RStudio (set working directory first)

# 1. Load dataset
View(video_games_sales)

#2.Clean the data and keep the needed variables
video_games_sales <- video_games_sales[, c("genre", "global_sales")]
video_games_sales <- na.omit(video_games_sales)

#3.Create Sports vs Other groups
video_games_sales$genre_group <- ifelse(video_games_sales$genre == "Sports",
                                        "Sports",
                                        "Other")

video_games_sales$genre_group <- factor(video_games_sales$genre_group,
                                        levels = c("Sports", "Other"))

#4.Summary statistics for Sports vs Other
group_counts <- table(video_games_sales$genre_group)
group_means  <- tapply(video_games_sales$global_sales,
                       video_games_sales$genre_group,
                       mean)
group_sds    <- tapply(video_games_sales$global_sales,
                       video_games_sales$genre_group,
                       sd)

summary_stats <- data.frame(
  genre_group = names(group_counts),
  count       = as.numeric(group_counts),
  mean        = as.numeric(group_means),
  sd          = as.numeric(group_sds)
)

print(summary_stats)

#5.Normality Test for Sports

sports_only <- subset(video_games_sales, genre_group == "Sports")$global_sales
other_only  <- subset(video_games_sales, genre_group == "Other")$global_sales

shapiro_sports <- shapiro.test(if(length(sports_only) > 5000) sample(sports_only, 5000) else sports_only)
shapiro_other  <- shapiro.test(if(length(other_only) > 5000) sample(other_only, 5000) else other_only)

# print results
print("Shapiro-Wilk test for Sports (possibly sampled):")
print(shapiro_sports)
print("Shapiro-Wilk test for Other (possibly sampled):")
print(shapiro_other)


#6.Independent samples t-test Sports vs Other
t_test_result <- t.test(global_sales ~ genre_group,
                        data = video_games_sales,
                        var.equal = FALSE)   # Welch t-test

print(t_test_result)

#7.Boxplot
boxplot(global_sales ~ genre_group,
        data = video_games_sales,
        main = "Global Sales: Sports vs Other Genres",
        xlab = "Genre Group",
        ylab = "Global Sales (millions)")

#Save the Boxplots as image
png("boxplot_genre_group.png", width = 800, height = 600)

boxplot(global_sales ~ genre_group,
        data = video_games_sales,
        main = "Global Sales: Sports vs Other Genres",
        xlab = "Genre Group",
        ylab = "Global Sales (millions)")

dev.off()

#8.Histogram
hist(video_games_sales$global_sales,
     breaks = 30,
     main = "Distribution of Global Video Game Sales",
     xlab = "Global Sales (millions)")

#Save the Histogram as image
png("hist_global_sales.png", width = 800, height = 600)

hist(video_games_sales$global_sales,
     breaks = 30,
     main = "Distribution of Global Video Game Sales",
     xlab = "Global Sales (millions)")

dev.off()



#9.Scatterplot: category vs sales (jittered for clarity)

video_games_sales$genre_numeric <- as.numeric(video_games_sales$genre_group)

png("scatterplot_genre_sales.png", width = 900, height = 700)
# add jitter to x so points don't overlap exactly
plot(jitter(video_games_sales$genre_numeric, amount = 0.15),
     video_games_sales$global_sales,
     main = "Scatterplot of Global Sales: Sports vs Other (jittered)",
     xlab = "Genre Group (1 = Sports, 2 = Other)",
     ylab = "Global Sales (millions)",
     pch = 16,
     cex = 0.6,
     col = ifelse(video_games_sales$genre_group == "Sports", "blue", "red"),
     xaxt = "n")
axis(1, at = c(1,2), labels = c("Sports", "Other"))
dev.off()


# 10. Export cleaned dataset used for analysis (optional)

write.csv(video_games_sales, "video_games_sales_cleaned.csv", row.names = FALSE)

# 11. Write a short results summary to a text file

sink("results_summary.txt")
cat("Summary of analysis: Sports vs Other (global_sales)\n\n")
cat("Summary statistics (saved to summary_stats_by_genre_group.csv):\n")
print(summary_stats)
cat("\nShapiro-Wilk test results (note: large samples may yield significant p-values):\n")
print(shapiro_sports)
print(shapiro_other)
cat("\nVariance tests:\n")
print(bartlett_res)
if (!is.null(levene_res)) { print(levene_res) } else { cat("Levene's test not run (package 'car' not installed)\n") }
cat("\nWelch t-test result (saved to t_test_result.txt):\n")
print(t_test_result)
cat("\nInterpretation: If p-value > 0.05, we fail to reject the null hypothesis; there is no evidence of a difference in mean global sales between Sports and Other.\n")
sink()


