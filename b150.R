View(video_games_sales)

#Clean the data and keep the needed variables
video_games_sales <- video_games_sales[, c("genre", "global_sales")]
video_games_sales <- na.omit(video_games_sales)

#Create Sports vs Other groups
video_games_sales$genre_group <- ifelse(video_games_sales$genre == "Sports",
                                        "Sports",
                                        "Other")

video_games_sales$genre_group <- factor(video_games_sales$genre_group,
                                        levels = c("Sports", "Other"))


#Boxplot
boxplot(global_sales ~ genre_group,
        data = video_games_sales,
        main = "Global Sales: Sports vs Other Genres",
        xlab = "Genre Group",
        ylab = "Global Sales (millions)")

#Histogram
hist(video_games_sales$global_sales,
     breaks = 30,
     main = "Distribution of Global Video Game Sales",
     xlab = "Global Sales (millions)")

#Save the Boxplots as image
png("boxplot_genre_group.png", width = 800, height = 600)

boxplot(global_sales ~ genre_group,
        data = video_games_sales,
        main = "Global Sales: Sports vs Other Genres",
        xlab = "Genre Group",
        ylab = "Global Sales (millions)")

dev.off()




