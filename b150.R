View(video_games_sales)

video_games_sales <- video_games_sales[, c("genre", "global_sales")]
video_games_sales <- na.omit(video_games_sales)

