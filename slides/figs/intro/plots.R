library(ISLR)
library(ggplot2)
library(dplyr)
library(xtable)
library(tidyr)

# Plot raw -----------------

dataset <- ISLR::Auto

ggplot(data = dataset_mean, aes(x = horsepower, y = mpg_average)) + geom_point() + 
    xlab("Horsepower") + 
    ylab("Miles Per Gallon (MPG)") +
    theme_minimal()
 
ggsave("auto_data.pdf", width = 15, height = 10, units = "cm")


# Plot means -------------

dataset_mean <- dataset %>% dplyr::group_by(horsepower) %>% summarize(mpg_average = mean(mpg))

ggplot(data = dataset_mean, aes(x = horsepower, y = mpg_average)) + geom_line() + 
    xlab("Horsepower") + 
    ylab("Miles Per Gallon (MPG)") +
    theme_minimal()

ggsave("auto_data_mean.pdf", width = 15, height = 10, units = "cm")

# Plot regression --------------

#model.lm <- lm(horsepower ~ mpg, dataset)

ggplot(data = dataset, aes(x = horsepower, y = mpg)) + 
    geom_point() + 
    geom_smooth(method="lm", formula = y ~ x, se=FALSE) +
    xlab("Horsepower") + 
    ylab("Miles Per Gallon (MPG)") +
    theme_minimal()

ggsave("auto_data_regression.pdf", width = 15, height = 10, units = "cm")

# Plot function --------

ggplot(data = dataset_mean, aes(x = horsepower, y = mpg_average)) + 
    geom_smooth(se=FALSE) +
    xlab("Horsepower") + 
    ylab("Miles Per Gallon (MPG)") +
    theme_minimal()

ggsave("auto_function.pdf", width = 15, height = 10, units = "cm")

# Show dataset --------------

xtable(dataset[1:10,] %>% select(mpg,horsepower,name))

# Plot k-nn ------------

hp <- seq(46,230)
knn.results <- data.frame(horsepower = hp, 
           k.1 = caret::knnregTrain(select(dataset,horsepower), data.frame(horsepower=hp), dataset$mpg, k=1),
           k.20 = caret::knnregTrain(select(dataset,horsepower), data.frame(horsepower=hp), dataset$mpg, k=20),
           k.100 = caret::knnregTrain(select(dataset,horsepower), data.frame(horsepower=hp), dataset$mpg, k=100),
           k.500 = caret::knnregTrain(select(dataset,horsepower), data.frame(horsepower=hp), dataset$mpg, k=500))

knn.results.tidy <- gather(knn.results, k.1, k.20, k.100, k.500, key = "K", value = "mpg", factor_key = TRUE)

ggplot(knn.results.tidy, aes(y=mpg, x=horsepower,color=K,series=K)) + 
    geom_line()+
    xlab("Horsepower") + 
    ylab("Miles Per Gallon (MPG)") +
    theme_minimal()

ggsave("auto_knn_compare.pdf", width = 15, height = 10, units = "cm")
           