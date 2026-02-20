install.packages("ggplot2")
library(ggplot2)
library(boot)

data <- read.csv("master.csv")

data$visual <- factor(data$visual, 
levels = rev(names(sort(tapply(data$error, data$visual, mean)))))

bootstrap_summary <- function(x) {
    b <- boot(x, statistic = function(data, i) mean(data[i]), R = 1000)

    ci <- quantile(b$t, c(0.025, 0.975))

    return(data.frame(
        y = mean(x),
        ymin = ci[1],
        ymax = ci[2]
    ))
}

p <- ggplot(data, aes(x = visual, y = error)) +
    stat_summary(fun.data = bootstrap_summary, geom = "pointrange", color = "blue") +
    ylab("Error") +
    xlab("Visualization Type") +
    coord_flip() +
    theme_minimal()

print(p)