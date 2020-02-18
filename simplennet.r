library(nnet)
library(ggplot2)

file <- file("data.csv", open="r", encoding="shift-jis")
data <- read.table(file, header=T, sep=",", fill = TRUE)

time = data$time
cost = data$cost
factor1 = data$factor1

#learning phase
nn <- nnet(cost~time+factor1, data, size=length(time)/2, linout=TRUE)
#cost,o—Í‘w time,“ü—Í‘w linout,TRUE‚¾‚ÆŽÀ”

testing <- time+5

#predicting phase
output <- predict(nn, testing)

d1 <- data.frame(x=time, y=cost, DATA='Actual')
d2 <- data.frame(x=time+5, y=output, DATA='Estimate')
d <- rbind(d1, d2)

#display
png("nn.png", height=800, width=1280)
g <- ggplot(data=d, aes(x, y, geom="line", colour=DATA)) + xlim(0,length(testing)+5) + geom_line()  + xlab('TIME (DAYS)') + ylab('COST') + scale_colour_manual(values=c("black","red")) + theme_bw(base_size=8)
g
dev.off()

mse <- -2*nn$value[1]+2*2
abs (mse)

(1/length(time))*sum((cost-output)^2) #ŽÀ”’l‚ÆÔü‚ÌŒë·