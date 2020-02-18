library(nnet)
library(ggplot2)
library(TTR)

file <- file("data.csv", open="r", encoding="shift-jis")
data <- read.table(file, header=T, sep=",", fill = TRUE)

time = data$time
cost = data$cost

age <- SMA(cost,20)
sx <- data$cost/age
plot(cost, type="b")
par (new=T)
plot(age, type="b", col="red")

#learning phase
nn <- nnet(cost~time+sx, data, size=length(time)/2, linout=TRUE)
#cost,o—Í‘w time,“ü—Í‘w linout,TRUE‚¾‚ÆŽÀ”

testing <- time+10
moving <- sx+14

#predicting phase
output <- predict(nn, moving)

d1 <- data.frame(x=time, y=cost, DATA='Actual')
d2 <- data.frame(x=time+14, y=output, DATA='Estimate')
d3 <- data.frame(x=time, y=age, DATA='age')
d <- rbind(d1, d2)

#display
png("nn.png--------------------------------------------------------", height=800, width=1280)
g <- ggplot(data=d, aes(x, y, geom="line", colour=DATA)) + xlim(0,length(moving)+14) + geom_line()  + xlab('TIME (DAYS)') + ylab('COST') + scale_colour_manual(values=c("black","red")) + theme_bw(base_size=8)
g
dev.off()

#MSE
(1/length(time))*sum((cost-output)^2) #ŽÀ”’l‚ÆÔü‚ÌŒë·

