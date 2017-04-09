library(tseries)
library(quantmod)
# library(rusquant)

from <- "2016-01-01"
to <- Sys.Date()
tickerArray <- c("GLD", "GDX")
getSymbols(tickerArray, from = from, to = to)

data <- cbind(Cl(GLD), GDX$GDX.Adjusted)
data <- data[complete.cases(data)]
names(data) <- c("GLD", "GDX")
# print(head(data))

model <- lm(GLD ~ GDX+0, data)

print(summary(model))

# Draw charts
layout(matrix(1:2, ncol=1))
candleChart(c(GLD, GDX), multi.col = TRUE, theme = 'white', layout=NULL, TA=NULL)
plot(GLD ~ GDX, data)

abline(model)
#dev.new()

# spread <- data$GLD - coef(model)[1] * data$GDX
# 
# plot(spread, type = "l")
# 
# print(adf.test(as.vector(spread), k = 0))
# 
# plot(as.vector(Cl(GLD)), col = 'blue', type = 'l', main = 'Cointegration GLD & GDX', ylab = 'price')
# lines(as.vector(GDX$GDX.Adjusted * coef(model)[1]), col = 'red', type = 'l')
# 
# coint.res <- residuals(lm(GLD ~ GDX, data))
# library("strucchange")
# breakpoints(coint.res ~ 1)
