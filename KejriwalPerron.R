library("strucchange")

## Nile data with one breakpoint: the annual flows drop in 1898
## because the first Ashwan dam was built
data(Nile)
## F statistics indicate one breakpoint
fs.nile <- Fstats(Nile ~ 1)
bp.fsnile <- breakpoints(fs.nile)
bp.nile <- breakpoints(Nile ~ 1)

summary(bp.fsnile)
summary(bp.nile)
breapoints(bp.nile)

### fit null hypothesis model and model with 1 breakpoint
fm0 <- lm(Nile ~ 1)
fm1 <- lm(Nile ~ breakfactor(bp.nile, breaks = 1))

### confidence interval
ci.nile <- confint(bp.nile)
ci.nile

# Draw Nile, F statistics and breakpoints for Nile
par(mfrow = c(4, 1))
par(mar = c(0.5, 4.5, 0.5, 0.5))
plot(Nile)
plot(fs.nile)
lines(bp.fsnile)
plot(bp.nile)
plot(Nile)
lines(ts(fitted(fm0), start = 1871), col = "green")
lines(ts(fitted(fm1), start = 1871), col = "blue")
lines(bp.nile)
lines(ci.nile)

### UK Seatbelt data: a SARIMA(1,0,0)(1,0,0)_12 model
### (fitted by OLS) is used and reveals (at least) two
### breakpoints - one in 1973 associated with the oil crisis and
### one in 1983 due to the introduction of compulsory
### wearing of seatbelts in the UK.
data("UKDriverDeaths")
seatbelt <- log10(UKDriverDeaths)
seatbelt <- cbind(seatbelt, lag(seatbelt, k = -1), lag(seatbelt, k = -12))
colnames(seatbelt) <- c("y", "ylag1", "ylag12")
seatbelt <- window(seatbelt, start = c(1970, 1), end = c(1984, 12))
plot(seatbelt[, "y"], ylab = expression(log[10](casualties)))
### testing
re.seat <- efp(y ~ ylag1 + ylag12, data = seatbelt, type = "RE")
plot(re.seat)
### dating
bp.seat <- breakpoints(y ~ ylag1 + ylag12, data = seatbelt, h = 0.1)
#catL2BB 15
summary(bp.seat)
lines(bp.seat, breaks = 2)
### minimum BIC partition
plot(bp.seat)
breakpoints(bp.seat)
### the BIC would choose 0 breakpoints although the RE and supF test
### clearly reject the hypothesis of structural stability. Bai &
### Perron (2003) report that the BIC has problems in dynamic regressions.
### due to the shape of the RE process of the F statistics choose two
### breakpoints and fit corresponding models
bp.seat2 <- breakpoints(bp.seat, breaks = 2)
fm0 <- lm(y ~ ylag1 + ylag12, data = seatbelt)
fm1 <- lm(y ~ breakfactor(bp.seat2) / (ylag1 + ylag12) - 1, data = seatbelt)
### plot
plot(seatbelt[, "y"], ylab = expression(log[10](casualties)))
time.seat <- as.vector(time(seatbelt))
lines(time.seat, fitted(fm0), col = 3)
lines(time.seat, fitted(fm1), col = 4)
lines(bp.seat2)
### confidence intervals
ci.seat2 <- confint(bp.seat, breaks = 2)
ci.seat2
lines(ci.seat2)