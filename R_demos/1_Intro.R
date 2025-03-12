
library(testGam)

data("gefcom_small")

head(gefcom_small)

plot(gefcom_small$NetDemand)

train <- gefcom_small[gefcom_small$Year < 2011, ]
test <- gefcom_small[gefcom_small$Year == 2011, ]

library(mgcv)
fit1 <- gam(NetDemand ~ NetDemand.24 + Dow + Trend + wM + wM_s95 + s(Posan, bs = "cc"), data = train)

library(mgcViz)
fit1 <- getViz(fit1, nsim = 100)
plot(fit1)

print(plot(fit1, allTerms = T), pages = 1)

check1D(fit1, "wM") + l_gridCheck1D()

fit2 <- gamV(NetDemand ~ NetDemand.24 + Dow + Trend + s(wM) + s(wM_s95) + s(Posan, bs = "cc"), 
             data = train, aViz = list(nsim = 100))

check1D(fit2, "wM") + l_gridCheck1D()

check(fit2)

fit3 <- gamV(NetDemand ~ NetDemand.24 + Dow + Trend + s(wM, k = 15) + s(wM_s95) + s(Posan, bs = "cc"), 
             data = train, aViz = list(nsim = 100))

AIC(fit1, fit2, fit3)

summary(fit3)

preds <- predict(fit3, newdata = test)

plot(test$NetDemand)
lines(preds, col = 2)

check1D(fit3, "Posan") + l_gridCheck1D(sd)


fit4 <- gamV(list(NetDemand ~ NetDemand.24 + Dow + Trend + s(wM, k = 15) + 
                    s(wM_s95) + s(Posan, bs = "cc"), 
                            ~ s(Posan)),
             data = train, aViz = list(nsim = 100), family = gaulss)

check1D(fit4, "Posan") + l_gridCheck1D(sd)

AIC(fit3, fit4)
