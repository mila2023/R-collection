library(openmeteo)
library(data.table)
jpeg("C:/Users/kamil/RProjects/PD2/Pogoda/ziemniaki_Lubuskie.jpeg", width = 1000)

miasto1 = "Gorzów Wielkopolski"
wojj1 = "Lubuskie"

################################################################################
woj11 <- weather_history(location = miasto1, start = "2021-01-01", end = "2021-12-31",
                        dail = c("temperature_2m_max", "temperature_2m_min", "rain_sum", "sunshine_duration"))

woj12 <- weather_history(location = miasto1, start = "2022-01-01", end = "2022-12-31",
                        dail = c("temperature_2m_max", "temperature_2m_min", "rain_sum", "sunshine_duration"))

woj13 <- weather_history(location = miasto1, start = "2023-01-01", end = "2023-12-31",
                        dail = c("temperature_2m_max", "temperature_2m_min", "rain_sum", "sunshine_duration"))



woj11$month <- format(as.Date(woj11$date),"%m")
woj12$month <- format(as.Date(woj12$date),"%m")
woj13$month <- format(as.Date(woj13$date),"%m")

woj11 <- aggregate(woj11$daily_rain_sum, list(woj11$month), sum)
colnames(woj11) <- c("month", "rain")
woj11[,2] <- as.numeric(woj11[,2])
woj11[,1] <- as.numeric(woj11[,1])

woj12 <- aggregate(woj12$daily_rain_sum, list(woj12$month), sum)
colnames(woj12) <- c("month", "rain")
woj12[,2] <- as.numeric(woj12[,2])
woj12[,1] <- as.numeric(woj12[,1])

woj13 <- aggregate(woj13$daily_rain_sum, list(woj13$month), sum)
colnames(woj13) <- c("month", "rain")
woj13[,2] <- as.numeric(woj13[,2])
woj13[,1] <- as.numeric(woj13[,1])


plot.new()
par(mar=c(5,5,5,5))
plot.window(c(1,12),c(0,150))

palette1 <- c("#FF9A00","#9E783F","#4B2020")

points(woj11$month, woj11$rain, pch=15, col=palette1[1])
points(woj12$month, woj12$rain, pch=16, col=palette1[2])
points(woj13$month, woj13$rain, pch=17, col=palette1[3])
# points(woj22$month, woj22$rain, pch=17, col=palette1[4])

lines(woj11$month, woj11$rain, col=palette1[1])
lines(woj12$month, woj12$rain, col=palette1[2])
lines(woj13$month, woj13$rain, col=palette1[3])
# lines(woj22$month, woj22$rain, col=palette1[4])

axis(1, at=c(1:12) ,cex.axis = 1.0, las = 2, labels = c("Styczeń","Luty","Marzec","Kwiecień","Maj","Czerwiec","Lipiec","Sierpień","Wrzesień","Październik","Listopad","Grudzień"))

axis(2,at = seq(0, 150, by = 20),las = 1, cex.axis=1.3)
title(main = c(wojj1,"Opady na przestrzeni miesięcy"), ylab="Opady (mm)", 
      cex.main = 1.5, cex.lab= 1.5)

legend(10,150,cex = 1.2 ,col = palette1[1:3], pch = c(15,16,17),
       legend = c("opady rok 2021", "opady rok 2022", "opady rok 2023"))
arrows(x0 = 3.5, y0 = -3, x1 = 3.5, y1 = 180, col = "darkgray", lty = "dashed")
arrows(x0 = 9.5, y0 = -3, x1 = 9.5, y1 = 180, col = "darkgray", lty = "dashed")
grid(36, 27, col = "lightgray")
box("plot")
dev.off()