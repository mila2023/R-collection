library(openmeteo)
library(data.table)
jpeg("C:/Users/kamil/RProjects/PD2/Pogoda/Truskawki_OpolskieWarminsko_Temp.jpeg", width = 1000)

miasto1 = "Opole"
wojj1 = "Opolskie"
miasto2 = "Olsztyn"
wojj2 = "Warmińsko-mazurskie"

################################################################################
woj1 <- weather_history(location = miasto1, start = "2021-01-01", end = "2023-12-31",
                         dail = c("temperature_2m_max", "temperature_2m_min", "rain_sum", "sunshine_duration"))

woj1$month <- format(as.Date(woj1$date),"%Y-%m")

woj11 <- aggregate(woj1$daily_temperature_2m_max, list(woj1$month), max)
colnames(woj11) <- c("Date", "maks")
woj11[,2] <- as.numeric(woj11[,2])
woj11$ID <- seq.int(nrow(woj11))

woj12 <- aggregate(woj1$daily_temperature_2m_min, list(woj1$month), min)
colnames(woj12) <- c("Date", "min")
woj12[,2] <- as.numeric(woj12[,2])
woj12$ID <- seq.int(nrow(woj12))

################################################################################

woj2 <- weather_history(location = miasto2, start = "2021-01-01", end = "2023-12-31",
                        dail = c("temperature_2m_max", "temperature_2m_min", "rain_sum", "sunshine_duration"))

woj2$month <- format(as.Date(woj2$date),"%Y-%m")

woj21 <- aggregate(woj2$daily_temperature_2m_max, list(woj2$month), max)
colnames(woj21) <- c("Date", "maks")
woj21[,2] <- as.numeric(woj21[,2])
woj21$ID <- seq.int(nrow(woj21))

woj22 <- aggregate(woj2$daily_temperature_2m_min, list(woj2$month), min)
colnames(woj22) <- c("Date", "min")
woj22[,2] <- as.numeric(woj22[,2])
woj22$ID <- seq.int(nrow(woj22))

################################################################################

plot.new()
par(mar=c(5,5,5,5))
plot.window(c(1,36),c(-25,35))

palette1 <- c("darkmagenta","deepskyblue4","deeppink","darkturquoise")

points(woj11$ID, woj11$maks, pch=16, col=palette1[1])
points(woj12$ID, woj12$min, pch=16, col=palette1[2])
points(woj21$ID, woj21$maks, pch=17, col=palette1[3])
points(woj22$ID, woj22$min, pch=17, col=palette1[4])

lines(woj11$ID, woj11$maks, col=palette1[1])
lines(woj12$ID, woj12$min, col=palette1[2])
lines(woj21$ID, woj21$maks, col=palette1[3])
lines(woj22$ID, woj22$min, col=palette1[4])

axis(1,at=c(1:36) ,cex.axis = 1.0, las = 2, labels = c("2021-01", "2021-02", "2021-03", "2021-04", "2021-05", "2021-06", "2021-07", "2021-08", 
      "2021-09", "2021-10", "2021-11", "2021-12", "2022-01", "2022-02", "2022-03", "2022-04", "2022-05", "2022-06", "2022-07", "2022-08",
      "2022-09", "2022-10", "2022-11", "2022-12", "2023-01", "2023-02", "2023-03", "2023-04", "2023-05", "2023-06", "2023-07", "2023-08",
      "2023-09", "2023-10", "2023-11", "2023-12"))

axis(2,at=c(-20:35),las = 1, lty=1, cex.axis=1.3)
title(main = c(wojj1,wojj2,"Temperatury maksymalne i minimalne na przestrzeni miesięcy"), ylab="Temperatura", 
      cex.main = 1.5, cex.lab= 1.5)

legend(27.5,-7,cex = 1.2 ,col = palette1[1:4], pch = c(16,16,17,17),
       legend = c("max temp opolskie", "min temp opolskie", "max temp warm.", "min temp warm."))
arrows(x0 = -1, y0 = 0, x1 = 55, y1 = 0)
grid(36, 27, col = "lightgray")
box("plot")
dev.off()

