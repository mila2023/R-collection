library(openmeteo)
library(data.table)
jpeg("C:/Users/kamil/RProjects/PD2/Pogoda/lubel_SUN_SUM.jpeg", width = 1000)

miasto = "Lublin"
woj = "Lubelskie"

woj21 <- weather_history(location = miasto,
                         start = "2021-01-01",
                         end = "2021-12-31",
                         dail = c("temperature_2m_max", "temperature_2m_min",
                                  "rain_sum", "sunshine_duration"))

woj22 <- weather_history(location = miasto,
                         start = "2022-01-01",
                         end = "2022-12-31",
                         dail = c("temperature_2m_max", "temperature_2m_min",
                                  "rain_sum", "sunshine_duration"))

woj23 <- weather_history(location = miasto,
                         start = "2023-01-01",
                         end = "2023-12-31",
                         dail = c("temperature_2m_max", "temperature_2m_min",
                                  "rain_sum", "sunshine_duration"))

woj21$month <- format(as.Date(woj21$date),"%m")
woj22$month <- format(as.Date(woj22$date),"%m")
woj23$month <- format(as.Date(woj23$date),"%m")

srtemp21 <- aggregate(woj21$daily_sunshine_duration, list(woj21$month), sum )
srtemp21 <- as.double(srtemp21$x)
srtemp21 <- srtemp21/3600

srtemp22 <- aggregate(woj22$daily_sunshine_duration, list(woj22$month), sum )
srtemp22 <- as.double(srtemp22$x)
srtemp22 <- srtemp22/3600

srtemp23 <- aggregate(woj23$daily_sunshine_duration, list(woj23$month), sum )
srtemp23 <- as.double(srtemp23$x)
srtemp23 <- srtemp23/3600

counts <- matrix(rbind(srtemp21, srtemp22, srtemp23), 3, 12)
par(mar=c(5,7,5,5))
barplot(counts,
        main= c(woj,"Suma godzin nasłonecznienia w zależności od miesiąca"),
        xlab="Miesiąc",ylab = "Suma nasłonecznienia (h)",cex.main=1.5,
        cex.names = 1.5, las = 2, cex.axis= 1.3,cex.lab=1.3, angle=30,
        col=c("honeydew","turquoise","#086E6A"),
        legend = rownames(counts), beside=TRUE, ylim=c(0,450), density = c(5,10,15))
axis(1, at=seq(2.5,46.5,by=4), tick=FALSE, cex.axis = 0.9, labels = c("Styczeń","Luty","Marzec","Kwiecień","Maj","Czerwiec","Lipiec","Sierpień","Wrzesień","Październik","Listopad","Grudzień"))
box("plot")
legend(43.8,450,cex = 1.2 ,fill = c("honeydew","turquoise","#086E6A"),density = c(5,10,15),legend = c("rok 2021","rok 2022","rok 2023"))
dev.off()
