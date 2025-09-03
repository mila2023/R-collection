library(dplyr)
# generując wykresy zmieniamy nazwy plików
burakcukrowy2021 <- read.csv("/Users/majawojtysiak/Documents/Plony/burakcukrowy_2021.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
ziemniaki2021 <- read.csv("/Users/majawojtysiak/Documents/Plony/ziemniaki_2021.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
kukurydzanaziarno2021 <- read.csv("/Users/majawojtysiak/Documents/Plony/kukurydzanaziarno_2021.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
truskawki2021 <- read.csv("/Users/majawojtysiak/Documents/Plony/truskawki_2021.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")

ziemniaki2022 <- read.csv("/Users/majawojtysiak/Documents/Plony/ziemniaki_2022.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
burakcukrowy2022 <- read.csv("/Users/majawojtysiak/Documents/Plony/burakcukrowy_2022.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
kukurydzanaziarno2022 <- read.csv("/Users/majawojtysiak/Documents/Plony/kukurydzanaziarno_2022.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
truskawki2022 <- read.csv("/Users/majawojtysiak/Documents/Plony/truskawki_2022.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")

ziemniaki2023 <- read.csv("/Users/majawojtysiak/Documents/Plony/ziemniaki_2023.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
burakcukrowy2023 <- read.csv("/Users/majawojtysiak/Documents/Plony/burakcukrowy_2023.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
truskawki2023 <- read.csv("/Users/majawojtysiak/Documents/Plony/truskawki_2023.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
kukurydzanaziarno2023 <- read.csv("/Users/majawojtysiak/Documents/Plony/kukurydzanaziarno_2023.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")


nazwy <- burakcukrowy2021[1, ]
burakcukrowy2021 <- burakcukrowy2021[-c(1,2,3,4), ]
colnames(burakcukrowy2021) <- nazwy
nazwy <- burakcukrowy2022[1, ]
burakcukrowy2022 <- burakcukrowy2022[-c(1,2,3,4), ]
colnames(burakcukrowy2022) <- nazwy
nazwy <- burakcukrowy2023[3, ]
burakcukrowy2023 <- burakcukrowy2023[-c(1,2,3,4,5,6), ]
colnames(burakcukrowy2023) <- nazwy


nazwy <- kukurydzanaziarno2021[1, ]
kukurydzanaziarno2021 <- kukurydzanaziarno2021[-c(1,2,3,4), ]
colnames(kukurydzanaziarno2021) <- nazwy
nazwy <- kukurydzanaziarno2022[1, ]
kukurydzanaziarno2022 <- kukurydzanaziarno2022[-c(1,2,3,4), ]
colnames(kukurydzanaziarno2022) <- nazwy
nazwy <- kukurydzanaziarno2023[3, ]
kukurydzanaziarno2023 <- kukurydzanaziarno2023[-c(1,2,3,4,5,6), ]
colnames(kukurydzanaziarno2023) <- nazwy


nazwy <- truskawki2021[1, ]
truskawki2021 <- truskawki2021[-c(1,2,3,4), ]
colnames(truskawki2021) <- nazwy
nazwy <- truskawki2022[1, ]
truskawki2022 <- truskawki2022[-c(1,2,3,4), ]
colnames(truskawki2022) <- nazwy
nazwy <- truskawki2023[3, ]
truskawki2023 <- truskawki2023[-c(1,2,3,4,5,6), ]
colnames(truskawki2023) <- nazwy


nazwy <- ziemniaki2021[1, ]
ziemniaki2021 <- ziemniaki2021[-c(1,2,3,4), ]
colnames(ziemniaki2021) <- nazwy
nazwy <- ziemniaki2022[1, ]
ziemniaki2022 <- ziemniaki2022[-c(1,2,3,4), ]
colnames(ziemniaki2022) <- nazwy
nazwy <- ziemniaki2023[3, ]
ziemniaki2023 <- ziemniaki2023[-c(1,2,3,4,5,6), ]
colnames(ziemniaki2023) <- nazwy

#funckja do generwoania wykresu da lubuskiego
jpeg("/Users/majawojtysiak/Documents/Plony/plony.jpeg", width = 1000)
funkcja_1 <- function(x, y, z, r, c) {
  dwudziestypierwszy <- x[r, c]
  dwudziestydrugi <- y[r, c]
  dwudziestytrzeci <- z[r, c]
  vector <- c(dwudziestypierwszy, dwudziestydrugi, dwudziestytrzeci)
  vector <- as.numeric(gsub(",", ".", as.vector(vector)))
  vector 
}

c_1 <- funkcja_1(truskawki2021, truskawki2022, truskawki2023, 3, 3)
c_2 <- funkcja_1(ziemniaki2021, ziemniaki2022, ziemniaki2023, 3, 3)
c_3 <- funkcja_1(kukurydzanaziarno2021, kukurydzanaziarno2022, kukurydzanaziarno2023, 3, 3)
counts <- matrix(cbind(c_1, c_2, c_3), 3, 3)

par(mar=c(5,7,5,5))
counts
barplot(counts,
        main="PLONY W WOJEWÓDZTWIE LUBELSKIM",
        xlab="Roślina", ylab = "Plony (dekatona/hektar)", cex.main=1.5,
        cex.names = 1.5, cex.axis= 1.3, cex.lab=1.3, angle=30,
        col=c("#000000","#B6E4B3","#41B7C4"), 
        legend = c("2021", "2022", "2023"), beside=TRUE, ylim=c(0, 350), density = c(50))
axis(1, at = seq(2.5, 10.5, by=4), tick = FALSE, labels = c("Truskawki", "Ziemniaki","Kukurydza"))
box("plot")
dev.off() 

# funkcja do generowania wykresów 2 - 4 (dla poszczególnych roślin)

plony <- function(x, y, z) {
c_1 <- funkcja_1(x, y, z, 1, 3)
c_2 <- funkcja_1(x, y, z, 3, 3)
c_3 <- funkcja_1(x, y, z, 4, 3)
c_4 <- funkcja_1(x, y, z, 8, 3)
c_5 <- funkcja_1(x, y, z, 14, 3)
c_6 <- funkcja_1(x, y, z, 16, 3)
counts <- matrix(cbind(c_1, c_2, c_3, c_4, c_5, c_6), 3, 6)
counts
}
truskawki <- plony(truskawki2021, truskawki2022, truskawki2023)
ziemniaki <- plony(ziemniaki2021, ziemniaki2022, ziemniaki2023)
kukurydza <- plony(kukurydzanaziarno2021, kukurydzanaziarno2022, kukurydzanaziarno2023)
                   
jpeg("/Users/majawojtysiak/Documents/Plony/truskawki_porownanie.jpeg", width = 1000)
par(mar=c(5,7,5,5))

barplot(truskawki,
        main="PLONY TRUSKAWEK W WYBRANYCH WOJEWÓDZTWACH",
        ylab = "Plony (dekatona/hektar)", cex.main=1.5,
        cex.names = 1.5, cex.axis= 1.3, cex.lab=1.3, angle=30,
        col=c("#de2d26","#fee0d2", "#fc9272"),
        legend = c("2021", "2022","2023"), beside=TRUE, ylim=c(0, 100), density = c(25, 25, 25))
axis(1, at = seq(2.5, 22.5, by=4), tick = FALSE, labels = c("Dolnośląskie", "Lubelskie", "Lubuskie", "Opolskie", "Warmińsko-mazurskie", "Zachodniopomorskie"))
box("plot")
dev.off() 
jpeg("/Users/majawojtysiak/Documents/Plony/ziemniaki_porownanie.jpeg", width = 1000)
par(mar=c(5,7,5,5))

barplot(ziemniaki,
        main="PLONY ZIEMNIAKÓW W WYBRANYCH WOJEWÓDZTWACH",
        ylab = "Plony (dekatona/hektar)", cex.main=1.5,
        cex.names = 1.5, cex.axis= 1.3, cex.lab=1.3, angle=30,
        col=c("#fec44f","#fff7bc","#d95f0e"),
        legend = c("2021", "2022","2023"), beside=TRUE, ylim=c(0, 400), density = c(25, 25, 25))
axis(1, at = seq(2.5, 22.5, by=4), tick = FALSE, labels = c("Dolnośląskie", "Lubelskie", "Lubuskie", "Opolskie", "Warmińsko-mazurskie", "Zachodniopomorskie"))
box("plot")
dev.off() 
jpeg("/Users/majawojtysiak/Documents/Plony/kukurydza_porownanie.jpeg", width = 1000)
par(mar=c(5,7,5,5))

barplot(kukurydza,
        main="PLONY KUKURYDZY W WYBRANYCH WOJEWÓDZTWACH",
        ylab = "Plony (dekatona/hektar)", cex.main=1.5,
        cex.names = 1.5, cex.axis= 1.3, cex.lab=1.3, angle=30,
        col=c("#addd8e","#f7fcb9","#31a354"),
        legend = c("2021", "2022","2023"), beside=TRUE, ylim=c(0, 100), density = c(25, 25, 25))
axis(1, at = seq(2.5, 22.5, by=4), tick = FALSE, labels = c("Dolnośląskie", "Lubelskie", "Lubuskie", "Opolskie", "Warmińsko-mazurskie", "Zachodniopomorskie"))
box("plot")
dev.off() 

#generowanie ostatniego wykresu z rankingiem
c_1 <- as.vector(truskawki2023[, 1])
c_2 <- as.numeric(gsub(",", ".", as.vector(kukurydzanaziarno2023[, 3])))
plony2023 <- as.data.frame(c_2)
rownames(plony2023)<- c_1
plony2023 <- arrange(plony2023, desc(c_2))
vector <- as.vector(pull(plony2023, c_2))
vector2 <- rownames(plony2023)

jpeg("/Users/majawojtysiak/Documents/Plony/ranking.jpeg", width = 1000)
par(mar=c(12,9,7,7))
barplot(vector,
main="RANKING URODZAJNOŚCI W ZIEMNIAKI",
ylab = "Plony (dekatona/hektar)", cex.main=1.5,
cex.names = 1.5, cex.axis= 1.3, cex.lab=1.3, angle=30,
col=c("#fec44f"),
ylim=c(0, 100), density = c(10))
axis(1, at = seq(1, 18.4, by=1.15), las = 2, tick = FALSE, labels = vector2)
box("plot")
dev.off() 
















