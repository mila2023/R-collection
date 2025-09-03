library(dplyr)
jpeg("C:/Users/kamil/RProjects/PD2/plony/plony_trus_trans.jpeg", width = 1000)
burakcukrowy2021 <- read.csv("C:/Users/kamil/RProjects/PD2/plony/burakcukrowy_2021.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
ziemniaki2021 <- read.csv("C:/Users/kamil/RProjects/PD2/plony/ziemniaki_2021.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
kukurydzanaziarno2021 <- read.csv("C:/Users/kamil/RProjects/PD2/plony/kukurydzanaziarno_2021.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
truskawki2021 <- read.csv("C:/Users/kamil/RProjects/PD2/plony/truskawki_2021.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")

ziemniaki2022 <- read.csv("C:/Users/kamil/RProjects/PD2/plony/ziemniaki_2022.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
burakcukrowy2022 <- read.csv("C:/Users/kamil/RProjects/PD2/plony/burakcukrowy_2022.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
kukurydzanaziarno2022 <- read.csv("C:/Users/kamil/RProjects/PD2/plony/kukurydzanaziarno_2022.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
truskawki2022 <- read.csv("C:/Users/kamil/RProjects/PD2/plony/truskawki_2022.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")

ziemniaki2023 <- read.csv("C:/Users/kamil/RProjects/PD2/plony/ziemniaki_2023.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
burakcukrowy2023 <- read.csv("C:/Users/kamil/RProjects/PD2/plony/burakcukrowy_2023.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
truskawki2023 <- read.csv("C:/Users/kamil/RProjects/PD2/plony/truskawki_2023.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
kukurydzanaziarno2023 <- read.csv("C:/Users/kamil/RProjects/PD2/plony/kukurydzanaziarno_2023.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")



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




wojewodztwa <- truskawki2021[1]
dwudziestypierwszy <- truskawki2021[3]
dwudziestydrugi <- truskawki2022[3]
dwudziestytrzei <- truskawki2023[3]

# wojewodztwa <- ziemniaki2021[1]
# dwudziestypierwszy <- ziemniaki2021[3]
# dwudziestydrugi <- ziemniaki2022[3]
# dwudziestytrzei <- ziemniaki2023[3]

a <- as.numeric(gsub(",", ".", as.vector(pull(dwudziestypierwszy))))
b <- as.numeric(gsub(",", ".", as.vector(pull(dwudziestydrugi))))
c <- as.numeric(gsub(",", ".", as.vector(pull(dwudziestytrzei))))

counts <- matrix(rbind(a, b, c), 3, 16)
par(mar=c(5,7,5,5))
counts
barplot(counts,
        main="Plony truskawek na przestrzeni lat w różnych województwach",
        xlab="Województwo",ylab = "Plony truskawek (dekatona/hektar)",cex.main=1.5,
        cex.names = 1.5, las = 2, cex.axis= 1.3,cex.lab=1.3,angle=30,
        col = c("#000000","#B6E4B3","#41B7C4"),
        legend=rownames(counts), beside=TRUE, ylim=c(0,100), density = c(5,10,15))
axis(1, at=seq(2.5,124.5,by=8), tick=FALSE, cex.axis = 0.9, labels = c("Dolnośląs.", "Kujaw.-pom.", "Lubelskie", "Lubuskie", "Łódzkie", "Małopolskie", "Mazow.", "Opolskie", "Podkarp.", "Podlaskie", "Pomorskie", "Śląskie", "Świętok.", "Warmiń.-mazur.", "Wielkopol.", "Zachodniopom."))
box("plot")
legend(60.3, 100, cex = 1.2 ,fill = c("#000000","#B6E4B3","#41B7C4"),density = c(5,10,15),legend = c("2021", "2022", "2023"))
dev.off() 




