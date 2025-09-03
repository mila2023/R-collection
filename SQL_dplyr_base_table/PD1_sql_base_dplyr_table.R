### Przetwarzanie Danych Ustrukturyzowanych 2024L
### Praca domowa nr. 1
###
### UWAGA:
### nazwy funkcji oraz ich parametrow powinny pozostac niezmienione.
###  
### Wskazane fragmenty kodu przed wyslaniem rozwiazania powinny zostac 
### zakomentowane
###

# -----------------------------------------------------------------------------#
# library(sqldf)
# library(dplyr)
# library(data.table)
# library(compare)
# library(microbenchmark)
# Posts <- read.csv("Posts.csv.gz")
# Users <- read.csv("Users.csv.gz")
# Votes <- read.csv("Votes.csv.gz")
# PostLinks <- read.csv("PostLinks.csv.gz")
# Comments <- read.csv("Comments.csv.gz")
# -----------------------------------------------------------------------------#



# -----------------------------------------------------------------------------#
# Zadanie 1
# -----------------------------------------------------------------------------#

sql_1 <- function(Users){
  sqldf("
    SELECT STRFTIME('%Y',CreationDate) AS Year,
    STRFTIME('%m',CreationDate) AS Month,
    COUNT(*) AS TotalAccountsCount,
    AVG(Reputation) AS AverageReputation
    FROM Users
    GROUP BY Year,Month
  ") 
}

base_1 <- function(Users){
  
  year <- format(as.Date(Users$"CreationDate"),"%Y") # stworzenie char z latami
  month <- format(as.Date(Users$"CreationDate"), "%m") # stworzenie char z miesiacami
  countid <- aggregate(Users$Id, list(year, month), length) # stworzenie data.frame zawierającej Year, Month oraz liczbe Id z Users przypisanych do tych danych
  meanrep <- aggregate(Users$Reputation, list(year, month), mean) # stworzenie data.frame zawierającej Year, Month oraz srednia z Reputation z Users przypisanych do tych danych
  Answer <- data.frame(Year = countid$Group.1, Month = countid$Group.2,
            TotalAccountsCount = countid$x, AverageReputation = meanrep$x) # stworzenie data frame Answer przechowujacej dane z powyzszych dwoch data.frame
  Answer <- Answer[order(Answer$Year, Answer$Month),] # sortowanie Answer po latach i miesiacach rosnaco
  row.names(Answer) <- NULL # uregulowanie nazw wierszy
  Answer # wypisanie wyniku
}

dplyr_1 <- function(Users){
  
  Answer <- Users%>% # przypisanie do Answer data frame Users
    mutate(Year = format(as.Date(CreationDate), "%Y"))%>% # dodanie do Answer kolumny Year, bedacej mutacja kolumny CreationDate
    mutate(Month = format(as.Date(CreationDate), "%m"))%>% # to samo co wyzej dla Month
    select(Year, Month, Id, Reputation)%>% # wybranie aby zachowac jedynie kolumny wymienione po przecinku w data frame Answer
    group_by(Year, Month)%>% # grupowanie data frame Answer po Year, Month
    summarise(TotalAccountsCount = length(Id), AverageReputation = mean(Reputation))%>% # podsumowanie danych z Answers, pojawia sie zliczenie duplikowanych Id i srednia z Reputation
    as.data.frame # zamiana wyniku na data.frame
  Answer # wypisanie wyniku
}

table_1 <- function(Users){
    Users <- as.data.table(Users) # zrobienie z Users data table
    Users[, Year:=format(as.Date(CreationDate), "%Y")] # dodanie kolumny Year formatujacej dane w Creation Date
    Users[, Month:=format(as.Date(CreationDate), "%m")] # to samo dla Month
    Answer <- Users[, .(TotalAccountsCount = .N, AverageReputation = mean(Reputation)), by= .(Year,Month)] # podsumowanie danych grupujac po Year, Month zliczajac wystapienia 
                                                                                                           # wierszy oraz srednie z Reputation
    Answer <- as.data.frame(Answer) # zamiana wyniku na data.frame
    Answer  # wypisanie wyniku
}


# compare(sql_1(Users), base_1(Users), allowAll = TRUE)
# compare(sql_1(Users), dplyr_1(Users), allowAll = TRUE)
# compare(sql_1(Users), table_1(Users), allowAll = TRUE)

# suppressMessages(
#   microbenchmark(
#     sqldf = sql_1(Users),
#     base = base_1(Users),
#     dplyr = dplyr_1(Users),
#     data.table = table_1(Users))
# )

# -----------------------------------------------------------------------------#
# Zadanie 2
# -----------------------------------------------------------------------------#

sql_2 <- function(Posts, Users){
  sqldf("
    SELECT Users.DisplayName, Users.Location, Users.Reputation,
      STRFTIME('%Y-%m-%d', Users.CreationDate) AS CreationDate,
      Answers.TotalCommentCount
    FROM( SELECT OwnerUserId, SUM(CommentCount) AS TotalCommentCount
      FROM Posts 
      WHERE PostTypeId == 2 AND OwnerUserId != ''
      GROUP BY OwnerUserId) AS Answers JOIN Users ON
      Users.Id == Answers.OwnerUserId
    ORDER BY TotalCommentCount DESC
    LIMIT 10
  ")
}

base_2 <- function(Posts, Users){
  
  Answers <- Posts[Posts$PostTypeId == 2 & Posts$OwnerUserId != "", c("OwnerUserId", "CommentCount")] # wyciagniecie z Posts wierszy gdzie w kolumnie PostTypeId wystepuje 2
                                                                                                      # i OwnerUserId przyjmuje wartosc, wybranie kolumn OwnerUserId i CommentCount
  Answers <- aggregate(Answers$CommentCount, by = list(Answers$OwnerUserId), FUN = sum) # podsumowanie danych po OwnerUserId, sumujac wartosci CommentCount im odpowiadajace
  colnames(Answers) <- c("OwnerUserId", "TotalCommentCount") # zmiana nazwy kolumn Answers
  Temp <- merge.data.frame(Answers, Users, by.x = "OwnerUserId", by.y = "Id", all.x = FALSE, all.y = FALSE) # laczenie Answers z Users w Temp
  Temp <- Temp[, c("DisplayName", "Location", "Reputation", "CreationDate", "TotalCommentCount")] # wybranie z Temp odpowiednich kolumn
  Temp$CreationDate <- as.Date(Temp$CreationDate, "%Y-%m-%d") # modyfikacja kolumny CreationDate, by przechowywala tylko rok-miesiac-dzien
  Temp <- Temp[order(-Temp$TotalCommentCount),] # sortowanie danych po TotalCommentCount malejaco
  rownames(Temp) <- NULL # wyregulowanie nazw wierszy
  Temp <- Temp[1:10,] # wybranie 10 gornych wierszy
  Temp # wypisanie wyniku
}

dplyr_2 <- function(Posts, Users){
  
  Answer <- Posts%>% # przypisanie do Answer data frame Posts
    filter(PostTypeId == 2, OwnerUserId != '')%>% # wyciagniecie z Answer wierszy gdzie w kolumnie PostTypeId wystepuje 2 i OwnerUserId przyjmuje wartosc
    group_by(OwnerUserId)%>% # grupowanie Answer po OwnerUserId
    summarise(TotalCommentCount = sum(CommentCount))%>% # podsumowanie po kolumnie przechowujacej wiersze bedace sumami wierszy CommentCount przypisanych do jednego OwnerUserId
    inner_join(Users, by = c("OwnerUserId" = "Id"))%>% # laczenie Answers z Users
    mutate(CreationDate = format(as.Date(CreationDate), "%Y-%m-%d"))%>% # modyfikacja kolumny CreationDate, by przechowywala tylko rok-miesiac-dzien
    arrange(desc(TotalCommentCount))%>% # sortowanie danych po TotalCommentCount malejaco
    select(DisplayName, Location, Reputation, CreationDate, TotalCommentCount)%>% # wybranie z Answer odpowiednich kolumn
    top_n(10)%>% # wybranie 10 gornych wierszy
    as.data.frame #zapisanie wyniku jako data.frame
  Answer # wypisanie wyniku
}

table_2 <- function(Posts, Users){
  setDT(Posts) # zamiana Posts w data.table
  setDT(Users) # zamiana Users w data.table
  Answers <- Posts[PostTypeId == 2 & OwnerUserId != "", ] # wyciagniecie z Answer wierszy gdzie w kolumnie PostTypeId wystepuje 2 i OwnerUserId przyjmuje wartosc
  Answers <- Answers[, .(TotalCommentCount = sum(CommentCount)), by = .(OwnerUserId)] # grupowanie Answer po OwnerUserId podsumowywujac CommentCount jako suma wierszy przypisanych do jednego OwnerUserId
  AnUs <- merge(Answers, Users, by.x = "OwnerUserId", by.y = "Id", all = FALSE) # laczenie Answers z Users
  AnUs[, Date:=format(as.Date(CreationDate), "%Y-%m-%d")] # stworzenie nowej kolumny Date, przechowujacej tylko rok-miesiac-dzien
  AnUs <- AnUs[, .(DisplayName, Location, Reputation, Date, TotalCommentCount)] # wybranie z Answer odpowiednich kolumn
  setnames(AnUs, "Date", "CreationDate") # zmiana nazwy kolumny
  setorder(AnUs, -TotalCommentCount) # sortowanie danych po TotalCommentCount malejaco
  AnUs <- head(AnUs, 10) # wybranie 10 gornych wierszy
  AnUs <- as.data.frame(AnUs) # zamiana wyniku na data.frame
  AnUs # wypisanie wyniku
}


# compare(sql_2(Posts, Users), base_2(Posts, Users), allowAll = TRUE)
# compare(sql_2(Posts, Users), dplyr_2(Posts, Users), allowAll = TRUE)
# compare(sql_2(Posts, Users), table_2(Posts, Users), allowAll = TRUE)

# suppressMessages(
#   microbenchmark(
#     sqldf = sql_2(Posts, Users),
#     base = base_2(Posts, Users),
#     dplyr = dplyr_2(Posts, Users),
#     data.table = table_2(Posts, Users))
# )

# -----------------------------------------------------------------------------#
# Zadanie 3
# -----------------------------------------------------------------------------#

sql_3 <- function(Posts, Users, Votes){
  sqldf("
    SELECT Spam.PostId, UsersPosts.PostTypeId, UsersPosts.Score,
      UsersPosts.OwnerUserId, UsersPosts.DisplayName,
      UsersPosts.Reputation
    FROM(
      SELECT PostId
      FROM Votes
      WHERE VoteTypeId == 12
        ) AS Spam JOIN (
      SELECT Posts.Id, Posts.OwnerUserId, Users.DisplayName,
        Users.Reputation, Posts.PostTypeId, Posts.Score
      FROM Posts JOIN Users ON Posts.OwnerUserId = Users.Id
        ) AS UsersPosts ON Spam.PostId = UsersPosts.Id
  ")
}

base_3 <- function(Posts, Users, Votes){
  Spam <- data.frame(Votes[Votes$VoteTypeId == 12, "PostId"]) # wyciagniecie wierszy z Votes ktore dla VoteTypeId maja 12, wybranie tylko kolumny PostId jako Spam
  colnames(Spam) <- "PostId" # zmiana nazwy kolumny
  UsersPosts <- merge.data.frame(Posts, Users, by.x = "OwnerUserId", by.y = "Id") # polaczenie Posts i Users w UsersPosts
  UsersPosts <- UsersPosts[, c("Id", "OwnerUserId", "DisplayName", "Reputation", "PostTypeId", "Score")] # wybranie odpowiednich kolumn
  Answer <- merge.data.frame(Spam, UsersPosts, by.x = "PostId", by.y = "Id") # polacznie Spam i UsersPosts w Answer
  Answer <- Answer[,c("PostId", "PostTypeId", "Score", "OwnerUserId", "DisplayName", "Reputation")] # wybranie odpowiedznich kolumn
  Answer # wypisanie wyniku
}

dplyr_3 <- function(Posts, Users, Votes){
  
  Answer <- Posts%>% # przypisanie wartosci Posts do Answer
    inner_join(Users, by = c("OwnerUserId" = "Id"))%>% # polaczenie Answer z Users
    select(Id, OwnerUserId, DisplayName, Reputation, PostTypeId, Score)%>% # wybranie odpowiedznich kolumn
    inner_join(Votes, by = c("Id" = "PostId"))%>% # polaczenie Answer z Votes
    filter(VoteTypeId == 12)%>% # wyciagniecie wierszy z Answer ktore dla VoteTypeId maja 12
    select(Id, PostTypeId, Score, OwnerUserId, DisplayName, Reputation)%>% # wybranie odpowiedznich kolumn
    as.data.frame # zamiania wyniku na data frame
  Answer # wypisanie wyniku
}

table_3 <- function(Posts, Users, Votes){
  setDT(Posts) # zamiana Posts na data.table
  setDT(Users) # zamiana Users na data.table
  setDT(Votes) # zamiana Votes na data.table
  Spam <- as.data.table(Votes[VoteTypeId == 12, PostId]) # wyciagniecie wierszy z Votes ktore dla VoteTypeId maja 12, wybranie tylko kolumny PostId jako Spam
  setnames(Spam, "V1", "PostId") # zmiana nazwy kolumny
  UsersPosts <- merge(Posts, Users, by.x = "OwnerUserId", by.y = "Id", all = FALSE) # polaczenie Posts i Users w UsersPosts
  UsersPosts <- UsersPosts[, .(Id, OwnerUserId, DisplayName, Reputation, PostTypeId, Score)] # wybranie odpowiedznich kolumn
  Answer <- merge(Spam, UsersPosts, by.x = "PostId", by.y = "Id", all = FALSE) # polacznie Spam i UsersPosts w Answer
  Answer <- Answer[, .(PostId, PostTypeId, Score, OwnerUserId, DisplayName, Reputation)] # wybranie odpowiedznich kolumn
  Answer <- as.data.frame(Answer) # zamiana wyniku na data frame
  Answer # wypisanie wyniku
}


# compare(sql_3(Posts, Users, Votes), base_3(Posts, Users, Votes), allowAll = TRUE)
# compare(sql_3(Posts, Users, Votes), dplyr_3(Posts, Users, Votes), allowAll = TRUE)
# compare(sql_3(Posts, Users, Votes), table_3(Posts, Users, Votes), allowAll = TRUE)

# suppressMessages(
#   microbenchmark(
#     sqldf = sql_3(Posts, Users, Votes),
#     base = base_3(Posts, Users, Votes),
#     dplyr = dplyr_3(Posts, Users, Votes),
#     data.table = table_3(Posts, Users, Votes))
# )

# -----------------------------------------------------------------------------#
# Zadanie  4
# -----------------------------------------------------------------------------#

sql_4 <- function(Posts, Users, PostLinks){
  sqldf("
    SELECT Users.Id, Users.DisplayName, Users.UpVotes, Users.DownVotes, Users.Reputation,
      COUNT(*) AS DuplicatedQuestionsCount
    FROM (
      SELECT Duplicated.RelatedPostId, Posts.OwnerUserId
      FROM (
        SELECT PostLinks.RelatedPostId
        FROM PostLinks
        WHERE PostLinks.LinkTypeId == 3
        ) AS Duplicated JOIN Posts
        ON Duplicated.RelatedPostId = Posts.Id
      ) AS DuplicatedPosts JOIN Users ON Users.Id == DuplicatedPosts.OwnerUserId
    GROUP BY Users.Id
    HAVING DuplicatedQuestionsCount > 100
    ORDER BY DuplicatedQuestionsCount DESC
  ")
}

base_4 <- function(Posts, Users, PostLinks){
  
  Duplicated <- data.frame(PostLinks[PostLinks$LinkTypeId == 3, "RelatedPostId"]) # wyciagniecie wierszy z PostLinks ktore dla LinkTypeId maja 3, wybranie tylko kolumny RelatedPostId jako Duplicated
  colnames(Duplicated) <- "RelatedPostId" # zmiana nazwy kolumny
  DuplicatedPosts <- merge.data.frame(Duplicated, Posts, by.x = "RelatedPostId", by.y = "Id") # laczenie Duplicated i Posts w DuplicatedPosts
  DuplicatedPosts <- DuplicatedPosts[,c("RelatedPostId", "OwnerUserId")] # wybranie odpowiedznich kolumn z DuplicatedPosts
  Answer <- merge.data.frame(DuplicatedPosts, Users, by.x = "OwnerUserId", by.y = "Id") # laczenie DuplicatedPosts i Users w Answer
  Answer <- Answer[, c("OwnerUserId", "DisplayName", "UpVotes", "DownVotes", "Reputation")] # wybranie odpowiedznich kolumn z Answer
  colnames(Answer)[colnames(Answer) == "OwnerUserId"] <- "Id" # zmiana nazwy kolumny
  a <- data.frame(table(Id = Answer$Id)) # stworzenie kolumny pomocniczej a zawierajacej Id i zliczenie ich wystepowan w Answer
  Answer <- merge.data.frame(Answer, a, by = "Id") # laczenie Answer i a w Answer
  Answer <- unique(Answer) # wybieranie unikatowych wierszy
  colnames(Answer)[colnames(Answer) == "Freq"] <- "DuplicatedQuestionsCount" # zmiana nazwy kolumny
  Answer <- Answer[Answer$DuplicatedQuestionsCount > 100,] # wybranie wierszy gdzie DuplicatedQuestionsCount wynosi > 100
  Answer <- Answer[order(-Answer$DuplicatedQuestionsCount),] # sortowanie danych po DuplicatedQuestionsCount malejaco
  rownames(Answer) <- NULL # wyregulowanie nazw wierszy
  Answer # wypisanie wyniku

}

dplyr_4 <- function(Posts, Users, PostLinks){

  Answer <- PostLinks%>% # przepisanie wartosci PostLinks do Answer
    filter(LinkTypeId == 3)%>% # wyciagniecie wierszy z Answer ktore dla LinkTypeId maja 3
    select(RelatedPostId)%>% # wybranie tylko kolumny RelatedPostId
    inner_join(Posts, by = c("RelatedPostId" = "Id"))%>% # laczenie Answer i Posts
    select(RelatedPostId, OwnerUserId)%>% # wybranie odpowiedznich kolumn
    inner_join(Users, by = c("OwnerUserId" = "Id"))%>% # laczenie Answer i Users
    select(OwnerUserId, DisplayName, UpVotes, DownVotes, Reputation)%>% # wybranie odpowiedznich kolumn
    rename(Id = OwnerUserId)%>% # zmiana nazwy kolumny
    add_count(Id)%>% # dodanie kolumny zliczajacej powtorzenia Id w data frame
    distinct()%>% # wybranie unikatowych wierszy
    rename(DuplicatedQuestionsCount = n)%>% # zmiana nazwy kolumny
    filter(DuplicatedQuestionsCount > 100)%>% # wybranie wierszy gdzie DuplicatedQuestionsCount wynosi > 100
    arrange(desc(DuplicatedQuestionsCount))%>%  # sortowanie danych po DuplicatedQuestionsCount malejaco
    as.data.frame # zapisanie jako data frame
  Answer # wypisanie wynikow
}

table_4 <- function(Posts, Users, PostLinks){
  setDT(Posts) # zamiana Posts na data.table
  setDT(Users) # zamiana Users na data.table
  setDT(PostLinks) # zamiana PostLinks na data.table
  Duplicated <- as.data.table(PostLinks[LinkTypeId == 3, RelatedPostId]) # wyciagniecie wierszy z PostLinks ktore dla LinkTypeId maja 3, wybranie tylko kolumny RelatedPostId jako Duplicated
  setnames(Duplicated, "V1", "RelatedPostId") # zmiana nazwy kolumny
  DuplicatedPosts <- merge(Duplicated, Posts, by.x = "RelatedPostId", by.y = "Id", all = FALSE) # laczenie Duplicated i Posts
  DuplicatedPosts <- DuplicatedPosts[, .(RelatedPostId, OwnerUserId)] # wybranie odpowiedznich kolumn
  Answer <- merge(Users, DuplicatedPosts, by.x = "Id", by.y = "OwnerUserId", all = FALSE) # laczenie Users z Duplicated w Answers
  Answer <- Answer[, .(Id, DisplayName, UpVotes, DownVotes, Reputation)] # wybranie odpowiednich kolumn
  Answer <- Answer[, DuplicatedQuestionsCount := .N, by= .(Id, DisplayName, UpVotes, DownVotes, Reputation)] # grupowanie po Id, DisplayName, UpVotes, DownVotes, Reputation, zliczanie powtorzen do DuplicatedQuestionsCount
  Answer <- Answer[DuplicatedQuestionsCount > 100,] # wybranie wierszy gdzie DuplicatedQuestionsCount wynosi > 100
  Answer <- unique(Answer) # wybranie unikatowych wierszy
  setorder(Answer, -DuplicatedQuestionsCount) # sortowanie danych po DuplicatedQuestionsCount malejaco
  Answer <- as.data.frame(Answer) # zamiana wyniku na data frame
  Answer # wypisanie wynikow
  
}


# compare(sql_4(Posts, Users, PostLinks), dplyr_4(Posts, Users, PostLinks), allowAll = TRUE)
# compare(sql_4(Posts, Users, PostLinks), base_4(Posts, Users, PostLinks), allowAll = TRUE)
# compare(sql_4(Posts, Users, PostLinks), table_4(Posts, Users, PostLinks), allowAll = TRUE)

# suppressMessages(
#   microbenchmark(
#     sqldf = sql_4(Posts, Users, PostLinks),
#     base = base_4(Posts, Users, PostLinks),
#     dplyr = dplyr_4(Posts, Users, PostLinks),
#     data.table = table_4(Posts, Users, PostLinks))
# )

# -----------------------------------------------------------------------------#
# Zadanie 5
# -----------------------------------------------------------------------------#

sql_5 <- function(Posts, PostLinks){
  sqldf("
    SELECT QuestionsAnswers.Id, QuestionsAnswers.Title, QuestionsAnswers.Score,
      MAX(Duplicated.Score) AS MaxScoreDuplicated, COUNT(*) AS DulicatesCount,
      CASE
        WHEN QuestionsAnswers.Hour<'06' THEN 'Night'
        WHEN QuestionsAnswers.Hour<'12' THEN 'Morning'
        WHEN QuestionsAnswers.Hour<'18' THEN 'Day'
        ELSE 'Evening'
        END DayTime
    FROM (
      SELECT Id, Title, STRFTIME('%H',CreationDate) AS Hour, Score
      FROM Posts
      WHERE Posts.PostTypeId IN(1,2)
    ) AS  QuestionsAnswers
    JOIN (
      SELECT PL3.RelatedPostId, Posts.Score
      FROM (
        SELECT RelatedPostId, PostId
        FROM PostLinks
        WHERE LinkTypeId == 3
      ) AS  PL3
      JOIN Posts ON PL3.PostId = Posts.Id
    ) AS Duplicated
    ON QuestionsAnswers.Id = Duplicated.RelatedPostId
    GROUP BY QuestionsAnswers.Id
    ORDER BY DulicatesCount DESC
  ")
}

base_5 <- function(Posts, PostLinks){

  QuestionsAnswers <- Posts[Posts$PostTypeId %in% c(1,2), c("Id", "Title", "CreationDate", "Score")] # wyciagniecie wierszy z Posts ktore dla PostTypeId maja 1 lub 2, wybranie odpowiednich kolumn 
  QuestionsAnswers$CreationDate <- as.POSIXct(QuestionsAnswers$CreationDate, tz = "UTC", "%Y-%m-%dT%H:%M:%S") # zamiana kolumny CreationDate na typ kalendarzowo czasowy
  QuestionsAnswers$CreationDate <- format(QuestionsAnswers$CreationDate, format = "%H") # wyciagniecie godziny z kolumny CreationDate
  colnames(QuestionsAnswers)[colnames(QuestionsAnswers) == "CreationDate"] <- "Hour" # zmiana nazwy tej kolumny
  
  PL3 <- PostLinks[PostLinks$LinkTypeId == 3, c("RelatedPostId", "PostId")]  # wyciagniecie wierszy z PostLinks ktore dla LinkTypeId maja 3, wybranie kolumn RelatedPostId, PostId
  
  Duplicated <- merge.data.frame(PL3, Posts, by.x = "PostId", by.y = "Id") # laczenie PL3 z Posts
  Duplicated <- Duplicated[, c("RelatedPostId", "Score")] # wybranie odpowiedznihc kolumn
  
  QuDu <- merge.data.frame(QuestionsAnswers, Duplicated, by.x = "Id", by.y = "RelatedPostId") # laczenie QuestionsAnswers i Duplicated
  
  QuDu$DayTime <- ifelse(QuDu$Hour < '06', "Night", 
                         ifelse(QuDu$Hour < '12', "Morning", 
                                ifelse(QuDu$Hour < '18', "Day", "Evening"))) # dodanie kolumny DayTime zaleznej od kolumny Hour
  colnames(QuDu)[colnames(QuDu) == "Score.x"] <- "Score" # zmiana nazwy kolumny
  
  Max <- aggregate(QuDu$Score.y, by = list(QuDu$Id), FUN = max) # powstaniecie data frame Id i najwiekszych Score.y z Duplicated przypisanych do tych Id 
  QuDu <- merge.data.frame(QuDu, Max, by.x = "Id", by.y = "Group.1") # laczenie QuDu z Max
  colnames(QuDu)[colnames(QuDu) == "x"] <- "MaxScoreDuplicated" # zmiana nazwy kolumny
  a <- data.frame(table(Id = QuDu$Id)) # powstaniecie data frame a zawierajacej jednorazowo Id i zliczone liczby wystepowan tych Id w QuDu
  QuDu <- merge.data.frame(QuDu, a, by = "Id") # laczenie QuDu z a
  colnames(QuDu)[colnames(QuDu) == "Freq"] <- "DulicatesCount" # zmiana nazwy kolumny
  QuDu <- QuDu[, c("Id", "Title", "Score", "MaxScoreDuplicated", "DulicatesCount", "DayTime")] # wybranie odpowiednich kolumn
  QuDu <- unique(QuDu) # wybranie unikatowych wierszy
  QuDu <- QuDu[order(-QuDu$DulicatesCount),] # sortowanie po DulicatesCount malejaco
  QuDu # wypisanie wynikow
  
}

dplyr_5 <- function(Posts, PostLinks){
  
  Duplicated <- PostLinks%>% # skopowianie wartosci z PostLinks do Dupicated
    filter(LinkTypeId == 3)%>% # wyciagniecie wierszy ktore dla LinkTypeId maja 3
    select(RelatedPostId, PostId)%>% # wybranie kolumn RelatedPostId, PostId
    inner_join(Posts, by = c("PostId" = "Id"))%>% # laczenie z Posts
    select(RelatedPostId, Score) # wybranie odpowiedznihc kolumn
  
  Answer <- Posts%>% # skopowianie wartosci z Posts do Answer
    filter(PostTypeId %in% c(1, 2))%>% # wyciagniecie wierszy z Posts ktore dla PostTypeId maja 1 lub 2
    select(Id, Title, CreationDate, Score)%>% # wybranie odpowiednich kolumn
    mutate(CreationDate = as.POSIXct(CreationDate, tz = "UTC", "%Y-%m-%dT%H:%M:%S"))%>% # zamiana kolumny CreationDate na typ kalendarzowo czasowy
    mutate(CreationDate = format(CreationDate, format = "%H"))%>% # wyciagniecie godziny z kolumny CreationDate
    rename(Hour = CreationDate)%>% # zmiana nazwy tej kolumny
    mutate(DayTime = ifelse(Hour < '06', "Night",
                          ifelse(Hour < '12', "Morning",
                                 ifelse(Hour < '18', "Day", "Evening"))))%>% # dodanie kolumny DayTime zaleznej od kolumny Hour  
    inner_join(Duplicated, by = c("Id" = "RelatedPostId"))%>% # laczenie Answer z Duplicated
    rename(Score = Score.x)%>% # zmiana nazwy kolumny
    group_by(Id, Title, Score, DayTime)%>% # grupowanie po Id, Title, Score, DayTime
    summarise(MaxScoreDuplicated = max(Score.y), DulicatesCount = length(Id))%>% # stworzenie kolumn przechowujacych maksymalne dane z Score.y odpowidajace danemu Id i zliczacz powtorzen wierszy DulicatesCount
    arrange(desc(DulicatesCount))%>% # sortowanie po DulicatesCount malejaco
    select(Id, Title, Score, MaxScoreDuplicated, DulicatesCount, DayTime)%>% # wybranie odpowiednich kolumn
    as.data.frame # zamiana na data.frame
  Answer # wypisanie wyniku
  
}

table_5 <- function(Posts, PostLinks){
  
  setDT(Posts) # zamiana Posts na data.table
  setDT(PostLinks) # zamiana PostLinks na data.table
  QuestionsAnswers <- Posts[PostTypeId %in% c(1, 2), .(Id, Title, CreationDate, Score)] # wyciagniecie wierszy z Posts ktore dla PostTypeId maja 1 lub 2, wybranie odpowiednich kolumn 
  QuestionsAnswers[, Hour:=format(as.POSIXct(QuestionsAnswers$CreationDate, tz = "UTC", "%Y-%m-%dT%H:%M:%S"), "%H")] # powstaniecie kolumny Hour zawierajacej godzine z datowo-czaowej wersji CreationDate
  QuestionsAnswers <- QuestionsAnswers[, .(Id, Title, Hour, Score)] #wybranie odpowiednich kolumn
  
  PL3 <- PostLinks[LinkTypeId == 3, .(RelatedPostId, PostId)] # wyciagniecie wierszy z PostLinks ktore dla LinkTypeId maja 3, wybranie kolumn RelatedPostId, PostId
  
  Duplicated <- merge(PL3, Posts, by.x = "PostId", by.y = "Id", all = FALSE) # laczenie PL3 i Posts
  Duplicated <- Duplicated[, .(RelatedPostId, Score)] # wybranie odpowiednich kolumn
  
  QuDu <- merge(QuestionsAnswers, Duplicated, by.x = "Id", by.y = "RelatedPostId", all = FALSE) # laczenie QuestionsAnswers i Duplicated
  QuDu[, DayTime := ifelse(Hour < '06', "Night", 
                           ifelse(Hour < '12', "Morning", 
                                  ifelse(Hour < '18', "Day", "Evening")))] # dodanie kolumny DayTime zaleznej od kolumny Hour  
  setnames(QuDu, "Score.x", "Score") # zmiana nazwy kolumny
  QuDu <- QuDu[, .(MaxScoreDuplicated = max(Score.y), DulicatesCount = .N), by= .(Id, Title, Score, DayTime)] # grupowanie po Id, Title, Score, DayTime, tworzac kolumne przechowujaca maksymalne 
                                                                                                              # dane z Score.y odpowiadajace danemu Id i zliczacz powtorzen wierszy DulicatesCount
  setorder(QuDu, -DulicatesCount) # sortowanie po DulicatesCount malejaco
  QuDu <- QuDu[, .(Id, Title, Score, MaxScoreDuplicated, DulicatesCount, DayTime)] # wybranie odpowiednich kolumn
  QuDu <- as.data.frame(QuDu) # zamiana wyniku na data frame
  QuDu # wypisanie wyniku
  
}


# compare(sql_5(Posts, PostLinks), base_5(Posts, PostLinks), allowAll = TRUE)
# compare(sql_5(Posts, PostLinks), dplyr_5(Posts, PostLinks), allowAll = TRUE)
# compare(sql_5(Posts, PostLinks), table_5(Posts, PostLinks), allowAll = TRUE)

# suppressMessages(
#   microbenchmark(
#     sqldf = sql_5(Posts, PostLinks),
#     base = base_5(Posts, PostLinks),
#     dplyr = dplyr_5(Posts, PostLinks),
#     data.table = table_5(Posts, PostLinks))
# )
