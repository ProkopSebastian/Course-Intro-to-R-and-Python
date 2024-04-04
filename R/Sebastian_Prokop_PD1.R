### Przetwarzanie danych w językach R i Python 2024L
### Praca domowa nr. 1 / Homework Assignment no. 1
###
### WAŻNE
### Ten plik powinien zawierać tylko rozwiązania zadań w postaci
### definicji funkcji, załączenia niezbędnych bibliotek
### i komentarzy do kodu.
###
### Raport powinien zawierać:
### * source() tego pliku,
### * odczytanie danych,
### * dołączenie bibliotek,
### * pomiary czasu wykonania (z mikrobenchmarkiem),
### * porównanie równoważności wyników,
### * interpretację zapytań.
# -----------------------------------------------------------------------------#
# Task 1
# -----------------------------------------------------------------------------#

sqldf_1 <- function(Posts, Users){
  sql_command <- "SELECT Location, COUNT(*) AS Count
  FROM (
  SELECT Posts.OwnerUserId, Users.Id, Users.Location
  FROM Users
  JOIN Posts ON Users.Id = Posts.OwnerUserId
  )
  WHERE Location NOT IN ('')
  GROUP BY Location
  ORDER BY Count DESC
  LIMIT 10
  "
  sqldf_sol <- sqldf(sql_command)
  return(sqldf_sol)
}

base_1 <- function(Posts, Users){
  inner_command <- merge(Users, Posts, by.x = "Id", by.y = "OwnerUserId")
  inner_command_selected <- inner_command[, c("Id", "Location")] # najpierw działało bez przecinka, potem musiałem dodać
  filtered <- inner_command_selected[!(inner_command_selected$Location == "" | is.na(inner_command_selected$Location)), ]
  grouped <- aggregate(Id ~ Location, data = filtered, FUN = length)
  grouped_sorted <- grouped[order(-grouped$Id), ]
  ans <- head(grouped_sorted, 10)
  # print(ans)
  # print(class(ans))
  ans <- data.frame(Location = ans$Location, Count = ans$Id) # Dlaczego to musi tu być?
  # rownames(ans) <- NULL
  # print(ans)
  return(ans)
}

dplyr_1 <- function(Posts, Users){
  joined <- inner_join(Users, Posts, by = join_by("Id" == "OwnerUserId"))
  ans <- joined %>% select(Location, Id) %>%
  filter(joined$Location != "") %>%
  group_by(Location) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  top_n(10, Count)
  ans <- as.data.frame(ans)
  return(ans)
}

data.table_1 <- function(Posts, Users){
  UsersDT <- setDT(Users)
  PostsDT <- setDT(Posts)
  polaczone <- merge(UsersDT, PostsDT, by.x = "Id", by.y = "OwnerUserId")
  sol_table <- polaczone[Location != "", .(Id, Location)
  ][, .(.N), keyby = .(Location)
  ][order(-N)
  ][1:10]
}

# -----------------------------------------------------------------------------#
# Task 2
# -----------------------------------------------------------------------------#

sqldf_2 <- function(Posts, PostLinks){
  sql_zap <- "SELECT Posts.Title, RelatedTab.NumLinks
  FROM
  (
  SELECT RelatedPostId AS PostId, COUNT(*) AS NumLinks
  FROM PostLinks
  GROUP BY RelatedPostId
  ) AS RelatedTab
  JOIN Posts ON RelatedTab.PostId=Posts.Id
  WHERE Posts.PostTypeId=1
  ORDER BY NumLinks DESC
  "
  sol_sql <- sqldf(sql_zap)
  return(sol_sql)
}

base_2 <- function(Posts, PostLinks){
  #res <- PostLinks[, c("RelatedPostId")]
  #res <- as.data.frame(res) # w tym miejscu straciłem dużo czasu, po wyborze w powyższej linii
  # class(res) zwracało integer, zostawiam, żeby pamiętać
  res <- subset(PostLinks, select = c("RelatedPostId"))
  class(res)
  RelatedTab <- aggregate(. ~ res$RelatedPostId, data = res, FUN = length)
  names(RelatedTab) <- c("PostId", "NumLinks")
  resu <- merge(x = RelatedTab, y = Posts, by.x = "PostId", by.y = "Id", all.x = FALSE, all.y = FALSE)
  resu <- resu[resu$PostTypeId == 1, c("Title", "NumLinks")]
  resu <- resu[order(-resu$NumLinks), ]
  rownames(resu) <- NULL
  return(resu)
}

dplyr_2 <- function(Posts, PostLinks){
  # na pewnym etapie pojawia się tibble zamiast data.frame, to podobna rzecz.
  RelatedTab <- PostLinks %>%
  group_by(RelatedPostId) %>%
  summarise(NumLinks = n())
  joined <- inner_join(x = RelatedTab, y = Posts, by = join_by("RelatedPostId" == "Id")) %>%
  filter(PostTypeId == 1)
  extracted <- joined[c("Title", "NumLinks")]
  sorted <- arrange(.data = extracted, desc(extracted$NumLinks))
  sorted <- as.data.frame(sorted)
}

data.table_2 <- function(Posts, PostLinks){
  PostLinksDT <- setDT(PostLinks)
  PostsDT <- setDT(Posts)
  tmp <- PostLinksDT[, .(.N), by = RelatedPostId]
  tmp <- merge.data.table(x = PostsDT, y = tmp, by.x = "Id", by.y = "RelatedPostId")
  tmp <- tmp[PostTypeId == 1, .(Title, N)][order(-N)]
  tmp <- as.data.frame(tmp)
  names(tmp)[2] <- "NumLinks"
  return(tmp)
}

# -----------------------------------------------------------------------------#
# Task 3
# -----------------------------------------------------------------------------#

sqldf_3 <- function(Comments, Posts, Users){
  sql_3 = "SELECT Title, CommentCount, ViewCount, CommentsTotalScore,
  DisplayName, Reputation, Location
  FROM (
  SELECT Posts.OwnerUserId, Posts.Title, Posts.CommentCount, Posts.ViewCount,
  CmtTotScr.CommentsTotalScore
  FROM (
  SELECT PostId, SUM(Score) AS CommentsTotalScore
  FROM Comments
  GROUP BY PostId
  ) AS CmtTotScr
  JOIN Posts ON Posts.Id = CmtTotScr.PostId
  WHERE Posts.PostTypeId=1
  ) AS PostsBestComments
  JOIN Users ON PostsBestComments.OwnerUserId = Users.Id
  ORDER BY CommentsTotalScore DESC
  LIMIT 10
  "
  sql_3_sol <- sqldf(sql_3)
}

base_3 <- function(Comments, Posts, Users){
  CmtTotScr <- aggregate(Score ~ PostId, data = Comments, FUN = sum)
  names(CmtTotScr)[2] <- "CommentsTotalScore"
  PostsBestComments <- merge(x = CmtTotScr, y = Posts, by.x = "PostId", by.y = "Id", all.x = FALSE, all.y = FALSE)
  PostsBestComments <- PostsBestComments[PostsBestComments$PostTypeId == 1, c("OwnerUserId", "Title", "CommentCount", "ViewCount", "CommentsTotalScore")]
  ans3 <- merge(PostsBestComments, Users, by.x = "OwnerUserId", by.y = "Id", all.x = FALSE, all.y = FALSE)
  ans3 <- ans3[order(-ans3$CommentsTotalScore), c("Title", "CommentCount", "ViewCount", "CommentsTotalScore", "DisplayName", "Reputation", "Location")]
  ans3 <- head(ans3, 10)
  rownames(ans3) <- NULL
  return(ans3)
}

dplyr_3 <- function(Comments, Posts, Users){
  CmtTotScr <- Comments %>%
  group_by(PostId) %>%
  summarise(CommentsTotalScore = sum(Score))
  PostsBestComments <- inner_join(Posts, CmtTotScr, by = join_by("Id" == "PostId"))
  PostsBestComments <- PostsBestComments %>% filter(PostsBestComments$PostTypeId == 1) %>%
  select(OwnerUserId, Title, CommentCount, ViewCount, CommentsTotalScore)
  ansd <- inner_join(PostsBestComments, Users, by = join_by("OwnerUserId" == "Id"))  %>% 
  select(Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location) %>%
  top_n(10, CommentsTotalScore) %>%
  arrange(desc(CommentsTotalScore))
  ans3 <- as.data.frame(ansd)
}

data.table_3 <- function(Comments, Posts, Users){
  PostsDT <- setDT(Posts)
  UsersDT <- setDT(Users)
  CommentsDT <- setDT(Comments)
  CmtTotScr <- CommentsDT[, .(CommentsTotalScore=sum(Score)), by = PostId]
  PostsBestComments <- merge.data.table(CmtTotScr, PostsDT, by.x = "PostId", by.y = "Id")
  PostsBestComments <- PostsBestComments[PostsBestComments$PostTypeId == 1, .(OwnerUserId, Title, CommentCount, ViewCount, CommentsTotalScore)]
  ans <- merge.data.table(Users, PostsBestComments, by.x = "Id", by.y = "OwnerUserId")
  ans <- ans[order(-CommentsTotalScore)][1:10, .(Title, CommentCount, ViewCount, CommentsTotalScore,
  DisplayName, Reputation, Location)]
  ans4 <- as.data.frame(ans)
}

# -----------------------------------------------------------------------------#
# Task 4
# -----------------------------------------------------------------------------#

sqldf_4 <- function(Posts, Users){
  sql4 <- "SELECT DisplayName, QuestionsNumber, AnswersNumber, Location,
  Reputation, UpVotes, DownVotes
  FROM (
  SELECT *
  FROM (
  SELECT COUNT(*) as AnswersNumber, OwnerUserId
  FROM Posts
  WHERE PostTypeId = 2
  GROUP BY OwnerUserId
  ) AS Answers
  JOIN
  (
  SELECT COUNT(*) as QuestionsNumber, OwnerUserId
  FROM Posts
  WHERE PostTypeId = 1
  GROUP BY OwnerUserId
  ) AS Questions
  ON Answers.OwnerUserId = Questions.OwnerUserId
  WHERE AnswersNumber > QuestionsNumber
  ORDER BY AnswersNumber DESC
  LIMIT 5
  ) AS PostsCounts
  JOIN Users
  ON PostsCounts.OwnerUserId = Users.Id"

  res1 <- sqldf(sql4)
}

base_4 <- function(Posts, Users){
  tmp1 <- Posts[Posts$PostTypeId == 2, ]
  tmp1 <- aggregate(Id ~ tmp1$OwnerUserId, data = tmp1, FUN = length)
  names(tmp1) <- c("OwnerUserId", "AnswersNumber")

  tmp2 <- Posts[Posts$PostTypeId == 1, ]
  tmp2 <- aggregate(Id ~ tmp2$OwnerUserId, data = tmp2, FUN = length)
  names(tmp2) <- c("OwnerUserId", "QuestionsNumber")

  postsCounts <- merge(x = tmp1, y = tmp2, by.x = "OwnerUserId", by.y = "OwnerUserId")
  postsCounts <- postsCounts[postsCounts$AnswersNumber > postsCounts$QuestionsNumber, ]
  postsCounts <- postsCounts[order(-postsCounts$AnswersNumber), ]
  postsCounts <- head(postsCounts, 5)
  res2 <- merge(postsCounts, Users, by.x = "OwnerUserId", by.y = "Id", sort = FALSE)
  res2 <- res2[, c("DisplayName", "QuestionsNumber", "AnswersNumber", "Location", "Reputation", "UpVotes", "DownVotes")]
}

dplyr_4 <- function(Posts, Users){
  Answers <- Posts %>%
  filter(PostTypeId == 2) %>%
  group_by(OwnerUserId) %>%
  summarise(AnswersNumber = n()) %>%
  na.omit()
  Questions <- Posts %>%
  filter(PostTypeId == 1) %>%
  group_by(OwnerUserId) %>%
  summarise(QuestionsNumber = n()) %>%
  na.omit()
  PostsCounts <- inner_join(Answers, Questions, join_by("OwnerUserId" == "OwnerUserId")) %>%
  filter(AnswersNumber > QuestionsNumber) %>%
  top_n(5, AnswersNumber) %>%
  arrange(desc(AnswersNumber))
  res3 <- inner_join(Users, PostsCounts, join_by("Id" == "OwnerUserId")) %>%
  select(DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes) %>%
  arrange(desc(AnswersNumber))
}

data.table_4 <- function(Posts, Users){
  postsDT <- setDT(Posts)
  usersDT <- setDT(Users)
  answers <- postsDT[PostTypeId == 2, .(AnswersNumber = .N), by = OwnerUserId]
  questions <- postsDT[PostTypeId == 1, .(QuestionsNumber = .N), by = OwnerUserId]
  answers <- na.omit(answers)
  questions <- na.omit(questions)
  postsCounts <- merge.data.table(answers, questions, by.x = "OwnerUserId", by.y = "OwnerUserId")
  postsCounts <- postsCounts[postsCounts$AnswersNumber > postsCounts$QuestionsNumber, ][order(-AnswersNumber)][1:5,]
  res4 <- merge.data.table(Users, postsCounts, by.x = "Id", by.y = "OwnerUserId")
  res4 <- res4[, .(DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes)][order(-AnswersNumber)]
  res4 <- as.data.frame(res4)
}

# -----------------------------------------------------------------------------#
# Task 2
# -----------------------------------------------------------------------------#

sqldf_5 <- function(Posts, Users){
  sql <- "SELECT
  Users.AccountId,
  Users.DisplayName,
  Users.Location,
  AVG(PostAuth.AnswersCount) as AverageAnswersCount
  FROM
  (
  SELECT
  AnsCount.AnswersCount,
  Posts.Id,
  Posts.OwnerUserId
  FROM (
  SELECT Posts.ParentId, COUNT(*) AS AnswersCount
  FROM Posts
  WHERE Posts.PostTypeId = 2
  GROUP BY Posts.ParentId
  ) AS AnsCount
  JOIN Posts ON Posts.Id = AnsCount.ParentId
  ) AS PostAuth
  JOIN Users ON Users.AccountId=PostAuth.OwnerUserId
  GROUP BY OwnerUserId
  ORDER BY AverageAnswersCount DESC
  LIMIT 10"
  sol1 <- sqldf(sql)
  return(sol1)
}

base_5 <- function(Posts, Users){
  anscount <- Posts[Posts$PostTypeId == 2, ]
  anscount <- aggregate(Id ~ ParentId, data = Posts, FUN = length)
  names(anscount)[2] <- "AnswersCount"
  postAuth <- merge(Posts, anscount, by.x = "Id", by.y = "ParentId")
  postAuth <- postAuth[, c("AnswersCount", "Id", "OwnerUserId")] # w notatnikach jupytera ten prezcinek nie jest potrzebny 
  sol2 <- merge(postAuth, Users, by.x = "OwnerUserId", by.y = "AccountId")
  sol2 <- aggregate(AnswersCount ~ OwnerUserId, data = sol2, FUN = mean)
  sol2 <- merge(sol2, Users[, c("AccountId", "DisplayName", "Location")], by.x = "OwnerUserId", by.y = "AccountId")
  sol2 <- sol2[order(-sol2$AnswersCount, -sol2$OwnerUserId), c("OwnerUserId", "DisplayName", "Location", "AnswersCount")]
  sol2 <- head(sol2, 10)
  rownames(sol2) <- NULL
  colnames(sol2)[1] <- "AccountId"
  colnames(sol2)[4] <- "AverageAnswersCount"
  return(sol2)
}

dplyr_5 <- function(Posts, Users){
  anscount <- Posts %>%
  filter(PostTypeId == 2) %>%
  group_by(ParentId) %>%
  summarise(AnswersCount = n())
  postAuth <- inner_join(Posts, anscount, join_by("Id" == "ParentId")) %>%
  select(AnswersCount, Id, OwnerUserId)
  sol3 <- inner_join(x = Users, y = postAuth, by = join_by(AccountId == OwnerUserId), relationship = "many-to-many") %>%
  group_by(AccountId) %>%
  reframe(DisplayName, Location, AverageAnswersCount = mean(AnswersCount)) %>%
  top_n(10, AverageAnswersCount) %>%
  arrange(desc(AverageAnswersCount), desc(AccountId)) %>%
  as.data.frame()
  return(sol3)
}

data.table_5 <- function(Posts, Users){
  PostsDT <- setDT(Posts)
  UsersDT <- setDT(Users)
  anscount <- PostsDT[PostTypeId == 2, .(AnswersCount = .N), by = ParentId]
  postauth <- merge.data.table(PostsDT, anscount, by.x = "Id", by.y = "ParentId")
  postauth <- postauth[, c("AnswersCount", "Id", "OwnerUserId")]
  sol4 <- merge.data.table(UsersDT, postauth, by.x = "AccountId", by.y = "OwnerUserId")
  sol4 <- na.omit(sol4)
  sol4 <- sol4[, .(DisplayName, Location, AverageAnswersCount = mean(AnswersCount)), by = AccountId
  ][order(-AverageAnswersCount, -AccountId)
  ][1:10, ]
  sol4 <- as.data.frame(sol4)
}
