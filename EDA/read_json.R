library("jsonlite")
rm(list=ls())
covid <- stream_in(file("covid.json"))
save(covid,file = "covid.Rda")
rm(list=ls())
business <- stream_in(file("business.json"))
save(business, file = "business.Rda")
rm(list=ls())
tip <- stream_in(file("tip.json"))
save(tip, file = "tip.Rda")
rm(list=ls())
user <- stream_in(file("user.json"))
save(user, file = "user.Rda")
rm(list=ls())
allreview <- stream_in(file("allreviews.json"))
save(allreview, file = "allreview.Rda")
a <- read_json("allreviews.json")
head(a)
a[[1]]
fun1 <- function(lst, n){
  sapp1ly(lst, `[`, n)
}
allreviews <- do.call(rbind.data.frame, a)
str_detect(business$name,"QDOBA")
sum(((fun1(a,3) %>% unlist) %in% business_ID_2))


chipotle <- read_json("chipreviews.json")
length(chipotle)
do.call(rbind.data.frame, chipotle[1:10])
load("user.Rda")
colnames(user)
user_part[1,]
user_part <- user[1000,]
rm(user)
