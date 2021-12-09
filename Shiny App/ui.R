library(shiny)
library(plotly)
library(shinythemes)
library(RColorBrewer)
library(tidyverse)
library(lubridate)
library(maps)
library(leaflet)
library(unikn)

more_than_10 <- read.csv("chipotle_review.csv") %>% 
  group_by(business_id) %>% 
  summarise(number = n()) %>% 
  filter(number > 10) %>% 
  select(business_id) %>% 
  unlist


chip_attr <- read_csv("chip_attr.csv") %>% filter(business_id %in% more_than_10)


chip_food_scores <- read.csv("chip_food_sep_score.csv") 
chip_amb_scores <- read.csv("chip_amb_sep_score.csv") %>% select(-gloves)
chip_service_scores <- read.csv("chip_service_sep_score.csv")%>% select(-am)
chip_price_scores <- read.csv("chip_price_sep_score.csv")

review_sep <- read.csv("review_sep.csv")

# extract term ------------------------------------------------------------

key <- c("business_id","date","stars")
food_sep <- chip_food_scores %>% colnames %>% 
  setdiff(key)
amb_sep <- chip_amb_scores %>% colnames %>% 
  setdiff(key)
service_sep <- chip_service_scores %>% colnames %>% 
  setdiff(key)
price_sep <- chip_price_scores %>% colnames %>% 
  setdiff(key)

terms <- c(food_sep,amb_sep,service_sep,price_sep)
terms <- terms[!terms %>% str_detect("size")]
terms <- c("size",terms)

chip <- read.csv("chip_score.csv")
chip$food_norm <- (chip$food_score-min(chip$food_score))/max(chip$food_score)

chip_review <- read.csv("chipotle_review.csv")



chip_location <- read_csv("chip_location.csv")

library(tidyverse)

get_review <- function(chip_score,id){
  k <- which(chip_score$business_id == id)
  library(stringr)
  item <- colnames(chip_score)[3:112]
  mean_score <- colMeans(chip_score[,3:112])
  q3_score <- apply(chip_score[,3:112],2,quantile,0.75)
  comp1 <- (chip_score[k,3:112]<mean_score)
  comp2 <- (chip_score[k,3:112]>q3_score)
  
  #######################################################
  ########################  FOOD  #######################
  #######################################################
  
  food_dish <- c("black","guac", "water","cilantro","corn","salsa","bowl",
                 "tortilla", "beans", "veggie", "cheese", "taco", "meat", 
                 "lettuce", "rice", "sour.cream", "chicken","salad","burrito",
                 "chips", "sauce", "steak")
  food_adj <- c("fresh", "cooked", "taste","size","salty","stale")
  food_other <- c("ingredient","meal","portion","lunch")
  overall_food = ""
  if(chip_score$food_score[k]!=0){
    
    if(comp1[1]){overall_food <- paste0('Your <font size="3"><span style=\"color:#31678D\"><b>FOOD SCORE</b></span></font> is <span style=\"color:#E0607E\">lower than average</span>. ')}
    else if(comp2[1]){overall_food <- paste0('Your <font size="3"><span style=\"color:#31678D\">FOOD SCORE</span></font> is <span style=\"color:#59C7EB\">better than 75% of other chain stores</span>. ')}
    else{overall_food <- paste0('Your <font size="3"><span style=\"color:#31678D\">FOOD SCORE</span></font> is <span style=\"color:#0AA398\">slightly better than average store</span>. ')}
    
  }
  
  dish_n <- ""
  dish_p <- ""
  
  for(i in food_dish){
    if(chip_score[k,i]!=0){
      if(comp1[,i]){dish_n = paste0(dish_n,i,", ")}
      if(comp2[,i]){dish_p = paste0(dish_p,i,", ")}
    }
  }
  
  #"Dishes and ingredients: "
  posd <- ""
  negd <- ""
  if(dish_p != ""){
    dish_p <- str_remove(dish_p,"[,][ ]$")
    posd <- paste0("Customers enjoy <span style=\"color:#00A9E0\">",dish_p,"</span>. ")
  }
  if(dish_n != ""){
    dish_n <- str_remove(dish_n,"[,][ ]$")
    negd <- paste0("The followings are not as satisfying: <span style=\"color:#E68098\">",dish_n,"</span>. ")
  }
  
  #fresh
  fresh = ""
  if(chip_score[k,"fresh"]!=0){
    if(comp1[,"fresh"] || comp1[,"stale"]){fresh <- "Some customers complain that the ingredients are <span style=\"color:#E68098\">not fresh</span>. "}
    if(comp2[,"fresh"] && comp2[,"stale"]){fresh <- "The ingredients in your store are <span style=\"color:#00A9E0\">fresh</span>. "}
  }  
  
  # taste
  
  taste = ""
  if(chip_score[k,"taste"]!=0){
    if(comp1[,"taste"]){fresh <- "As for taste, your store gets <span style=\"color:#E0607E\">less scores than the average</span>. "}
    if(comp2[,"taste"]){fresh <- "Your store gets <span style=\"color:#59C7EB\">high score</span> on taste. "}
  }  
  
  
  
  if(chip_score[k,"salty"]!=0){
    taste = "The foods you offer are some times <span style=\"color:#E68098\">salty</span>. "
  }
  
  sugg_food <-paste0(overall_food,posd,negd,fresh,taste,collapse = "<br>")
  
  
  ##########################################################
  ########################  SERVICE  #######################
  ##########################################################
  
  serv_wait <- c("time", "fast","line","minute","wait", "slow", "quick")
  serv_staff <- c("attitude", "staff", "friendly", "welcoming", "manager", 
                  "supervisor", "helpful", "employee","polite", "girl","guy",
                  "train", "rude","wrong","serving","experience", "cashier", 
                  "lady", "worker","management","gloves", "hand" )
  serv_online <- c("app","online","pickup","deliver","phone")
  #am pm
  overall_serv = ""
  if(chip_score$service_score[k]!=0){
    if(comp1[3]){overall_serv <- paste0('Your <font size="3"><span style=\"color:#F9E324\"><b>SERVICE SCORE</b></span></font> is <span style=\"color:#E0607E\">lower than average</span>. ')}
    else if(comp2[3]){overall_serv <- paste0('Your <font size="3"><span style=\"color:#F9E324\">SERVICE SCORE</span></font> is <span style=\"color:#59C7EB\">better than 75% of other chain stores</span>. ')}
    else{overall_serv <- paste0('Your <font size="3"><span style=\"color:#F9E324\">SERVICE SCORE</span></font> is <span style=\"color:#0AA398\">slightly better than average store</span>. ')}
  }
  
  
  
  wait = 0
  n = 0
  for(i in serv_wait){
    if(chip_score[k,i]!=0){
      n = n+1
      if(comp1[,i]){wait = wait+1}
    }
  }
  speed = ""
  if (n>0 && wait>=n/2) {speed <- "The <span style=\"color:#E68098\">serving speed</span> should be improved. "}
  
  
  staff = 0
  n = 0
  for(i in serv_staff){
    if(chip_score[k,i]!=0){
      n = n+1
      if(comp1[,i]){staff = staff+1}
      if(comp2[,i]){staff = staff-1}
    }
  }
  att = ""
  if (n>0 && staff>=n/2) {att <- "And customers want better service from the <span style=\"color:#E68098\">working staffs</span>. "}
  if(staff<0){att <- "And customers think your working staffs are <span style=\"color:#00A9E0\">doing well</span>. "}
  
  online = 0
  n = 0
  for(i in serv_online){
    if(chip_score[k,i]!=0){
      n = n+1
      if(comp1[,i]){online =online +1}
      if(comp2[,i]){online =online -1}
    }
  }
  ol = ""
  if (n>0 && online>=n/2) {ol <- "Paying more attention on <span style=\"color:#E68098\">online services</span>, including ordering with <span style=\"color:#E68098\">app</span> and <span style=\"color:#E68098\">phone call</span>, <span style=\"color:#E68098\">pickup</span>, <span style=\"color:#E68098\">deliver services</span> may help you attract more customers. "}
  if(online<0){ol <- "<span style=\"color:#00A9E0\">Online services</span>, including ordering with <span style=\"color:#00A9E0\">app</span> and <span style=\"color:#00A9E0\">phone call</span>, <span style=\"color:#00A9E0\">pickup</span>, <span style=\"color:#00A9E0\">deliver services</span> are your highlights. "}
  
  
  sugg_serv <- paste0(overall_serv,speed,att,ol,collapse = "<br>")
  
  
  ##########################################################
  #########################  PRICE  ########################
  ##########################################################
  
  price <- item[103:110]
  overall_price = ""
  if(chip_score$price_score[k]!=0){
    if(comp1[4]){overall_price <- paste0('Your <font size="3"><span style=\"color:#30A66E\"><b>PRICE SCORE</b></span></font> is <span style=\"color:#E0607E\">lower than average</span>. ')}
    else if(comp2[4]){overall_price <- paste0('Your <font size="3"><span style=\"color:#30A66E\">PRICE SCORE</span></font> is <span style=\"color:#59C7EB\">better than 75% of other chain stores</span>. ')}
    else{overall_price <- paste0('Your <font size="3"><span style=\"color:#30A66E\">PRICE SCORE</span></font> is <span style=\"color:#0AA398\">slightly better than average store</span>. ')}
  }
  
  
  
  
  charge = 0
  n = 0
  for(i in price){
    if(chip_score[k,i]!=0){
      n = n+1
      if(comp1[,i]){charge = charge +1}
      if(comp2[,i]){charge = charge -1}
    }
  }
  pc = ""
  if (n>0 && charge>=n/2) {pc <- "<span style=\"color:#E68098\">Higher cost performance</span> are expected. "}
  if(charge<0) {pc <- "Customers think the food really <span style=\"color:#00A9E0\">worth its price</span>. "}
  
  sugg_price <- paste0(overall_price,pc)
  
  ##########################################################
  #######################  AMBIANCE  #######################
  ##########################################################
  
  amb <- c("ambiance", "environment","atmosphere","counter","seat","music",
           "wifi","window" , "floor","decor" , "door" , "table") 
  amb_clean <- c("dirty", "filthy","clean", "trash" ,"disgusting","mess")
  amb_park <- "parking"
  overall_amb = ""
  if(chip_score$amb_score[k]!=0){
    if(comp1[4]){overall_amb <- paste0('Your <font size="3"><span style=\"color:#440154\"><b>AMBIENCE SCORE</b></span></font> is <span style=\"color:#E0607E\">lower than average</span>. ')}
    else if(comp2[4]){overall_amb <- paste0('Your <font size="3"><span style=\"color:#440154\">AMBIENCE SCORE</span></font> is <span style=\"color:#59C7EB\">better than 75% of other chain stores</span>. ')}
    else{overall_amb <- paste0('Your <font size="3"><span style=\"color:#440154\">AMBIENCE SCORE</span></font> is <span style=\"color:#0AA398\">slightly better than average store</span>. ')}
  }
  
  
  
  clean = 0
  n = 0
  for(i in amb_clean){
    if(chip_score[k,i]!=0){
      n = n+1
      if(comp1[,i]){clean = clean +1}
      if(comp2[,i]){clean = clean -1}
    }
  }
  cl = ""
  if (n>0 && clean>=n/2) {cl <- "<span style=\"color:#E68098\">Sanitary</span> condition needs improvement. "}
  if(clean<0) {cl <- "The <span style=\"color:#00A9E0\">sanitary</span> condition is good. "}
  
  park = ""
  if(chip_score[k,"parking"]!=0){
    if(comp1[,i]){park = "Customers may find it <span style=\"color:#E68098\">inconvenient to park</span> around your restaurant. "}
  }
  
  sugg_amb <- paste0(overall_amb,cl,park,collapse = "<br>")
  return(c(sugg_food,sugg_serv,sugg_price,sugg_amb))
  
}

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  headerPanel(h1("Chipotle Analyzer")),
  tags$head(tags$style(
    HTML(
      "
      @import url('//fonts.googleapis.com/css?family=Cabin+Sketch|Cabin:400,700');
      h1 {
      font-family: 'Cabin Sketch';
      font-weight: 500;
      line-height: 1.5;
      }
      "
    )
  )
  ),
  sidebarLayout(
    sidebarPanel(
      htmlOutput("caption"),
      dateRangeInput("date","",
                     start = chip_review$date %>% date%>% min,
                     end = chip_review$date %>% date %>% max,
                     min = chip_review$date %>% date%>% min,
                     max = chip_review$date %>% date %>% max),
      htmlOutput("review_number"),
      htmlOutput("which"),
      plotlyOutput("hist_sep"),
      uiOutput("contact")
      
      
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map and ratings",
                 leafletOutput("map"),
                 htmlOutput("claim2"),
                 fluidRow(
                   column(6,plotlyOutput("hist_rating")),
                   column(6,plotlyOutput("line_rating"))
                 )
        ),
        tabPanel("Sentiment Scores",
                 plotlyOutput("bar_sep"),
                 fluidRow(
                   column(8,htmlOutput("related_word")),
                   column(4,actionButton("randomize","Randomize!",class = "btn-success"))),
                 htmlOutput("claim"),
                 htmlOutput("related_review_positive"),
                 htmlOutput("related_review_negative")
                 
        ),
        tabPanel("Advice",
                 htmlOutput("amb_advice"),
                 htmlOutput("food_advice"),
                 htmlOutput("price_advice"),
                 htmlOutput("service_advice")
                 
        )
        
      )
      
      
    )
  )
  
  
)

