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

server <- function(input, output, session) {
  
  output$caption <- renderText({
    '<font size="4"><span style=\"color:#008ECE\">Select Date Range: </span></font>'
  })
  output$review_number <- renderText({
    
    validate(
      need(id(),""))
    chip_filtered <- chip_review%>% 
      filter(business_id == id()) %>% 
      filter(date(date) >= input$date[1] & date(date) < input$date[2]) %>% 
      mutate(quat = quarter(date),year = year(date)) %>% 
      mutate(half = ifelse(quat<=2,1,2))
    paste0('<font size="4"><span style=\"color:#008ECE\"><b>Review Number:', nrow(chip_filtered),"</b></span></font>")
  })
  
  output$contact <- renderUI({
    email <- a("txu98@wisc.edu",href = "txu98@wisc.edu")
    link <- a("GitHub link to this project",href = "https://github.com/tsai0104/STAT-628-Module-3-Group-10-")
    tagList(HTML(paste("If you have further questions about this shiny app,","please contact us for details.<br/>",sep = "<br/>")),
            HTML("Email: Tinghui Xu "),email,
            HTML("<br/>"),link)
    
  })
  
  # chip_filtered <- reactive({chip_review%>% 
  #   filter(business_id == id()) %>% 
  #   filter(date(date) >= input$date[1] & date(date) < input$date[2]) %>% 
  #   mutate(quat = quarter(date),year = year(date)) %>% 
  #   mutate(half = ifelse(quat<=2,1,2))})
  
  
  
  
  chip_sep <- reactive({chip_sep <- chip%>% 
    filter(business_id == id()) })
  
  id <- reactive({
    input$map_marker_click$id
  })
  
  # map ---------------------------------------------------------------------
  
  output$map <- renderLeaflet({
    chip_map <- leaflet() %>%
      setView(-96, 37.8, 3) %>% 
      addTiles() %>% 
      addCircleMarkers(lng = chip_attr$longitude,
                       lat = chip_attr$latitude, 
                       popup = chip_attr$address,
                       radius = 10,
                       clusterOptions = markerClusterOptions(),
                       layerId = chip_attr$business_id)
    chip_map
  })
  # hist_rating -------------------------------------------------------------
  output$hist_rating <- renderPlotly({
    validate(
      need(id(),"Please click on the map and choose one Chipotle restaurant."))
    chip_filtered <- chip_review%>% 
      filter(business_id == id()) %>% 
      filter(date(date) >= input$date[1] & date(date) < input$date[2]) %>% 
      mutate(quat = quarter(date),year = year(date)) %>% 
      mutate(half = ifelse(quat<=2,1,2))
    validate(
      need(nrow(chip_filtered)!=0,"No reviews in the date range!")
    )
    
    chip_all <- chip_review %>% 
      filter(date(date) >= input$date[1] & date(date) < input$date[2]) %>% 
      mutate(quat = quarter(date),year = year(date)) %>% 
      mutate(half = ifelse(quat<=2,1,2))
    
    average_rating <- chip_all %>% group_by(stars) %>% summarise(average = n()/nrow(chip_all))
    chip_rating <- chip_filtered %>% group_by(stars) %>% summarise(proportion = n()/nrow(chip_filtered))
    chip_rating$stars <-factor(chip_rating$stars,levels = c("1","2","3","4","5"))
    
    ggplotly(ggplot(chip_rating)+geom_bar(aes(stars,y = proportion,fill = stars), stat = "identity")+
               scale_x_discrete(drop=FALSE)+
               xlab("Rating")+
               ylab("Percentage")+
               scale_fill_manual(values = as.character(pal_seeblau[1:5]))+
               geom_point(data = average_rating,aes(stars,average),col = "#E0607E")+
               theme_unikn()) %>% 
      layout(showlegend = F)
  })
  
  # line_rating -------------------------------------------------------------
  output$line_rating <- renderPlotly({
    validate(
      need(id(),"Please click on the map and choose one Chipotle restaurant."))
    chip_filtered <- chip_review%>% 
      filter(business_id == id()) %>% 
      filter(date(date) >= input$date[1] & date(date) < input$date[2]) %>% 
      mutate(quat = quarter(date),year = year(date)) %>% 
      mutate(half = ifelse(quat<=2,1,2))
    
    validate(
      need(nrow(chip_filtered)!=0,"No reviews in the date range!")
    )
    chip_all <- chip_review %>% 
      filter(date(date) >= input$date[1] & date(date) < input$date[2]) %>% 
      mutate(quat = quarter(date),year = year(date)) %>% 
      mutate(half = ifelse(quat<=2,1,2))
    
    chip_rating_time <- chip_filtered %>% group_by(year,half) %>% 
      summarize(average_rating = mean(stars, na.rm = T),
                number = n()) %>% 
      mutate(plot_time = date(ifelse(half == 1, paste0(year,"-01-01"),paste0(year,"-06-30"))))
    chip_all_time <- chip_all %>% 
      filter(date >= min(chip_filtered$date) & date <= max(chip_filtered$date)) %>% 
      group_by(year,half) %>% 
      summarize(average_rating = mean(stars, na.rm = T),
                number = n()) %>% 
      mutate(plot_time = date(ifelse(half == 1, paste0(year,"-01-01"),paste0(year,"-06-30"))))
    
    ggplotly(ggplot(chip_rating_time)+geom_line(aes(plot_time,average_rating),col = "skyblue")+
               geom_line(data=chip_all_time, aes(plot_time,average_rating), col = "#E0607E", linetype = "dashed")+
               xlab("Date")+
               ylab("Rating")+theme_unikn())
  })
  
  # radar_sep ---------------------------------------------------------------
  output$hist_sep <- renderPlotly({
    validate(
      need(id(),"Please click on the map and choose one Chipotle restaurant.")
    )
    
    
    chip_filtered_sep_scores <- data.frame(sep = factor(c("Food","Ambience","Service","Price"),ordered = T),
                                           filter_score = chip_sep()[,c(3,4,5,6)] %>% as.numeric(),
                                           average_score = sapply(chip[,c(3,4,5,6)],median,na.rm=T) %>% as.numeric())
    # chip_1 <- chip%>% gather(key = sep, value = "scores",3:6) 
    # chip_1$sep <- ifelse(chip_1$sep == "food_score","Food",
    #                      ifelse(chip_1$sep == "amb_score","Ambience",
    #                             ifelse(chip_1$sep == "service_score","Service","Price")))
    
    
    
    ggplotly(             ggplot(chip_filtered_sep_scores)+
                            geom_bar(aes(sep,filter_score,fill = sep),stat = "identity")+
                            geom_point(aes(sep,average_score,),col = "#E0607E")+
                            theme(axis.title.x=element_blank(),
                                  axis.title.y=element_blank(),
                                  axis.text.x=element_text(angle=60, hjust=1))+
                            guides(fill=guide_legend(title=""))+ggtitle("Sentiment Scores"))%>%  
      layout(legend = list(orientation = "h",
                           x = -0.1, y = -0.5,
                           font = list(size = 10),
                           xaxis = list(size = 5,title = ""),
                           yaxis = list(size = 5),title = "")
      )
    
  })
  
  output$bar_sep <- renderPlotly({
    validate(
      need(id(),"Please click on the map and choose one Chipotle restaurant.")
    )
    
    chip_word <- chip_sep() %>% select(-ends_with("score")) %>% 
      gather(word , sentiment_score, black:refund, factor_key=TRUE) %>% 
      filter(sentiment_score != 0) %>% 
      mutate(sep = ifelse(word %in% food_sep,
                          "Food",
                          ifelse(word %in% amb_sep,
                                 "Ambience",
                                 ifelse(word %in% service_sep,
                                        "Service",
                                        ifelse(word %in% price_sep, "Price",NA)))))
    
    
    chip_food_sub <- chip_word %>% filter(sep == "Food")
    yaxis_order <- chip_food_sub$word[chip_food_sub$sentiment_score %>% order()] %>% as.character()
    chip_food_sub$word <- factor(chip_food_sub$word,levels = yaxis_order)
    food_sub <- plot_ly(data=chip_food_sub,x = ~sentiment_score, y = ~word, type = 'bar', orientation = 'h',
                        marker = list(color = 'rgba(48, 104, 142, 0.6)',
                                      line = list(color = 'rgba(48, 104, 142, 1.0)',
                                                  width = 3)), name = "FOOD")
    
    chip_amb_sub <- chip_word %>% filter(sep == "Ambience")
    yaxis_order <- chip_amb_sub$word[chip_amb_sub$sentiment_score %>% order()] %>% as.character()
    chip_amb_sub$word <- factor(chip_amb_sub$word,levels = yaxis_order)
    amb_sub <- plot_ly(data=chip_amb_sub,x = ~sentiment_score, y = ~word, type = 'bar', orientation = 'h',
                       marker = list(color = 'rgba(68, 13, 84, 0.6)',
                                     line = list(color = 'rgba(68, 13, 84, 1.0)',
                                                 width = 3)), name = "Ambience")
    
    chip_service_sub <- chip_word %>% filter(sep == "Service")
    yaxis_order <- chip_service_sub$word[chip_service_sub$sentiment_score %>% order()] %>% as.character()
    chip_service_sub$word <- factor(chip_service_sub$word,levels = yaxis_order)
    service_sub <- plot_ly(data=chip_service_sub,x = ~sentiment_score, y = ~word, type = 'bar', orientation = 'h',
                           marker = list(color = 'rgba(253, 231, 37, 0.6)',
                                         line = list(color = 'rgba(253, 231, 37, 1.0)',
                                                     width = 3)),name = "Service")
    
    chip_price_sub <- chip_word %>% filter(sep == "Price")
    yaxis_order <- chip_price_sub$word[chip_price_sub$sentiment_score %>% order()] %>% as.character()
    chip_price_sub$word <- factor(chip_price_sub$word,levels = yaxis_order)
    price_sub <- plot_ly(data=chip_price_sub,x = ~sentiment_score, y = ~word, type = 'bar', orientation = 'h',
                         marker = list(color = 'rgba(53, 183, 121, 0.6)',
                                       line = list(color = 'rgba(53, 183, 121, 1.0)',
                                                   width = 3)),name = "Price")
    subplot(food_sub,amb_sub,service_sub,price_sub,nrows = 2)
  })
  
  
  # selected_word -----------------------------------------------------------
  
  output$selected_word <- renderPrint({
    event_data("plotly_click")$y
  })
  
  output$related_word <- renderText({
    validate(
      need(event_data("plotly_click")$y,"Please click on the bars above to choose one word.")
    )
    paste0('<font size="5"><span style=\"color:#008ECE\"><b><u>Selected:  ',event_data("plotly_click")$y,'</u></b></span></font>')
  })
  
  output$claim <- renderText({
    '<font size="4"><span style=\"color:#0AA398\">The "Randomize!" button will randomly show relevant reviews <b><u>if there is more than one review.</u></b><br>If the review does not change, it means that there are no more review.</span></font>'
  })
  output$claim2 <- renderText({
    '<font size="3">The red points and dash line in the following plots are the <b>AVERAGE of overall Chipotles</b> in the selected time period.</font>'
  })
  
  
  output$related_review_positive <- renderText({
    a <- input$randomize
    validate(
      need(event_data("plotly_click")$y,"")
    )
    selected_word <- event_data("plotly_click")$y
    sep_sign <- sign(event_data("plotly_click")$x)
    selected_word_score <- review_sep %>% 
      filter(business_id == id()) %>% 
      select_(selected_word)
    
    
    selected_date <- review_sep %>% 
      filter(business_id == id() )%>% 
      .[which(sign(selected_word_score) == sign(1)),"date"]
    
    selected_review <- chip_review %>% 
      filter(business_id == id() & date %in% selected_date) %>%
      select(text) %>% 
      unlist()
    
    selected_review1 <- selected_review[sample(1:length(selected_review),1,replace = T)]
    for(word in terms){
      selected_review1 <- selected_review1%>% str_replace_all(regex(word,ignore_case = T),paste0('<font size="3"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\">',word,'</span></font>'))
    }
    selected_review1 <- selected_review1%>% str_replace_all(regex(paste0('<font size="3"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\">',selected_word,'</span></font>'),ignore_case = T),paste0('<font size="5"><span style=\"color:',brewer.pal(8,"Spectral")[1],'\"><b>',selected_word,'</b></span></font>'))
    
    if(length(selected_review1) == 0){
      paste0('<font size="5"><span style=\"color:#E0607E\"><b>No Positive Reviews about this word! </b></span></font>')
    }else{if(is.na(selected_review1)|length(selected_review1) == 0){
      paste0('<font size="5"><span style=\"color:#E0607E\"><b>No Positive Reviews about this word! </b></span></font>')
    }else{
      paste0('<font size="5"><span style=\"color:#008ECE\"><b>Positive:</span></font>',selected_review1)}}
    
    
    
  })
  
  output$related_review_negative <- renderText({
    a <- input$randomize
    validate(
      need(event_data("plotly_click")$y,"")
    )
    selected_word <- event_data("plotly_click")$y
    selected_word_score <- review_sep %>% 
      filter(business_id == id()) %>% 
      select_(selected_word)
    
    
    selected_date <- review_sep %>% 
      filter(business_id == id() )%>% 
      .[which(sign(selected_word_score) == sign(-1)),"date"]
    
    selected_review <- chip_review %>% 
      filter(business_id == id() & date %in% selected_date) %>%
      select(text) %>% 
      unlist()
    
    selected_review1 <- selected_review[sample(1:length(selected_review),1,replace = T)]
    for(word in terms){
      selected_review1 <- selected_review1%>% str_replace_all(regex(word,ignore_case = T),paste0('<font size="3"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\">',word,'</span></font>'))
    }
    selected_review1 <- selected_review1%>% str_replace_all(regex(paste0('<font size="3"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\">',selected_word,'</span></font>'),ignore_case = T),paste0('<font size="5"><span style=\"color:',brewer.pal(8,"Spectral")[1],'\"><b>',selected_word,'</b></span></font>'))
    
    
    if(length(selected_review1) == 0){
      paste0('<font size="5"><span style=\"color:#008ECE\"><b>No Negative Reviews about this word! </b></span></font>')
    }else{if(is.na(selected_review1)|length(selected_review1) == 0){
      paste0('<font size="5"><span style=\"color:#008ECE\"><b>No Negative Reviews about this word! </b></span></font>')
    }else{
      paste0('<font size="5"><span style=\"color:#E0607E\"><b>Negative:<b> </span></font>',selected_review1)}}
    
    
    
  })
  
  
  
  output$food_advice <- renderText({
    validate(
      need(id(),"")
    )
    paste0('<font size="6"><span style=\"color:#31678D\"><b><u>FOOD</u></b></span></font>',
           '<font size="3"><br><b>',get_review(chip,id())[1],'</b></font>')
  })
  output$amb_advice <- renderText({
    validate(
      need(id(),"Please click on the map and choose one Chipotle restaurant.")
    )
    paste0('<font size="6"><span style=\"color:#440154\"><b><u>AMBIENCE</u></b></span></font>',
           '<font size="3"><br><b>',get_review(chip,id())[4],'</b></font>')
  })
  output$service_advice <- renderText({
    validate(
      need(id(),"")
    )
    paste0('<font size="6"><span style=\"color:#F9E324\"><b><u>SERVICE</u></b></span></font>',
           '<font size="3"><br><b>',get_review(chip,id())[2],'</b></font>')
  })
  output$price_advice <- renderText({
    validate(
      need(id(),"")
    )
    paste0('<font size="6"><span style=\"color:#30A66E\"><b><u>PRICE</u></b></span></font>',
           '<font size="3"><br><b>',get_review(chip,id())[3],'</b></font>')
  })
}


