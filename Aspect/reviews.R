chip_score = read.csv("C:/Users/89238/Documents/向兰心/UW-Madison/2/STAT 628/STAT 628 Module 3/code/chip_score.csv")
View(chip_score)
sum <- summary(chip_score[,3:112])
sum

get_review <- function(chip_score,k){
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
    
    if(comp1[1]){overall_food <- paste0("Your FOOD SCORE is lower than average. ")}
    else if(comp2[1]){overall_food <- paste0("Your FOOD SCORE is better than 75% of other chain stores. ")}
    else{overall_food <- paste0("Your FOOD SCORE is slightly better than average store. ")}
    
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
      posd <- paste0("Customers enjoy ",dish_p,". ")
    }
    if(dish_n != ""){
      dish_n <- str_remove(dish_n,"[,][ ]$")
      negd <- paste0("The followings are not as satisfying: ",dish_n,". ")
    }
    
    #fresh
    fresh = ""
    if(chip_score[k,"fresh"]!=0){
      if(comp1[,"fresh"] || comp1[,"stale"]){fresh <- "Some customers complain that the ingredients are not fresh. "}
      if(comp2[,"fresh"] && comp2[,"stale"]){fresh <- "The ingredients in your store are fresh. "}
    }  
    
    # taste
    
    taste = ""
    if(chip_score[k,"taste"]!=0){
      if(comp1[,"taste"]){fresh <- "As for taste, your store gets less scores than the average. "}
      if(comp2[,"taste"]){fresh <- "Your store gets high score on taste. "}
    }  
    if(chip_score[k,"salty"]!=0){
      taste = "The foods you offer are some times salty. "
    }
    
    sugg_food <-paste0(overall_food,posd,negd,fresh,taste)
  
  
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
    if(comp1[3]){overall_serv <- paste0("Your SERVICE SCORE is lower than average. ")}
    else if(comp2[3]){overall_serv <- paste0("Your SERVICE SCORE is better than 75% of other chain stores. ")}
    else{overall_serv <- paste0("Your SERVICE SCORE is slightly better than average score. ")}
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
  if (n>0 && wait>=n/2) {speed <- "The serving speed should be improved. "}
  
  
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
  if (n>0 && staff>=n/2) {att <- "And customers want better service from the working staffs. "}
  if(staff<0){att <- "And customers think your working staffs are doing well. "}
  
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
  if (n>0 && online>=n/2) {ol <- "Paying more attention on online services, including ordering with app and phone call, pickup, deliver services may help you attract more customers. "}
  if(online<0){ol <- "Online services, including ordering with app and phone call, pickup, deliver services are your highlights. "}
  
  
  sugg_serv <- paste0(overall_serv,speed,att,ol)
  
  
  ##########################################################
  #########################  PRICE  ########################
  ##########################################################
  
  price <- item[103:110]
  overall_price = ""
  if(chip_score$price_score[k]!=0){
    if(comp1[4]){overall_price <- paste0("Your PRICE SCORE is lower than average. ")}
    else if(comp2[4]){overall_price <- paste0("Your PRICE SCORE is better than 75% of other chain stores. ")}
    else{overall_price <- paste0("Your PRICE SCORE is slightly better than average score. ")}
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
  if (n>0 && charge>=n/2) {pc <- "Higher cost performance are expected. "}
  if(charge<0) {pc <- "Customers think the food really worth its price. "}
  
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
    if(comp1[2]){overall_amb <- paste0("Your AMBIANCE SCORE is lower than average. ")}
    else if(comp2[2]){overall_amb <- paste0("Your AMBIANCE SCORE is better than 75% of other chain stores. ")}
    else{overall_amb <- paste0("Your AMBIANCE SCORE is slightly better than average score. ")}
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
  if (n>0 && clean>=n/2) {cl <- "Sanitary condition needs improvement. "}
  if(clean<0) {cl <- "The sanitary condition is good. "}
  
  park = ""
  if(chip_score[k,"parking"]!=0){
      if(comp1[,i]){park = "Customers may find it inconvenient to park around your restaurant. "}
  }
  
  sugg_amb <- paste0(overall_amb,cl,park)
  return(c(sugg_food,sugg_serv,sugg_price,sugg_amb))
  
}

get_review(chip_score,84)

