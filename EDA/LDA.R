library(magrittr)
term_table <- chipotle %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  .$word %>% 
  table %>% 
  sort(decreasing = T)
delete_index <-  str_detect(names(term_table),"\\d") | term_table < 6 
term_table <- term_table[!delete_index] #去掉不符合要求的
term_table[1:10]
dictionary <- names(term_table) #创建词库
dictionary%<>%tolower
dictionary%<>%unique



document.list <- map(chipotle$text,str_split," ")  %>%  
  map(str_extract_all,regex("[a-z']{2,}",ignore_case = T)) %>% 
  map(1) %>% 
  map(tolower) %>% 
  map(str_remove_all, regex(str_c(str_c("^",stop_words$word,"$"),collapse = "|"),ignore_case = T))

document.list <- document.list %>% map(function(x){return(x[!str_detect(x,"^$")])})
map(chipotle$text,str_split," ")  %>%  
  map(str_extract_all,regex("[a-z']{2,}",ignore_case = T)) %>% 
  map(1) %>% 
  map(tolower) 

get.index <- function(x) {
  index <- match(x, dictionary) 
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- map(document.list, get.index)


get.index <- function(x) {
  index <- match(x, dictionary) 
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- map(document.list, get.index)



# LDA参数
iter <- 4000 
alpha <- 0.10 
eta <- 0.10
set.seed(1) 
dtm
library(lda)
# 计算coherence，选择最好的话题数量
coherence <- c()
for( Topic_num in 1:5){
  fit <- lda.collapsed.gibbs.sampler(documents = documents, K = Topic_num, vocab = dictionary, num.iterations = 2000, alpha = alpha, eta = eta, burnin = 1000, compute.log.likelihood = TRUE)
  
  
  coherence[Topic_num] <- CalcProbCoherence(fit$topics,dtm%>%as.matrix())%>%mean
  
  cat(Topic_num,"-")
  
}

ggplot()+geom_line(aes(x = 5:21,y = coherence[5:21]))+
  xlab("话题数量")+
  ylab("mean coherence")+
  theme(text = element_text(family='Kai'))+
  ggtitle("话题数量与coherence折线图")



# LDA建模
library(lda) 
# 参数
topic_num <- 4
iter<- 4000 
alpha <- 0.1
eta <- 0.1
seed <- 1
# 模型建立
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = topic_num, vocab = dictionary, num.iterations = iter, alpha = alpha, eta = eta, burnin = 1000, compute.log.likelihood = T)
library(LDAvis)
# LDAvis需要的参数
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x))) 
term.frequency <- as.integer(term_table)
document_length <- map_dbl(documents, function(x) sum(x[2, ]))
# LDAvis会给出一个文件夹，里面包含了可以上传到服务器上的LDA交互性可视化网页
json <- createJSON(phi = phi, theta = theta, 
                   doc.length = document_length, vocab = dictionary,
                   term.frequency = term.frequency)
# 运行下面的代码可以直接在网页上查看LDA交互性可视化网页
serVis(json, out.dir = './vis_brief', open.browser = T)

my_env <- "./env"
reticulate::use_virtualenv(my_env)

corpus <- chipotle %>% select(review_id,text) %>% rename(id = review_id)
docs <- prepare_documents(corpus,"text")
#> → Preprocessing 9 documents
#> ← 9 documents after perprocessing
dict <- corpora_dictionary(docs)
corpora <- doc2bow(dict, docs)

# lda model
model <- model_lda(
  corpus = corpora,
  id2word = dict,
  iterations = 50L,
  num_topics = 2L
)

# visualise
vis <- prepare_ldavis(model, corpora, dict)
# NOT RUN {
save_ldavis_html(vis, "lda.html")
# }