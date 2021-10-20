#패키지 설치
install.packages("rvest")
library(rvest)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
install.packages("stringr")
library("stringr")


#뉴스 페이지 Url 작업
naver_url <- 'http://news.naver.com/main/list.nhn?sid2=230&sid1=105&mid=shm&mode=LS2D&date='

date <- 20210919:20211019
naver_url_1 <-'&page='
page <- 1:2 #페이지 번호

for(dt in date){
  for(page_num in page){
    naver_url2 <-paste0(naver_url,dt,naver_url_1,page_num)
    print(naver_url2)
  }
}

#기사 페이지 Url 작업
naver_url <- 'http://news.naver.com/main/list.nhn?sid2=230&sid1=105&mid=shm&mode=LS2D&date=20211019&page=1'
html <- read_html(naver_url)#html소스코드 추출
temp <- unique(html_nodes(html,'#main_content')%>% #태그에 포함된 소스코드/속성 추출
  html_nodes(css='.list_body')%>%
  html_nodes(css='.type06_headline')%>%
  html_nodes('a')%>%
  html_attr('href')) #속성 값 추출

#반복문 사용

naver_url_1 <- 'http://news.naver.com/main/list.nhn?sid2=230&sid1=105&mid=shm&mode=LS2D&date'
date <- 20211001:20211019
naver_url_2 <- '&page='
page <- 1:2

news_url <- c()
news_date <- c()

for(dt in date){
  for(page_num in page){
    naver_url <- paste0(naver_url_1, dt, naver_url2, page_num)
    html <- read_html(naver_url)
    temp <- unique(html_nodes(html, '#main_content')%>%
                     html_nodes(css='.list_body')%>%
                     html_nodes(css='.type06_headline')%>%
                     html_nodes('a')%>%
                     html_attr('href'))
  news_url <- c(news_url,temp)
  news_date <- c(news_date,rep(dt,length(temp)))
  print(c(dt,page_num))
    }
}

#기사 내용 크롤링

news_content <- c()

for(i in 1:length(news_url)){
  html <-read_html(news_url[i])
  temp <- repair_encoding(html_text(html_nodes(html,'#articleBodyContents')),from = 'utf-8')
  news_content <- c(news_content, temp)
}

news <- cbind(date=news_date,url=news_url,content=unlist(news_content))
news <- as.data.frame(news)

content_pre <- gsub("[\r\n\t]",' ', news_content)
content_pre <- gsub("[[:punct:]]", ' ', content_pre)
content_pre <- gsub("[[:cntrl:]]", ' ', content_pre)
content_pre <- gsub("\\d+", ' ', content_pre)
content_pre <- gsub("[a-z]", ' ', content_pre)
content_pre <- gsub("[A-Z]", ' ', content_pre)
content_pre <- gsub("\\s+", ' ', content_pre)
str_trim(content_pre)
content_pre[1]

write.csv(content_pre, 'newsAnalysis.csv', row.names = F)
           
install.packages("Matrix")
library("Matrix")



library_basic = c('dplyr','ggplot2','igraph')
library_textM = c('KoNLP','tm','Matrix','wordcloud')
library_visual = c('qgraph', 'rgl', 'scales', 'IRdisplay','scales','RColorBrewer')
library_association = c('arules', 'arulesViz')
library_times = c('tseries', 'TTR', 'forecast')                                
head(content_pre)

# R 버전 다운그레이드 패키지 설치 (github에서 다운로드)

install.packages("multilinguer")
library(multilinguer)
install_jdk()
install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")

install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))

install.packages("corpus")

library("KoNLP")
library("tm")
library("corpus")

#전처리 완료후 분석 시작

data_news <- read.csv('C:/Users/user/newsAnalysis.csv')
head(data_news)
str(data_news)

useSejongDic()

text = data_news[,1]
cps = Corpus(VectorSource(text))
dtm = tm::DocumentTermMatrix(cps,control = list(tokenize =extractNoun,removeNumber = T,removePunctuation = T))

str(dtm)  
install.packages("stringr")
library("stringr")
rmat <- as.matrix(dtm)
rmat <- spMatrix(dtm$nrow,dtm$ncol,i=dtm$i,j=dtm$j,x=dtm$v)
wcount<-colSums(rmat)
wname<-dtm$dimnames$Terms
install.packages("rvest")
library(rvest)
wname<- repair_encoding(dtm$dimnames$Terms)
#rvest

colnames(rmat) <- wname
sort.var <- sort(wcount, decreasing = T)[100]
idx <- !(grepl('IT', wname) | (wcount<=sort.var))
wname.rel <- wname[idx]
wcount.rel <- wcount[idx]

#RColorBrewer
install.packages("RColorBrewer")
library("RColorBrewer")
pal <- brewer.pal(8, "Set2")

result_wordcount <- table(content_pre)

pal <- brewer.pal(7,"Set1")

#wordcloud

install.packages("wordcloud")
library("wordcloud")
install.packages("tmap")
library("tmap")
wordcloud(wname.rel, freq = wcount.rel, rot.per=0.35, random.order=FALSE, colors=pal)

wordcloud(names(result_wordcount), freq=result_wordcount, scale=c(4,0.5), min.freq=12, random.order=F, rot.per=0.35, colors=pal, family="malgun")

