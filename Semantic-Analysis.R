#KoNLP 설치 및 확인 - R 4.3.1 버전(R 버전 4만 넘으면 모두 가능한 듯)
# 참고 : https://www.youtube.com/watch?v=Ewy8mEUriJg
# https://lime-jelly.tistory.com/entry/R%EC%97%90%EC%84%9C-%ED%8C%A8%ED%82%A4%EC%A7%80-%EC%84%A4%EC%B9%98%EC%98%A4%EB%A5%98-%ED%95%B4%EA%B2%B0-%EB%B0%A9%EB%B2%95
install.packages("KoNLP")

install.packages("rJava")
install.packages("stringr")
install.packages("hash")
install.packages("tau")
install.packages("Sejong")
install.packages("RSQLite")
install.packages("devtools")

#KoNLP 설치 및 확인 
library(KoNLP)
extractNoun('부산에서 돼지 국밥은 너무 맛있었다.')

###

#필요한 패키지 설치
library(dplyr)
library(stringr) 
library(textclean) 
library(tidytext) 
library(KoNLP) 
library(tidyr) 
library(widyr) 
library(tidygraph) 
library(ggraph) 
library(ggplot2)

#CSV 파일 불러오기 
comments <- read.csv("webtoon_comment.csv", fileEncoding ="euc-kr" )
head(comments) #확인

#데이터 전처리
webtoon_comment <- comments %>%
  select(댓글) %>%
  mutate(댓글 = str_replace_all(댓글, "[^가-힣]", " "), 댓글 = str_squish(댓글), 아이디 = row_number())
head(webtoon_comment) #확인

#품사 기준으로 토큰화하기
comment_pos <- webtoon_comment %>%
  unnest_tokens(input = 댓글, output = word, token = SimplePos22, drop = F)
head(comment_pos) #확인

#품사를 분리하여 행 구성하기
comment_pos <- comment_pos %>%
  separate_rows(word, sep = "[+]")
comment_pos %>%
  select(word, 댓글)

#품사(명사, 형용사, 동사 등) 추출하기
noun <- comment_pos %>% #명사
  filter (str_detect(word, "/n")) %>%
  mutate (word=str_remove(word, "/.*$"))
head(noun) #확인

noun %>%
  select(word, 댓글) %>%
  count(word, sort=T)

pvpa <- comment_pos %>% #동사, 형용사
  filter(str_detect(word, "/pv|/pa")) %>%
  mutate (word=str_replace(word, "/.*$", "다"))

pvpa %>%
  select(word, 댓글) %>%
  count(word, sort=T)
head(pvpa) #확인

#noun과 pvpa를 결합한 뒤, 단어 의미를 이해할 수 있도록 두 글자 이상의 단어만 남기기
bind_comment <- bind_rows(noun, pvpa) %>%
  filter(str_count(word)>=2)%>%
  arrange(아이디)%>%
  select(word, 댓글, 아이디)
head(bind_comment) #확인

#단어쌍 빈도 구하기
pair <- bind_comment %>%
  pairwise_count(item = word, feature = 아이디, sort = T)
head(pair) #확인

#네트워크 만들기
graph_comment <- pair %>%
  filter(n >= 3) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality=centrality_degree(),
         group=as.factor(group_infomap()))

#네트워크에 연결 중심성, 커뮤니티 표현하기
set.seed(1234)
ggraph(graph_comment, layout="fr")+
  geom_edge_link(color="gray50", alpha=0.5)+
  geom_node_point(aes(size=centrality,
                      color=group),
                  show.legend=F)+
  scale_size(range=c(5, 15))+
  geom_node_text(aes(label=name),
                 repel=T,
                 size=5,
                 family="namugothic")+
  theme_graph()

  
#파이 계수 구하기
word_cors <- bind_comment %>%
add_count(word) %>%
filter(n>=3) %>%
pairwise_cor(item=word, feature = 아이디, sort = T)

word_cors

#특정 단어와 관련성이 큰 단어 살펴보기
word_cors %>%
  filter(item1=="외모")

word_cors %>%
  filter(item2=="현실")

#파이 계수가 큰 단어 추출하기
target <- c("외모", "현실", "자존감", "연애", "결혼", "호감")
top_cors <- word_cors %>%
  filter(item2 %in% target) %>%
  group_by(item2) %>%
  slice_max(correlation, n=5)

#막대 그래프 만들기
top_cors$item2 <- factor(top_cors$item2, levels=target)
library(ggplot2)
ggplot(top_cors, aes(x=reorder_within(item1, correlation, item2), y = correlation, fill=item2))+geom_col(show.legend=F)+facet_wrap(~item2, scales="free")+coord_flip()+scale_x_reordered()+labs(x=NULL)
