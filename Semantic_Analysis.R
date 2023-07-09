library(readr)
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

# CSV 파일 불러오기
comments <- read.csv("webtoon_comment.csv")

# 데이터 전처리
webtoon_comment <- comments %>%
  select(댓글) %>%
  mutate(댓글 = str_replace_all(댓글, "[^가-힣]", " "), 댓글 = str_squish(댓글), 아이디 = row_number())

# 품사 기준으로 토큰화하기
comment_pos <- webtoon_comment %>%
  unnest_tokens(input = 댓글, output = word, token = SimplePos22, drop = F)

# 품사를 분리하여 행 구성하기
comment_pos <- comment_pos %>%
  separate_rows(word, sep = "[+]")

# 품사(명사, 형용사, 동사 등) 추출하기
noun <- comment_pos %>% # 명사
  filter(str_detect(word, "/n")) %>%
  mutate(word = str_remove(word, "/.*$"))

pvpa <- comment_pos %>% # 동사, 형용사
  filter(str_detect(word, "/pv|/pa")) %>%
  mutate(word = str_replace(word, "/.*$", "다"))

# noun과 pvpa를 결합한 뒤, 단어 의미를 이해할 수 있도록 두 글자 이상의 단어만 남김
bind_comment <- bind_rows(noun, pvpa) %>%
  filter(str_count(word) >= 2) %>%
  arrange(아이디) %>%
  select(word, 댓글, 아이디)

# 단어쌍 빈도 구하기
pair <- bind_comment %>%
  pairwise_count(item = word, feature = 아이디, sort = T)

# 네트워크 그래프 만들기
graph_comment <- pair %>%
  filter(n >= 3) %>%
  as_tbl_graph(directed = F)

# 연결중심성 계산
centrality <- centrality_degree(graph_comment)

# 커뮤니티 할당
communities <- group_infomap(graph_comment)

# 데이터프레임에 연결중심성과 커뮤니티 정보 추가
node_data <- bind_comment %>%
  distinct(word, 아이디) %>%
  left_join(centrality, by = c("word" = "node")) %>%
  left_join(communities, by = c("word" = "name"))

# 네트워크 그래프 함수
word_networks <- function(x) {
  ggraph(x, layout = "fr") +
    geom_edge_link(color = "gray50", alpha = 0.5) +
    geom_node_point(aes(size = centrality, color = group), size = 4) +
    geom_node_text(aes(label = name), repel = T, size = 5) +
    theme_graph()
}

# 동시 출현 네트워크 출력
set.seed(1234)
word_networks(graph_comment)

# 파이 계수 구하기
word_cors <- bind_comment %>%
  add_count(word) %>%
  filter(n >= 3) %>%
  pairwise_cor(item = word, feature = 아이디, sort = T)

# 특정 단어와 관련성이 큰 단어 살펴보기
word_cors %>%
  filter(item1 == "외모")

word_cors %>%
  filter(item2 == "현실")

# 파이 계수가 큰 단어 추출하기
target <- c("외모", "현실", "자존감", "연애", "결혼", "호감")
top_cors <- word_cors %>%
  filter(item2 %in% target) %>%
  group_by(item2) %>%
  slice_max(correlation, n = 5)

# 막대 그래프 만들기
top_cors$item2 <- factor(top_cors$item2, levels = target)
ggplot(top_cors, aes(x = reorder_within(item1, correlation, item2), y = correlation, fill = item2)) +
  geom_col(show.legend = F) +
  facet_wrap(~item2, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL)