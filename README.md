# *Semantic_Analysis*

4학년 2학기 빅데이터프로그래밍 수업에서 파일럿 프로젝트로 진행했던 **의미망 구축 개인 프로젝트**입니다. 프로젝트의 주제는 **현대인들의 결혼·연애관**으로, 대상 데이터는 네이버 웹툰 **미혼남녀의 효율적 만남의 댓글**로 선정하였습니다. 

웹툰의 소개글에서 알 수 있듯 '사랑으로는 부족하고 조건으로 아쉬운 미혼남녀의 만남'을 주제로, 나이도 20대 후반에서 30대 초반의 비교적 평범하게 여겨지는 직장인 여성의 이야기가 전개됩니다. 하지만 이 웹툰 내용 자체를 대상으로 삼지 않은 이유는, 웹툰의 특성 상 작가의 생각이 많은 부분 투영되고, 여러 사람의 여론을 반영하기에 한계가 있기 때문입니다. 따라서 웹툰이 공개된 뒤에 많은 사람들이 의견을 내놓을 수 있는 의견의 장이 열리고, **양방향 소통이 가능한 웹툰 댓글을 분석 데이터**로 삼았습니다.

## 📌 데이터 수집 방법
- ![Static Badge](https://img.shields.io/badge/Python-%230000FF) **웹 스크래퍼(크롤링)**
   * pandas, numpy, selenium, webdriver, openpyxl 패키지 활용
   * 개발자 도구(F12)의 태그 확인. <회차, 이름, 아이디, 댓글, 좋아요, 싫어요> 추출

![화면 캡처 2023-07-03 235059](https://github.com/SemiKwon/Semantic_Analysis/assets/76101347/f72f2690-343b-4a63-9ac3-df49ac2e6e4e)

**<미혼남녀의 효율적 만남> 20화까지 베스트 댓글 15개를 수집, 데이터프레임화 및 csv 파일저장**

## 📌 데이터 분석 방법
![Static Badge](https://img.shields.io/badge/R-%23FF0000) **의미망 네트워크 구축**

**1) 텍스트 전처리 (stringr, textclean, tidytext)**
   - 불필요한 문자 제거하기
   - 연속된 공백 제거하기
   - 품사 기준 토큰화하기


**2) 텍스트 형태소 토큰화 (KoNLP)**
   - 한나눔 형태소 분석기 기반 22개 품사 세부 분류 : 명사, 형용사, 동사 등 추출 
  
