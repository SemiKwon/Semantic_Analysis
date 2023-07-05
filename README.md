# *Semantic_Analysis*

4학년 2학기 빅데이터프로그래밍 수업에서 파일럿 프로젝트로 진행했던 **네트워크 그래프 구축 및 파이 계수 막대그래프 제작, 데이터 분석 시각화 개인 프로젝트**입니다. 프로젝트의 주제는 **현대인들의 결혼·연애관**으로, 대상 데이터는 네이버 웹툰 **미혼남녀의 효율적 만남의 댓글**로 선정하였습니다. 

웹툰의 소개글에서 알 수 있듯 '사랑으로는 부족하고 조건으로 아쉬운 미혼남녀의 만남'을 주제로, 나이도 20대 후반에서 30대 초반의 비교적 평범하게 여겨지는 직장인 여성의 이야기가 전개됩니다. 하지만 이 웹툰 내용 자체를 대상으로 삼지 않은 이유는, 웹툰의 특성 상 작가의 생각이 많은 부분 투영되고, 여러 사람의 여론을 반영하기에 한계가 있기 때문입니다. 따라서 웹툰이 공개된 뒤에 많은 사람들이 의견을 내놓을 수 있는 의견의 장이 열리고, **양방향 소통이 가능한 웹툰 댓글을 분석 데이터**로 삼았습니다.

## 📌 데이터 수집 방법
- ![Static Badge](https://img.shields.io/badge/Python-%230000FF) **웹 스크래퍼(크롤링)**
   * pandas, numpy, selenium, webdriver, openpyxl 패키지 활용
   * 개발자 도구(F12)의 태그 확인. <회차, 이름, 아이디, 댓글, 좋아요, 싫어요> 추출

![화면 캡처 2023-07-03 235059](https://github.com/SemiKwon/Semantic_Analysis/assets/76101347/f72f2690-343b-4a63-9ac3-df49ac2e6e4e)

**<미혼남녀의 효율적 만남> 20화까지 베스트 댓글 15개를 수집, 데이터프레임화 및 csv 파일저장**

## 📌 데이터 분석 방법
![Static Badge](https://img.shields.io/badge/R-%23FF0000) **의미망 네트워크 구축**

**(1) 텍스트 전처리 (stringr, textclean, tidytext)**
   - 불필요한 문자 제거하기
   - 연속된 공백 제거하기
   - 품사 기준 토큰화하기

**(2) 텍스트 형태소 토큰화 및 추출 (KoNLP)**
   - 한나눔 형태소 분석기 기반 22개 품사 세부 분류 : 명사, [형용사/동사] 추출
   - bind_rows() : 명사 + [형용사/동사]

**(3) 단어 동시 출현 빈도**
   - 두 단어가 함께 몇 번씩 사용되었는지 확인
   - 한 단어를 기준으로 함께 사용된 모든 단어의 빈도 파악

**(4) 단어 동시 출현 빈도 데이터 네트워크 그래프로 변환**
   - pair에서 3회 이상 사용된 단어만 추출

**(5) 연결중심성 커뮤니티 변수 추가**
   - 연결중심성 : 노드(단어)가 다른 노드(단어)와 얼마나 밀접한 관계인지 나타낸 값
   - 커뮤니티 : 노드(단어) 간 관계가 가까워 밀접하게 연결된 노드 집단 (같은 색으로 표현하면 커뮤니티 구조를 이해할 수 있음.)
***
![Static Badge](https://img.shields.io/badge/R-%23FF0000) **단어 간 상관 분석(Phi coefficient)**


**파이계수**는 두 단어가 쌍으로 사용되는 경우가 하나 하나 따로 사용되는 경우에 비해 얼마나 많은지 나타낸 지표. 범위는 -1~1로, 1에 가까울수록 쌍으로 사용되기에 관련성이 크고, -1에 가까울수록 쌍으로 사용되는 경우가 적어 관련성도 작다는 의미

**(1) 파이계수 구하기**
   - (3)의 단어 동시 출현 빈도 데이터를 통해 파이 계수 연산

**(2) 관심 단어 별 파이 계수 큰 단어 추출**
   - **외모, 현실, 자존감, 연애, 결혼, 호감** 등 현대인들의 결혼·연애관을 엿볼 수 있는 단어들을 기준으로 추출

**(3) 막대 그래프 제작**
   - 결과를 막대 그래프로 시각화

## 📌 데이터 분석 결과

![3](https://github.com/SemiKwon/Semantic_Analysis/assets/76101347/7230a018-9bef-4ab8-af8a-840b3671c32c)

18개의 커뮤니티로 분류된 데이터셋에서 주목할 만한 노드는 6번 커뮤니티에 해당하는 '사랑'
6번 커뮤니티 다음으로 2, 4, 11, 12 커뮤니티에서 연결 중심성이 높은 것을 확인할 수 있음.

* 2 : 부자, 진짜, 조건, 현실
* 4 : 생각, 사다, 들다, 안하다
* 11 : 없다, 모르다
* 12 : 그렇다, 싫다

'없다, 모르다, 안하다, 싫다' 등의 형용사만 보더라도 현대인들이 결혼에 대해 나누는 담론이 긍정적이지 않은 것을 알 수 있음. 또 '조건, 현실, 부자'를 통해 '결혼은 조건이다, 결혼은 현실이다.'와 같은 표현이 현실에도 통용되는 의식을 엿볼 수 있었고, 결혼과 재력의 연관성에 대해 생각하는 사람들이 있음을 살필 수 있었음. 

---

![Phi_Coefficient](https://github.com/SemiKwon/Semantic_Analysis/assets/76101347/99dbd275-7d96-4c81-abe2-08f596041037)

외모, 현실, 자존감, 연애, 결혼, 호감 6개의 단어의 파이계수

몇몇 단어들에서는 파이계수가 0.5가 넘지 않는 것을 관찰할 수 있었음. 따라서 완벽한 당위성을 부여하기에는 어려운 면이 있었지만, 여기에서도 부모, 독립, 집안 등의 현실적인 단어들이 쌍으로 등장하는 것을 알 수 있었음. 또 긍정을 의미하는 단어보다는 부정성을 띠는 단어들이 높은 것으로 확인되었음.

- **현대인들의 연애** : 현실적으로 서로의 조건을 확인하며 개개인이 어울리는지에 대해 검증되는 사람을 이상형으로 삼고자 함.
- **현대인들의 결혼관** : 독립, 집안, 클래스 등에서 결혼은 개인이 아닌 집안과 집안의 만남이라고 생각하는 경향성을 읽어낼 수 있었음.

## 📌 보완점
* 데이터 전처리 시, OKT나 Komoran 등의 형태소 분석기를 사용하지 않아, 비교적 정확도가 떨어진 부분이 아쉽게 느껴졌습니다.
* 촉박한 시간으로 인해 데이터 전처리를 패키지에 의존했다는 점이 좀 아쉬웠습니다. 다음에는 제 기준에 따라 R에 내장되어 있는 함수를 활용하여 전처리해보고자 합니다.
