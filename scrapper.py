#모듈 설치
import pandas as pd
import numpy as np
import time
from selenium import webdriver
import openpyxl

driver = webdriver.Chrome("chromedriver.exe")
driver.get("https://comic.naver.com/webtoon/detail?titleId=774868&no=1")

comment = pd.DataFrame(data=[], columns=['회차', '이름', '아이디', '댓글', '좋아요', '싫어요'])
def scrapper (driver, comment, k):
    driver = webdriver.Chrome("chromedriver.exe")
    url = 'https://comic.naver.com/webtoon/detail?titleId=774868&no='+str(k)
    driver.get(url)
    time.sleep(1)
    driver.switch_to.frame('commentIframe')

    number = k
    name = driver.find_elements_by_css_selector('.u_cbox_nick')
    id_ = driver.find_elements_by_css_selector('.u_cbox_id')
    review = driver.find_elements_by_css_selector('.u_cbox_text_wrap')
    like = driver.find_elements_by_css_selector('.u_cbox_cnt_recomm')
    dislike = driver.find_elements_by_css_selector('.u_cbox_cnt_unrecomm')

    for i in range (len(id_)):
        data_comment = []
        data_comment.append(number)
        data_comment.append(name[i].text)
        data_comment.append(id_[i].text)
        data_comment.append(review[i].text)
        data_comment.append(like[i].text)
        data_comment.append(dislike[i].text)

        data_comment = pd.DataFrame(data=[data_comment], columns=comment.columns)
        comment = pd.concat([comment, data_comment])
        
    driver.close()
    print(str(number) + '화 완료')
    return comment

for k in range(1, 15):
    comment = scrapper(driver, comment, k)

comment.to_csv('webtoon_comments.csv', index=False)
comment.to_excel('webtoon_comments.xlsx', index=False)