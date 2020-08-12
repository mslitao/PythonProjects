from bs4 import BeautifulSoup
from datetime import datetime
from selenium import webdriver
import time
import sys
import os
import general_func


def try_crawl(id):
    try:
        if id == '':
            return None

        url = 'http://www.acmawards.com/nominees'
        htmlSource = general_func.url_open(url, from_encoding='utf-8')

        data = crawl_detail(htmlSource)
        return data
    except:
        return None

def crawl_detail(html_content):
    awards_results = []
    soup_content = BeautifulSoup(html_content, "html.parser")

    awards_main = soup_content.find('div', {'id': 'nominee-acc'})

    categoryList = awards_main.find_all('a', {'class' : 'nominees-title'})

    awards_divs =[]
    index = 0
    for categoryDiv in categoryList:
        category = categoryDiv.text.strip().split("\n")[0]
        print category
        index += 1
        print index
        divs = awards_main.find_all('div', {'id' : 'nominee-acc-'+str(index)})
        if divs is not None:
            for div in divs:
                awards_divs.append(div)

        award_div = awards_divs[index-1]
        nominees_section = award_div.find_all('div', {'class': 'thumbnail-block'})

        is_winner = 1
        for nominee_detail in nominees_section:
            nominee = {}
            info_caption = nominee_detail.find('div', {'class': 'caption'})
            info1 = info_caption.find('span', {'class': 'h3'}).text.strip()
            info2 = info_caption.find('em')

            nominee['Category'] = category

            if info2 is not None:
                nominee['Video'] = info1.strip('"').strip().replace('\n',';').replace('"','').strip("'")
                nominee['Artist'] = info2.text.strip()
            else:
                nominee['Video'] = ''
                nominee['Artist'] = info1

            if is_winner == 1:
                nominee['Winner'] = '1'
                is_winner = 0
            else:
                nominee['Winner'] = '0'

            awards_results.append(nominee)

    return awards_results

if __name__ == '__main__':
    reload(sys)
    sys.setdefaultencoding('utf8')

    outputDir = 'D:\\BingPrediction\\CMA-2016\\data';
    if not os.path.exists(outputDir):
        os.mkdir(outputDir)
    year = '2016'

    data_fw=open(os.path.join(outputDir,  'ACM-'+ str(year) +'.txt'), 'w')

    data = try_crawl(year)

    if(data is not None):
        print data
        for item in data:
            data_fw.write(item['Category'] + "\t" + item['Artist'] + "\t" + item['Video'] + "\t" + item['Winner'])
            data_fw.write("\n")
    else:
        print year
    data_fw.close()
    print 'finished'
