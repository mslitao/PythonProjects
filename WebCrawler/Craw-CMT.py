from bs4 import BeautifulSoup
from datetime import datetime
from selenium import webdriver
import time
import re
import sys
import os
import general_func


def try_crawl(id):
    try:
        if id == '':
            return None

        url = 'http://www.cmt.com/cmt-music-awards/'+id+ '.jhtml'
        htmlSource = general_func.url_open(url, from_encoding='utf-8')

        data = crawl_detail(htmlSource)
        return data
    except:
        return None

def crawl_detail(html_content):
    awards_results = []
    soup_content = BeautifulSoup(html_content, "html.parser")

    awards_main = soup_content.find('div', {'id': 'winners'})
    winners_section = awards_main.find('div', {'id': 'p2_a'}).find('div', {'class': 'p2_wrap'})
    nominees_section = awards_main.find('div', {'id': 'p2_b'}).find('div', {'class': 'p2_wrap'})

    winners_info = winners_section.find_all('div', {'class': 'promo_item'})

    for winner_detail in winners_info:
        winner = {}
        category = winner_detail.find('h3').text.strip()
        content = winner_detail.find('li').text.strip()
        items = content.split("Video Title:\n\n")
        winner['Category'] = category
        winner['Artist'] = items[0].strip('"').strip()
        winner['Winner'] = '1'
        if len(items) == 2:
            video = items[1].strip('"').strip().replace('\n',';').replace('"','').strip("'")
            video = re.sub(r'\((.+)\)', '', video)
            winner['Video'] = video.strip()
        else:
            winner['Video'] = ''
        awards_results.append(winner)

    nominees_info = nominees_section.find_all('div', {'class': 'promo_item'})
    for nominee_info in nominees_info:
        category = nominee_info.find('h3').text.strip()

        nominees_detail = nominee_info.find_all('li')
        for nominee_detail in nominees_detail:
            nominee = {}
            content = nominee_detail.text.strip()
            items = content.split("-")
            nominee['Category'] = category
            nominee['Artist'] = items[0].strip('"').strip().replace('\n',';').replace('"','').strip("'")
            nominee['Winner'] = '0'
            if len(items) == 2:
                video = items[0].strip('"').strip().replace('\n',';').replace('"','').strip("'")
                video = re.sub(r'\((.+)\)', '', video)
                nominee['Video'] = video.strip()
                nominee['Artist'] = items[1].strip('"').strip().replace('\n',';').replace('"','').strip("'")
            else:
                nominee['Video'] = ''
            awards_results.append(nominee)

    return awards_results

if __name__ == '__main__':
    reload(sys)
    sys.setdefaultencoding('utf8')

    outputDir = 'D:\\BingPrediction\\CMA-2016\\data\\CMT\\';
    if not os.path.exists(outputDir):
        os.mkdir(outputDir)
    year = sys.argv[1]
    print year

    data_fw=open(os.path.join(outputDir,  'CMT-'+ str(year) +'-v20161010.txt'), 'w')

    data = try_crawl(year)

    if(data is not None):
        for item in data:
            data_fw.write(item["Category"] + "\t" + item["Artist"] + "\t" + item["Video"] + "\t" + item["Winner"])
            data_fw.write("\n")
    else:
        print year
    data_fw.close()
    print 'finished'
