from bs4 import BeautifulSoup
from datetime import datetime
from selenium import webdriver
import time
import sys
import os
import general_func

def try_crawl(url):
    try:
        #driver = webdriver.Chrome()
        driver = webdriver.PhantomJS('D:\\Tools\\phantomjs\\bin\\phantomjs.exe')
        driver.get(url)
        time.sleep(1)
        htmlSource = driver.page_source
        driver.close()
        driver.quit()

        data = crawl_detail(htmlSource)
        return data
    except:
        return None


def crawl_detail(html_content):
    flights = []
    soup_content = BeautifulSoup(html_content, "html.parser")
    score_main = soup_content.find('div', {'id': 'main'}).find('div', {'id': 'body'})
    info_section = score_main.find('center').find('center').find('tbody')
    info_selections = info_section.find_all('tr')
    print len(info_selections)
    cnt = 0
    for section in info_selections:
        cnt = cnt +1
        if cnt < 4:
            continue
        items = section.find_all('td')
        if len(items) < 12:
            continue

        tw = items[0].find('font').text.strip()
        lw = items[1].find('font').text.strip()
        title = items[2].find('font').text.strip()
        studio = items[3].find('font').text.strip()
        weekly_gross = items[4].find('font').text.strip()
        per_change = items[5].find('font').text.strip()
        cnt_change = items[6].find('font').text.strip()
        average = items[8].find('font').text.strip()
        total_gross = items[9].find('font').text.strip()
        budget = items[10].find('font').text.strip()
        week = items[11].find('font').text.strip()

        flights.append({'tw': tw,'lw':lw, 'title':title, 'studio':studio,
                        'weekly_gross': weekly_gross,'per_change':per_change, 'cnt_change':cnt_change,
                        'average':average,'total_gross':total_gross,'budget':budget,'week':week});

    return flights

if __name__ == '__main__':
    reload(sys)
    sys.setdefaultencoding('utf8')

    year = 2016
    outputDir = 'D:\\SummerHit\\Resources\\';
    if not os.path.exists(outputDir):
        os.mkdir(outputDir)
    data_fw=open(os.path.join(outputDir,  'box-'+ str(year)+'.txt'), 'w')
    data_fw.write('tw'+'\t'+'lw'+'\t'+'title'+'\t'+'studio'+'\t'+'weekly_gross'+'\t'+'per_change'+'\t'+'cnt_change'+'\t'+'average'+'\t'+'total_gross'+'\t'+'budget'+'\t'+'week' + '\n')
    for month in range(1, 25):
        if(month <10):
            monthStr = '0'+str(month)
        else:
            monthStr = str(month)
        url = 'http://www.boxofficemojo.com/weekly/chart/?yr='+str(year)+'&wk='+monthStr +'&p=.htm'
        print url

        data = try_crawl(url)
        if(data is not None):
            for item in data:
                data_fw.write(item["tw"]  + "\t" + item["lw"] + "\t" +item["title"] + "\t" +item["studio"]  + "\t")
                data_fw.write(item["weekly_gross"]  + "\t" + item["per_change"] + "\t" +item["cnt_change"]  + "\t")
                data_fw.write(item["average"]  + "\t" + item["total_gross"] + "\t" +item["budget"] + "\t" +item["week"] + "\n")

    data_fw.close()
    print "finished"
