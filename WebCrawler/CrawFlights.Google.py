from bs4 import BeautifulSoup
from datetime import datetime
from selenium import webdriver
import time
import sys
import os

def try_crawl(url):
    try:

        driver = webdriver.Chrome()
        driver.get(url)
        time.sleep(5)
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
    score_main = soup_content.find('div', {'class': 'MHNSJI-wb-b MHNSJI-wb-d'})
    info_section = score_main.find('div', {'class': 'MHNSJI-wb-h'}).find('div', {'class': 'gwt-HTML MHNSJI-d-P'})
    info_selections = info_section.find_all('div', {'class': 'MHNSJI-d-cc'})
    for section in info_selections:
        priceDiv = section.find('div', {'class': 'MHNSJI-d-Ab'})
        price = priceDiv.find('div', {'elm': 'p'}).find('div', {'class': 'MHNSJI-d-zb'}).text.strip()
        type = section.find('div', {'class': 'MHNSJI-d-Ab'}).find('div', {'class': 'MHNSJI-d-Bb'}).text.strip()
        schedulePanels = section.find('div', {'class': 'MHNSJI-d-bc'}).find('div', {'class': 'MHNSJI-d-ac'}).find_all('span')
        if len(schedulePanels) == 2:
            schedule = schedulePanels[0].text.strip() + '-' + schedulePanels[1].text.strip()
        else:
            schedule = schedulePanels[0].text.strip()

        flight = section.find('div', {'class': 'MHNSJI-d-bc'}).find('div', {'class': 'MHNSJI-d-j'}).find('span').text.strip()
        flights.append({'price': price,'type':type, 'schedule':schedule, 'flight':flight});

    return flights

if __name__ == '__main__':
    reload(sys)
    sys.setdefaultencoding('utf8')

    dt = datetime.now().strftime('%Y-%m-%d_%H')

    outputDir = 'E:\\Flights\\Updates\\';
    if not os.path.exists(outputDir):
        os.mkdir(outputDir)

    data_fw=open(os.path.join(outputDir,  'flight-'+ dt+'.html'), 'w')
    url = "https://www.google.com/flights/#search;f=SEA;t=ORD,MDW;d=2016-05-20;r=2016-05-22";
    data = try_crawl(url)
    data_fw.write("<table border=\"1\">\n<tr><th>Price</th><th>Type</th><th>Schedule</th><th>Flight</th>\n")
    if(data is not None):
        for item in data:
            data_fw.write("<tr><td>" + item['price']  + "</td><td>" + item['type'] + "</td><td>" + item['schedule'] + "</td><td>" + item['flight'] + "</td></tr>\n")
    data_fw.write("</table>\n")
    data_fw.close()

    print "finished"
