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

        url = 'http://www.goodreads.com/search?q='+id
        htmlSource = general_func.url_open(url, from_encoding='gbk')

        data = crawl_detail(htmlSource)
        return data
    except:
        return None


def crawl_detail(html_content):
    book = {}
    soup_content = BeautifulSoup(html_content, "html.parser")

    #link_section = soup_content.find('meta',{'property': 'og:url'})
    #book['link'] =link_section['content'].encode('utf-8')
    link_section = soup_content.find('link',{'rel': 'canonical'})
    book['link'] =link_section['href'].encode('utf-8')

    score_main = soup_content.find('div', {'class': 'leftContainer'}).find('div', {'id': 'topcol'})

    info_section = score_main.find('div', {'id': 'metacol'}).find('div', {'id': 'bookMeta'})
    book['score'] = info_section.find('span',{'class':'value rating'}).find('span',{'class':'average'}).text.strip()
    book['ratings'] = info_section.find('a',{'class':'actionLinkLite votes'}).find('span',{'class':'value-title'}).text.strip().replace(' Ratings','').replace(' Rating','')
    book['reviews'] = info_section.find('span',{'class':'count'}).find('span',{'class':'value-title'}).text.strip()

    #print book
    bookDataBox = score_main.find('div', {'id': 'metacol'}).find('div', {'id': 'details'}).find('div', {'id': 'bookDataBox'})
    #print bookDataBox
    isbn_section= bookDataBox.find('span', {'itemprop': 'isbn'})
    book['isbn'] =isbn_section.text.strip()
    #print book

    return book

if __name__ == '__main__':
    reload(sys)
    sys.setdefaultencoding('utf8')

    outputDir = os.getcwd();
    if not os.path.exists(outputDir):
        os.mkdir(outputDir)
    index = 1
    data_fw=open(os.path.join(outputDir,  'Book-Review-part1-'+ str(index) +'.txt'), 'w')
    inputFile = os.path.join(outputDir,  'book_isbn-2016.part1.txt')
    #data_fw.write('id'+'\t'+'score'+'\t'+'ratings'+'\t'+'reviews'+'\t'+'link' + '\n')

    cnt = 0
    successCnt =0
    for line in open(inputFile, 'r'):
        cnt = cnt +1
        if cnt == 1:
            continue

        #if cnt > 1000:
        #   break

        if(successCnt >= 1000):
            successCnt = 0
            data_fw.close()
            index = index +1
            print index
            data_fw=open(os.path.join(outputDir,  'Book-Review-part1-'+ str(index) +'.txt'), 'w')

        items = line.split('\t')
        satoriId =items[1].replace('\n','')
        isbn = items[0]
        data = try_crawl(isbn)

        if(data is not None):
            successCnt = successCnt +1
            data_fw.write(satoriId + "\t" + data["isbn"] + "\t" +data["score"] + "\t" + data["ratings"] + "\t" +data["reviews"] + "\t" + data["link"] + "\n")
    data_fw.close()
    print "finished"
