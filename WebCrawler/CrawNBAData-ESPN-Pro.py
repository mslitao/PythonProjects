from bs4 import BeautifulSoup
import time
import sys
import os
from selenium import webdriver

def try_crawl(website_url, year, month, day):
    try:
        data = crawl(website_url, year, month, day)
        if(len(data) < 1):
            return None
        else:
            return data
    except:
        return None

def crawl(website_url, year, month, day):
    #url = website_url+ "/"  + month +'/'+ day +'/'+ year;
    url = website_url+ "/" + year + month + day;
    matchList = []
    print "[Message] Now running Crawler for " + url
    #content_html = general_func.url_open(url, from_encoding='gbk')

    driver = webdriver.PhantomJS('D:\\Tools\\phantomjs\\bin\\phantomjs.exe')
    driver.get(url)
    time.sleep(2)
    content_html = driver.page_source
    driver.close()
    driver.quit()

    soup_content = BeautifulSoup(content_html, "html.parser")
    score_main = soup_content.find('div', {'class': 'main-content layout-bc'})\
        .find('div', {'id': 'scoreboard-page'})
    #print score_main

    score_selection = score_main.find('div', {'id': 'events'})
    #print score_selection

    if(score_selection is None ):
        return matchList
    score_selections = score_selection.find_all("article")
    if(score_selections is None ):
        return matchList
    print len(score_selections)
    for selection in score_selections:
        table = selection.find("tbody",  {'id': 'teams'})
        if(table is None):
            print 'Non Table Found'
            return matchList
        homeTeam = table.find("tr",{'class':'home'})
        awayTeam = table.find("tr",{'class':'away'})
        homeTeam_td = homeTeam.find_all('td')
        awayTeam_td = awayTeam.find_all('td')

        homeName =homeTeam_td[0].find('h2').find_all('span')[1].text.strip()
        awayName =awayTeam_td[0].find('h2').find_all('span')[1].text.strip()

        if len(homeTeam_td)>=6 and len(awayTeam_td)>=6 :
            homeScore =homeTeam_td[len(homeTeam_td)-1].text.strip()
            awayScore =awayTeam_td[len(awayTeam_td)-1].text.strip()

            if int(homeScore)> int(awayScore):
                winner = homeName
            else:
                winner = awayName
        else:
            winner = ''
        matchList.append({'team1': homeName,'team2':awayName, 'winner':winner, 'date': month + "/" + day + "/" + year});

    return matchList

if __name__ == '__main__':
    reload(sys)

    teamMappingFile='D:\\BingPrediction\\NBA-2017\\Resource\\TeamName.mapping.txt'
    teamMapping = {}
    for line in open(teamMappingFile, 'r'):
        items = line.split('\t')
        teamMapping[items[0]] = items[1].strip()
    teamMapping['']=''

    year = int(sys.argv[1])
    month = int(sys.argv[2])
    day = int(sys.argv[3])

    #year = 2017
    #month = 4
    #day = 17
    outputDir = 'D:\\BingPrediction\\NBA-2017\\Resource\\' + '\\data\\';
    if not os.path.exists(outputDir):
        os.mkdir(outputDir)
    print outputDir

    if(month <10):
        monthStr = '0'+str(month)
    else:
        monthStr = str(month)

    if(day <10):
        dayStr = '0'+str(day)
    else:
        dayStr = str(day)

    data = try_crawl("http://www.espn.com/nba/scoreboard/_/date", str(year), monthStr, dayStr)

    data_fw=open(os.path.join(outputDir, "NBA-data_" + str(year) + monthStr + dayStr + '.txt'), 'w')
    data_fw.write("Week	Day	MatchDate	HomeTeam	AwayTeam	Winner")
    data_fw.write('\n')
    if(data is not None):

        for item in data:
            print item
            team1 = teamMapping[item["team1"]]
            team2 = teamMapping[item["team2"]]
            winner = teamMapping[item["winner"]]
            data_fw.write("0\t0\t"+item["date"]  + "\t" + team1 + "\t" +team2 + "\t" + winner)
            data_fw.write('\n')

    data_fw.close()
    print "finished"