import urllib
import urlparse
from bs4 import BeautifulSoup
from datetime import datetime
import json
import time
import sys
import os
import re
import string
from multiprocessing import Pool

import general_func


def try_crawl_legacy(website_url, year, month, day):
    try:
        data = crawl_legacy(website_url, year, month, day)
        if(len(data) < 1):
            return None
        else:
            return data
    except:
        return None

def crawl_legacy(website_url, year, month, day):
    url = website_url+ "/" + year + month + day;
    matchList = []
    print "[Message] Now running Legacy Crawler for " + url
    content_html = general_func.url_open(url, from_encoding='gbk')

    soup_content = BeautifulSoup(content_html, "html.parser")
    score_main = soup_content.find('div', {'id': 'nbaContent'})
    score_selection = score_main.find('div', {'id': 'nbaSSOuter'})

    if(score_selection is None ):
        return matchList
    score_selections = score_selection.find_all("div",{'class': 'GameLine'})
    if(score_selections is None ):
        return matchList
    print len(score_selections)
    for selection in score_selections:
        table = selection.find("div",  {'class': 'nbaModTopInfo'})
        if(table is None):
            return  matchList

        homeTeam = table.find("div",{'class':'nbaModTopTeamHm'})
        awayTeam = table.find("div",{'class':'nbaModTopTeamAw'})

        homeWin = homeTeam.find('div',  {'class': 'nbaModTopTeamNum win'})
        awayWin = awayTeam.find('div',  {'class': 'nbaModTopTeamNum win'})

        homeName=homeTeam.find('img')['title']
        awayName=awayTeam.find('img')['title']

        winner = "NA";
        if(homeWin != None):
            winner = homeName;
            label = "1";

        if(awayWin != None):
            winner = awayName;
            label = "0";

        matchList.append({'team1': homeName,'team2':awayName, 'winner':winner, 'date': month + "/" + day + "/" + year});

    return matchList

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
    url = website_url+ "/" + year + month + day;
    matchList = []
    print "[Message] Now running Crawler for " + url
    content_html = general_func.url_open(url, from_encoding='gbk')

    soup_content = BeautifulSoup(content_html, "html.parser")
    score_main = soup_content.find('div', {'id': 'nbaContent'})
    score_selection = score_main.find('div', {'id': 'nbaSSOuter'})

    if(score_selection is None ):
        return matchList
    score_selections = score_selection.find_all("div",{'class': 'GameLine'})
    if(score_selections is None ):
        return matchList
    for selection in score_selections:
        table = selection.find("div",  {'class': 'nbaModTopScore'}).find("div",  {'class': 'nbaTeamsRow'})
        if(table is None):
            return  matchList
        homeTeam = table.find("div",{'class':'nbaModTopTeamScr nbaModTopTeamHm'})
        awayTeam = table.find("div",{'class':'nbaModTopTeamScr nbaModTopTeamAw'})

        homeWin = homeTeam.find('h4',  {'class': 'nbaModTopTeamNum win'})
        awayWin = awayTeam.find('h4',  {'class': 'nbaModTopTeamNum win'})

        homeId = homeTeam.find('h5',  {'class': 'nbaModTopTeamName'}).text.strip()
        awayId = awayTeam.find('h5', {'class': 'nbaModTopTeamName'}).text.strip()

        homeName=homeTeam.find('img')['title']
        awayName=awayTeam.find('img')['title']

        if(homeWin != None):
            winner = homeName;
            label = "1";

        if(awayWin != None):
            winner = awayName;
            label = "0";

        matchList.append({'team1': homeName,'team2':awayName, 'winner':winner, 'label':label, 'date': month + "/" + day + "/" + year});

    return matchList

if __name__ == '__main__':
    reload(sys)
    year = int(sys.argv[1])
    month = int(sys.argv[2])
    day = int(sys.argv[3])

    if(month <10):
        monthStr = '0'+str(month)
    else:
        monthStr = str(month)

    if(day <10):
        dayStr = '0'+str(day)
    else:
        dayStr = str(day)

    outputDir = 'D:\\NBA\\Resource\\';
    if not os.path.exists(outputDir):
        os.mkdir(outputDir)
    data_fw=open(os.path.join(outputDir,  'NBA-Playoff-2016-'+ monthStr+'-' + dayStr+'.txt'), 'w')
    data = try_crawl("http://www.nba.com/gameline", str(year), monthStr, dayStr)
    if(data is None):
        data = try_crawl_legacy("http://www.nba.com/gameline", str(year), monthStr, dayStr)

    data_fw.write("Week Day	MatchDate	HomeTeam	AwayTeam	Winner")
    data_fw.write('\n')
    if(data is not None):
        for item in data:
            data_fw.write("0\t0\t"+item["date"]  + "\t" + item["team1"] + "\t" +item["team2"] + "\t" +item["winner"])
            data_fw.write('\n')
    data_fw.close()
    print "finished"
