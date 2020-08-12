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
from selenium import webdriver
from datetime import timedelta, date
import general_func



def try_crawl(url):
    try:
        data = crawl(url)
        if(len(data) < 1):
            return None
        else:
            return data
    except:
        return None

def crawl(url):
    matchList = []
    print "[Message] Now running Crawler for " + url
    driver = webdriver.PhantomJS('D:\\Tools\\phantomjs\\bin\\phantomjs.exe')
    driver.get(url)
    time.sleep(120)
    content_html = driver.page_source
    driver.close()
    driver.quit()

    #content_html = general_func.url_open(url, from_encoding='gbk')

    soup_content = BeautifulSoup(content_html, "html.parser")
    score_main = soup_content.find('div', {'class': 'flights-results-wrap'})
    score_main = score_main.find('div', {'id': 'fares-search-package-list'})
    #print(score_main)
    score_selection = score_main.find('ul', {'class': 'exp-container-flight-package'})

    if(score_selection is None ):
        print('No selection found')
        return matchList
    score_selections = score_selection.find_all("li",{'class': 'package'})
    if(score_selections is None ):
        return matchList
    for selection in score_selections:
        #print(selection)
        price_div = selection.find("div",  {'class': 'display-table flight-price'})
        prive_number = price_div.find("div",  {'class': 'flight-price-number'}).find("span", {'class': 'total-price'})

        price =re.sub("'\r\n'", r"' '", prive_number.text.strip())

        if(price is None):
            continue
        price = price.strip(" ").strip("$").replace(',','')
        print(price)
        if(float(price) > 1000):
            continue
        print(price)

        flight_detail = selection.find("div",  {'class': 'package-details-box flightDetails '})
        flight_departure =flight_detail.find_all("div",  {'class': 'city-pair'})[0]
        flight_routes =flight_departure.find_all("div", {'class': 'faredetails display-table'})
        print("departure")
        if(len(flight_routes) > 3):
            continue
        departure_detail = []
        for route in flight_routes:
            carrier_name =route.find("div",  {'class': 'column flight-description'}).find("div",  {'class': 'carrier-name'}).text.strip()
            flight_breakdown = route.find("div",  {'class': 'column flight-breakdown'})
            from_airport =flight_breakdown.find_all("ul")[0].find("li",  {'class': 'flight-airport '}).find('span').text.strip()
            from_time = flight_breakdown.find_all("ul")[0].find("li", {'class': 'flight-time'}).text.strip()
            from_date = flight_breakdown.find_all("ul")[0].find("li", {'class': 'flight-date'}).text.strip()
            to_airport = flight_breakdown.find_all("ul")[1].find("li", {'class': 'flight-airport'}).find('span').text.strip()
            to_time = flight_breakdown.find_all("ul")[1].find("li", {'class': 'flight-time'}).text.strip()
            to_date = flight_breakdown.find_all("ul")[1].find("li", {'class': 'flight-date'}).text.strip()
            departure_detail.append(carrier_name + ":" + from_airport + '('+from_time+from_date+')'+ "-" + to_airport+ '('+to_time + to_date+')')
            print(carrier_name + ":" + from_airport + '('+from_time+from_date+')'+ "-" + to_airport+ '('+to_time + to_date+')')

        flight_return = flight_detail.find_all("div", {'class': 'city-pair'})[1]
        flight_routes = flight_return.find_all("div", {'class': 'faredetails display-table'})
        print("return")
        if (len(flight_routes) > 3):
            continue
        return_detail = []
        for route in flight_routes:
            carrier_name = route.find("div", {'class': 'column flight-description'}).find("div", {'class': 'carrier-name'}).text.strip()
            flight_breakdown = route.find("div", {'class': 'column flight-breakdown'})
            from_airport = flight_breakdown.find_all("ul")[0].find("li", {'class': 'flight-airport '}).find('span').text.strip()
            from_time = flight_breakdown.find_all("ul")[0].find("li", {'class': 'flight-time'}).text.strip()
            from_date = flight_breakdown.find_all("ul")[0].find("li", {'class': 'flight-date'}).text.strip()
            to_airport = flight_breakdown.find_all("ul")[1].find("li", {'class': 'flight-airport'}).find('span').text.strip()
            to_time = flight_breakdown.find_all("ul")[1].find("li", {'class': 'flight-time'}).text.strip()
            to_date = flight_breakdown.find_all("ul")[1].find("li", {'class': 'flight-date'}).text.strip()
            return_detail.append(carrier_name + ":" + from_airport + '(' + from_time + from_date + ')' + "-" + to_airport + '(' + to_time + to_date + ')')
            print(carrier_name + ":" + from_airport + '(' + from_time + from_date + ')' + "-" + to_airport + '(' + to_time + to_date + ')')

        matchList.append(
            {'price': price, 'departure': departure_detail,'departure_num':len(departure_detail), 'return': return_detail,'return_num':len(return_detail)});

    return matchList

def daterange(start_date, end_date):
    for n in range(int ((end_date - start_date).days)):
        yield start_date + timedelta(n)

if __name__ == '__main__':
    reload(sys)

    outputDir = 'D:\\Python\\Flight\\Data\\';
    if not os.path.exists(outputDir):
        os.mkdir(outputDir)


    data_fw = open(os.path.join(outputDir, "Flight-data_Beijing" +'.txt'), 'w')
    data_fw.write("Price	departure_date	return_date	departure-Stops	departure1	departure2	departure3	return-Stops	return1	return2	return3")
    data_fw.write('\n')

    departure_start = date(2018, 6, 23)
    departure_end = date(2018, 7, 4)

    return_start = date(2018, 11, 23)
    return_end = date(2018, 12, 5)
    for departure_date in daterange(departure_start, departure_end):
        for return_date in daterange(return_start, return_end):
            url = 'https://www.justfly.com/flight/search?num_adults=2&num_children=0&num_infants=0&num_infants_lap=0&seat_class=Economy&seg0_date='+departure_date.strftime("%Y-%m-%d") +'&seg0_from=SEA&seg0_to=PEK&seg1_date='+return_date.strftime("%Y-%m-%d")+'&seg1_from=XIY&seg1_to=SEA&type=roundtrip")'
            print(url)


            data = try_crawl(url)
            #data = try_crawl("https://www.justfly.com/flight/search?num_adults=2&num_children=0&num_infants=0&num_infants_lap=0&seat_class=Economy&seg0_date=2018-06-28&seg0_from=SEA&seg0_to=XIY&seg1_date=2018-12-05&seg1_from=XIY&seg1_to=SEA&type=roundtrip")
            #print(data)

            if(data is not None):

                for item in data:
                    price = item["price"]
                    departure_detail = item["departure"]
                    return_detail = item["return"]
                    departure1 = departure_detail[0] if len(departure_detail)>0 else ''
                    departure2 = departure_detail[1] if len(departure_detail) > 1 else ''
                    departure3 = departure_detail[2] if len(departure_detail) > 2 else ''

                    return1 = return_detail[0] if len(return_detail) > 0 else ''
                    return2 = return_detail[1] if len(return_detail) > 1 else ''
                    return3 = return_detail[2] if len(return_detail) > 2 else ''

                    data_fw.write(price +"\t"+departure_date.strftime("%Y-%m-%d")+"\t"+return_date.strftime("%Y-%m-%d")+"\t"+ str(len(departure_detail))+"\t"+departure1  + "\t" + departure2 + "\t" +departure3 + "\t"+ str(len(return_detail)) +"\t"+ return1+ "\t" + return2+ "\t" + return3)
                    data_fw.write('\n')
            #break
        #break
    data_fw.close()
    print "finished"
