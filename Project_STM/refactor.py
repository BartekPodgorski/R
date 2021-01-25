import requests, re
from bs4 import BeautifulSoup
import pandas as pd
from datetime import datetime
#What to search
#'https://www.trustpilot.com/review/www.apple.com?page=1' 41
#'https://www.trustpilot.com/review/www.samsung.com?page='' 41
#'https://www.trustpilot.com/review/mi.com?page=' change to 27
#columns:
company_name = []
title_comment = []
comment_body = []
grade = []
country = []
final_dates = []
years = []

urls = []
for i in range(1,51):
    urls.append('https://www.trustpilot.com/review/www.apple.com?page=' + str(i))  
for i in range(1,51):
    urls.append('https://www.trustpilot.com/review/www.samsung.com?page=' + str(i))
#for i in range(1,27):
#	urls.append('https://www.trustpilot.com/review/mi.com?page='+ str(i))
  
for url in urls:
    website = requests.get(url)
    soup = BeautifulSoup(website.content, "html5lib")
    comments = soup.findAll("article",{"class":"review"})
    for comment in comments:
        script = comment.find("script",attrs={"data-initial-state":"review-info"}).get_text()
        scripts_data = eval(script, {})
        company_name.append(scripts_data["businessUnitDisplayName"])
        #nick_name = scripts_data["consumerName"]
        title_comment.append(scripts_data["reviewHeader"])
        comment_body.append(scripts_data["reviewBody"])
        grade.append(scripts_data["stars"])
        try:
            country.append(comment.find("div",attrs={"class":"consumer-information__location"}).find("span").get_text())
        except AttributeError:
            country.append('-')
        dates = comment.find("script",attrs={"data-initial-state":"review-dates"}).get_text()
        date_str = dates.replace('null','""')
        date_dicts = eval(date_str,{}) 
        final_date = date_dicts["publishedDate"]
        try:
            x = datetime.fromisoformat(final_date)
            date = x.strftime('%Y-%m-%d %H:%M:%S')
            final_dates.append(date)
        except ValueError:
            x = datetime.strptime(final_date,"%Y-%m-%dT%H:%M:%SZ")
            date = x.strftime('%Y-%m-%d %H:%M:%S')
            final_dates.append(date)        
        year = x.year
        years.append(year)

    print("Done! File is saved where you have your scrape-website.py")

df = pd.DataFrame({"company_name":company_name,"title_comment":title_comment,
    "comment_body":comment_body,"grade":grade,"country":country,"date":final_dates,"year":years})

df.to_csv('year_comments_final.csv', sep=';', index=False, encoding='utf-8')

