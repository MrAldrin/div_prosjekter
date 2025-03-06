# %%
import re
import requests
import random
import logging
from bs4 import BeautifulSoup
from dataclasses import dataclass
from selenium import webdriver
import time
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.firefox.options import Options
import sys
import pandas as pd


# %%
def get_prices(departure: str, destination: str, date: str, print_out=False):
    """_summary_

    Args:
        departure (str): example "OSL"
        destination (str): example "AMS"
        date (str): example format "2025-06-04".
    """

    url = f"https://www.kayak.no/flights/{departure}-{destination}/{date}?fs=fdDir=true;stops=~0&ucs=1a6g8dc&sort=price_a"
    if print_out:
        print(url)

    options = Options()
    options.add_argument("--headless")
    # driver = webdriver.Chrome(options=chrome_options)
    driver = webdriver.Firefox(options=options)
    driver.get(url)

    sleep_time = random.uniform(a=4, b=6)
    time.sleep(sleep_time)

    content = driver.page_source
    driver.quit()

    soup = BeautifulSoup(content, features="html.parser")

    results = soup.find_all(
        "div",
        attrs={
            "class": "yuAt yuAt-pres-rounded yuAt-mod-box-shadow yuAt-mod-responsive-margins"
        },
    )
    flight_data = []
    for result in results:
        # Local BeautifulSoup instance for each result:
        result_soup = BeautifulSoup(
            str(result), "html.parser"
        )  # Parse the result element

        # Extract Price:
        price_element = result_soup.find("div", class_="f8F1-price-text")
        price = None
        if price_element:
            price_text = price_element.text.strip().replace("\xa0", "")
            price = int(re.sub(r"[^\d]+", "", price_text))

        # Extract Times:
        time_element = result_soup.find("div", class_="vmXl vmXl-mod-variant-large")
        take_off = None
        landing = None

        if time_element:
            times = time_element.find_all("span")
            if len(times) >= 3:
                take_off = times[0].text.strip()
                landing = times[2].text.strip()

        flight_data.append({"price": price, "take_off": take_off, "landing": landing})
    return flight_data


# %%
departure = "OSL"
destination = "AMS"
date = "2025-06-04"
url = f"https://www.kayak.no/flights/{departure}-{destination}/{date}?fs=fdDir=true;stops=~0&ucs=1a6g8dc&sort=price_a"
# url = f"https://www.kayak.no/flights/{departure}-{destination}/{date}?ucs=qb41ig&sort=bestflight_a"
prices_list = []
# %%
url = f"https://www.kayak.no/flights/{departure}-{destination}/{date}?fs=fdDir=true;stops=~0&ucs=1a6g8dc&sort=price_a"

options = Options()
options.add_argument("--headless")
# driver = webdriver.Chrome(options=chrome_options)
driver = webdriver.Firefox(options=options)
driver.get(url)

sleep_time = random.uniform(a=4, b=6)
time.sleep(sleep_time)

content = driver.page_source
driver.quit()

soup = BeautifulSoup(content, features="html.parser")


# %%
prices_list = []
for p in prices:
    price_text = p.text.strip()
    price_text = price_text.replace("\xa0", "")
    numeric_value = re.findall(r"\d+", price_text)[0]
    prices_list.append(int(numeric_value))
    print(numeric_value)

# %%
dests = ["HAM", "DUS", "AMS"]

for dest in dests:
    price = get_prices(departure="OSL", destination=dest, date="2025-06-04")
    print(price)


# prices=get_prices(departure="OSL", destination="HAM", date="2025-06-04")
# %%
get_prices(departure="OSL", destination=dest, date="2025-06-04")
# %%
