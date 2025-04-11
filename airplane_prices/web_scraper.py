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
import polars as pl
import datetime as dt


def get_prices(departure: str, arrival: str, date: str, print_out=False):
    """_summary_

    Args:
        departure (str): example "OSL"
        arrival (str): example "AMS"
        date (str): example format "2025-06-04".
    """

    url = f"https://www.kayak.no/flights/{departure}-{arrival}/{date}?fs=fdDir=true;stops=~0&ucs=1a6g8dc&sort=price_a"
    if print_out:
        print(url)

    options = Options()
    options.add_argument("--headless")
    # driver = webdriver.Chrome(options=chrome_options)
    driver = webdriver.Firefox(options=options)
    driver.get(url)

    sleep_time = random.uniform(a=2, b=3)
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
        # price_element = result_soup.find("div", class_="f8F1-price-text")
        price_element = result_soup.find(string=re.compile(r"^\d[\d\s]*kr$"))
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

        flight_data.append(
            {
                "departure:": departure,
                "arrival": arrival,
                "date": date,
                "price": price,
                "take_off": take_off,
                "landing": landing,
            }
        )
    df = pl.DataFrame(flight_data)
    return df


def get_prices_wrapper(
    departures: list[str], arrivals: list[str], dates: list[str]
) -> pl.DataFrame:
    all_dfs = []
    for dep in departures:
        for dest in arrivals:
            for date in dates:
                df = get_prices(departure=dep, arrival=dest, date=date)
                all_dfs.append(df)

    return pl.concat(all_dfs)


# %%
dep = ["OSL"]
dests = ["HAM", "DUS", "AMS"]
dates = ["2025-06-01", "2025-06-03"]
df = get_prices_wrapper(departures=dep, arrivals=dests, dates=dates)

now = dt.datetime.now().strftime("%Y-%m-%d_%H-%M")
filename = f"flights_{dep[0]}_{now}.csv"
df.write_csv(filename)
# %%
