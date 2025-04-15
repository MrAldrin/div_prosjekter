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
from concurrent.futures import ThreadPoolExecutor

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    handlers=[logging.StreamHandler()],
)


def get_prices(departure: str, arrival: str, date: str, print_out=False):
    """_summary_

    Args:
        departure (str): example "OSL"
        arrival (str): example "AMS"
        date (str): example format "2025-06-04".
    """
    logging.info(f"Starting to scrape {departure}-{arrival} on {date}")

    url = f"https://www.kayak.no/flights/{departure}-{arrival}/{date}?fs=fdDir=true;stops=~0&ucs=1a6g8dc&sort=price_a"
    if print_out:
        print(url)

    options = Options()
    options.add_argument("--headless")
    # driver = webdriver.Chrome(options=chrome_options)
    driver = webdriver.Firefox(options=options)

    try:
        driver.get(url)

        sleep_time = 1.5
        logging.info(f"Sleeping for {sleep_time:.2f} to wait for the page to load")
        time.sleep(sleep_time)

        content = driver.page_source
    except Exception as e:
        logging.error(f"Error while fetching the page: {e}. Returning empy df")
        return pl.DataFrame()
    driver.quit()

    soup = BeautifulSoup(content, features="html.parser")

    results = soup.find_all(
        "div",
        attrs={
            "class": "yuAt yuAt-pres-rounded yuAt-mod-box-shadow yuAt-mod-responsive-margins"
        },
    )

    if not results:
        logging.warning(f"No results found for {departure} to {arrival} on {date}")

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

    logging.info(f"Scraped {len(flight_data)} flights {departure}-{arrival} on {date}")
    df = pl.DataFrame(flight_data)
    return df


def get_prices_wrapper(departures, arrivals, dates):
    logging.info("Starting to scrape multiple routes and dates")
    futures = []
    with ThreadPoolExecutor() as executor:
        for dep in departures:
            for dest in arrivals:
                for date in dates:
                    # Introduce a small random delay
                    delay = random.uniform(0, 4.0)  # Delay between 0.5 and 2 seconds
                    logging.info(f"Delaying task submission for {delay:.2f} seconds")
                    time.sleep(delay)

                    # Submit the task
                    futures.append(executor.submit(get_prices, dep, dest, date))

        # Collect results
        results = [future.result() for future in futures]
    logging.info("Finished scraping all routes and dates")
    return pl.concat(results)


# %%
dep = ["OSL"]
dests = ["HAM", "DUS", "AMS"]
dates = ["2025-06-01", "2025-06-03"]

df = get_prices_wrapper(departures=dep, arrivals=dests, dates=dates)

now = dt.datetime.now().strftime("%Y-%m-%d_%H-%M")
filename = f"results/flights_{dep[0]}_{now}.csv"
try:
    df.write_csv(filename)
    logging.info(f"Data successfully written to {filename}")
except Exception as e:
    logging.error(f"Failed to write CSV file: {e}")
logging.info("Scraping process completed successfully")
# %%
