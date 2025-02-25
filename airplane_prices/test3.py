# %%
import re
import requests
from bs4 import BeautifulSoup
from dataclasses import dataclass
from selenium import webdriver
import time
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.firefox.options import Options


# %%
def get_prices(departure: str, destination: str, date: str):
    """_summary_

    Args:
        departure (str): example "OSL"
        destination (str): example "AMS"
        date (str): example format "2025-06-04".
    """

    url = f"https://www.kayak.no/flights/{departure}-{destination}/{date}?fs=fdDir=true;stops=~0&ucs=1a6g8dc&sort=price_a"

    options = Options()
    options.add_argument("--headless")
    # driver = webdriver.Chrome(options=chrome_options)
    driver = webdriver.Firefox(options=options)
    driver.get(url)
    time.sleep(8)
    content = driver.page_source
    driver.quit()
    soup = BeautifulSoup(content, features="html.parser")

    prices = soup.find_all("div", attrs={"class": "f8F1-price-text"})
    prices_list = []
    for p in prices:
        price_text = p.text.strip()
        price_text = price_text.replace("\xa0", "")
        numeric_value = re.findall(r"\d+", price_text)[0]
        prices_list.append(int(numeric_value))
    return prices_list


# %%
departure = "OSL"
destination = "AMS"
date = "2025-06-04"
url = f"https://www.kayak.no/flights/{departure}-{destination}/{date}?fs=fdDir=true;stops=~0&ucs=1a6g8dc&sort=price_a"

prices_list = []
# %%
driver = webdriver.Chrome()
driver.get(url)
time.sleep(20)
content = driver.page_source

soup = BeautifulSoup(content)

best_price = soup.find_all(
    "div", attrs={"class": "hYzH-price", "aria-label": "Vis kun resultater for direkte"}
)
prices = soup.find_all("div", attrs={"class": "f8F1-price-text"})

# %%
prices_list = []
for p in prices:
    price_text = p.text.strip()
    price_text = price_text.replace("\xa0", "")
    numeric_value = re.findall(r"\d+", price_text)[0]
    prices_list.append(int(numeric_value))
    print(numeric_value)

# %%
get_prices(departure="OSL", destination="HAM", date="2025-06-04")
# %%
