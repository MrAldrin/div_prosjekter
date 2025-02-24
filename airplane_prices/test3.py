# %%
import requests
from bs4 import BeautifulSoup
from dataclasses import dataclass
from selenium import webdriver
import time

# %%
driver = webdriver.Chrome()
# %%
departure = "OSL"
destination = "AMS"
date = "2025-06-04"
url = f"https://www.kayak.no/flights/{departure}-{destination}/{date}?fs=fdDir=true;stops=~0&ucs=1a6g8dc&sort=price_a"
print(url)
# %%
driver.get(url)
time.sleep(20)
content = driver.page_source

soup = BeautifulSoup(content)

prices = soup.find_all(
    "div", attrs={"class": "hYzH-price", "aria-label": "Vis kun resultater for direkte"}
)
