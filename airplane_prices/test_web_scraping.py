import requests
from bs4 import BeautifulSoup
from dataclasses import dataclass


@dataclass
class FlightData:
    airline: str
    price: str


def scrape_google_flights(departure, destination, date):
    url = f"https://www.kayak.no/flights/{departure}-{destination}/{date}?fs=fdDir=true;stops=~0&ucs=1a6g8dc&sort=price_a"
    print(url)
    response = requests.get(url)
    soup = BeautifulSoup(response.text, "html.parser")

    prices = soup.find_all("span", attrs={"class": "Hv20-value"})
    # flights = []
    # for flight in soup.find_all("div", class_="flight-card"):
    #     airline = flight.find("div", class_="airline").text
    #     price = flight.find("div", class_="price").text
    #     flights.append(FlightData(airline, price))

    return prices


# Usage
flights = scrape_google_flights("OSL", "AMS", "2025-06-04")
print(flights)
# for flight in flights:
#     print(f"{flight.airline}: {flight.price}")


# def scrape_google_flights(departure, destination, date):
#     url = f"https://www.google.com/flights?hl=en#flt={departure}.{destination}.{date}"
#     response = requests.get(url)
#     soup = BeautifulSoup(response.content, "html.parser")

#     flights = []
#     for flight in soup.find_all("div", class_="flight-card"):
#         airline = flight.find("div", class_="airline").text
#         price = flight.find("div", class_="price").text
#         flights.append(FlightData(airline, price))

#     return flights


# # Usage
# flights = scrape_google_flights("NYC", "LAX", "2025-03-01")
# for flight in flights:
#     print(f"{flight.airline}: {flight.price}")
