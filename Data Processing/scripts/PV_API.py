import os
import requests
import json
from keys import account_key

api_url = 'https://datamall2.mytransport.sg/ltaodataservice/PV/'
base_dir = '../../Data/LTA API/'
pv_endpoints = [
    "ODTrain"
    , "Train"
    , "ODBus"
    , "Bus"
]

data_dir = {
    'ODTrain': 'PassengerVolumeByOriginDestinationTrainStations'
    , 'Train' : 'PassengerVolumeByTrainStations'
    , 'ODBus' : 'PassengerVolumeByOriginDestinationBusStops'
    , 'Bus' : 'PassengerVolumeByBusStops'
}

year = '2024'
months = ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12']

def retrieve_data(endpoint, date):
    response = requests.get(endpoint, headers={'AccountKey': account_key}, params= {'Date': date})
    print(response.status_code)
    if response.status_code == 200:
        res = response.json()
        data_link = res['value'][0]['Link']
        return data_link
    else:
        return 0

def download_file(endpoint, download_link):
    data_down = f'{base_dir}{data_dir[endpoint]}'

if __name__ == '__main__':
    
    for endpoint in pv_endpoints:
        url = f'{api_url}{endpoint}'
        for month in months:
            date = f'{year}{month}'
            download = retrieve_data(url, date)
            if download != 0:
                # download_file(endpoint, download)
                print(download)


