import os
import pandas as pd

filepath = 'Data\LTA API'

api_data = [
    'PassengerVolumeByBusStops'
    , 'PassengerVolumeByOriginDestinationBusStops'
    , 'PassengerVolumeByOriginDestinationTrainStations'
    , 'PassengerVolumeByTrainStations'
]

def main():
    ''