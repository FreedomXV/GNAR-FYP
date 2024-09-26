import os
from keys import account_key

api_url = 'http://datamall2.mytransport.sg/ltaodataservice/PV/'

endpoints = [
    "http://datamall2.mytransport.sg/ltaodataservice/PV/ODTrain"
    , "http://datamall2.mytransport.sg/ltaodataservice/PV/Train"
    , "http://datamall2.mytransport.sg/ltaodataservice/TrafficFlow"
    , "http://datamall2.mytransport.sg/ltaodataservice/TrafficIncidents"
    , "http://datamall2.mytransport.sg/ltaodataservice/RoadWorks"
]
api_url = 'http://datamall2.mytransport.sg/ltaodataservice/TrafficFlow'
print(api_url)

if __name__ == '__main__':

    dataset_base_path = os.path.join(os.getcwd(), '..', 'Data')

    if not os.path.exists(dataset_base_path):
        os.makedirs(dataset_base_path)