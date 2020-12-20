
import os
import requests
import random
import json
import sys
import time

from pprint import pprint

# Number of elements
nr_users   = 20
nr_top5    = 1
nr_infect  = 20
nr_contact = 20
map_max_n  = 100 
record_max = 100

# List of possible values
districts = ["Braga","Viana Do Castelo","Vila Real","Castelo Branco","Leiria","Lisboa","Porto","Evora","Beja","Faro","Portalegre","Coimbra","Braganca","Aveiro","Santarem","Setubal","Guarda","Viseu"]
usernames = ["Lori","Hyon","Lasandra","Barbara","Trish","Dia","Toshiko","Etsuko","Arielle","Delcie","Filiberto","Dayle","Ping","Carlee","Alyson","Zona","Sunni","Jude","Devon","Theron"]

# Request values
url_default = "http://localhost:8080/" 
url_users   = "http://localhost:8080/districts/user"
url_distr   = "http://localhost:8080/districts/top5"
url_infect  = "http://localhost:8080/districts/user/infection"
url_contact = "http://localhost:8080/districts/user/contact"
headers     = {'Content-Type': 'application/json'}

# save all district users for infections
districtUsers      = {}
districtInfections = {}
districtContacts   = {}
for dist in districts:
    districtUsers[dist] = []
    districtInfections[dist] = []
    districtContacts[dist] = []

def init_show_stats():
    print("* preparing to generate:")
    print("\t- nr of users           : ", nr_users)
    print("\t- nr of top5 insertions : ", nr_top5)
    print("\t- max record top5       : ", record_max)
    print("\t- map size (NxN)        : ", map_max_n)

def init_users(number_of_users):
    print("* generating users...")
    req_OK = 0
    error_list = []
    for i in range(0, number_of_users):
        requestUrl  = "{}/reg".format(url_users)
        districtGen = random.choice(districts)
        userGen     = random.choice(usernames).lower()
        requestJson = { "district": districtGen, "username": userGen }
        r = requests.post(requestUrl, data = json.dumps(requestJson), headers = headers)
        if r.status_code != 201:
            error_list.append(requestJson)
        else:
            districtUsers[districtGen].append(userGen)
            req_OK = req_OK + (r.status_code == 201)
        print("\trequests (status=201): ", req_OK, "/", nr_users)
        sys.stdout.write("\033[F")
        time.sleep(0.1)
    print("\trequests (status=201): ", req_OK, "/", nr_users, " | ", nr_users-req_OK, " got error.")

def init_top5(requests_per_district):
    print("* generating districts...")
    total = requests_per_district*len(districts)
    req_OK = 0
    error_list = []
    for d in range(0, len(districts)):
        district_name = districts[d]
        for i in range(0, requests_per_district):
            requestUrl = url_distr
            posX   = random.randint(0, map_max_n)
            posY   = random.randint(0, map_max_n)
            record = random.randint(0, record_max) 
            requestJson = { "district": district_name, "positionX": posX, "positionY": posY, "record": record }
            r = requests.put(requestUrl, data = json.dumps(requestJson), headers = headers)
            if r.status_code != 200:
                error_list.append(requestJson)
            else:
                req_OK = req_OK + (r.status_code == 200)
        print("\trequests (status=200): ", req_OK, "/", total)
        sys.stdout.write("\033[F")
        time.sleep(0.1)
    print("\trequests (status=200): ", req_OK, "/", total, " | ", total-req_OK, " got error.")

def init_infections_contacts(which, total_infections):
    print("* generating ", which, "...")
    req_OK = 0
    error_list = []
    generated = 0
    while generated < total_infections:
        dname           = random.choice(districts)
        usersInDistrict = districtUsers[dname]
        # in case its an empty list
        if len(usersInDistrict) > 0:
            username    = random.choice(usersInDistrict)  
            requestJson = { "district": dname, "username": username }
            if which == "infection":
                requestUrl  = url_infect
            else:
                requestUrl  = url_contact
                districtContacts[dname].append(username)
            r = requests.put(requestUrl, data = json.dumps(requestJson), headers = headers)
            if r.status_code != 200:
                error_list.append(requestJson)
            else:
                if which == "infection":
                    districtInfections[dname].append(username)
                else:
                    districtContacts[dname].append(username)
                    districtContacts[dname] = list(dict.fromkeys(districtContacts[dname]))
                req_OK = req_OK + 1
            print("\trequests (status=200): ", req_OK, "/", total_infections)
            sys.stdout.write("\033[F")
            time.sleep(0.1)
            generated = generated + 1
    print("\trequests (status=200): ", req_OK, "/", total_infections, " | ", total_infections-req_OK, " got error.")

def main():
    print("starting...")
    os.system('cls' if os.name == 'nt' else 'clear')
    init_show_stats()
    init_users(nr_users)
    init_top5(nr_top5)
    init_infections_contacts("infection", nr_infect)
    init_infections_contacts("contact", nr_contact)
    print("final stats:")
    print("users:")
    pprint(districtUsers)
    print("infections:")
    pprint(districtInfections)
    print("contacts:")
    pprint(districtContacts)
    print("done.")

if __name__ == '__main__':
    main()