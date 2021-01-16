
import os
import requests
import random
import json
import sys
import time

from pprint import pprint

# List of possible values
districts = ["Braga","Vianadocastelo","Vilareal","Castelobranco","Leiria","Lisboa","Porto","Evora","Beja","Faro","Portalegre","Coimbra","Braganca","Aveiro","Santarem","Setubal","Guarda","Viseu"]
#usernames = ["Lori","Hyon","Lasandra","Barbara","Trish","Dia","Toshiko","Etsuko","Arielle","Delcie","Filiberto","Dayle","Ping","Carlee","Alyson","Zona","Sunni","Jude","Devon","Theron"]

# Request values
url_default = "http://localhost:8080/" 
url_users   = "http://localhost:8080/districts/user"
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

def init_users(number_of_users, name):
    req_OK=0
    for i in range(0, number_of_users):
        requestUrl  = "{}/reg".format(url_users)
        districtGen = random.choice(districts)
        userGen     = "{}-{}".format(name, i)
        requestJson = { "district": districtGen, "username": userGen }
        r = requests.post(requestUrl, data = json.dumps(requestJson), headers = headers)
        if r.status_code != 201:
            error_list.append(requestJson)
        else:
            districtUsers[districtGen].append(userGen)
            req_OK = req_OK + (r.status_code == 201)
    error = number_of_users-req_OK
    output_result = "(gen_users) PID {} | OK (201): {}/{}, ERROR: {}".format(sys.argv[1], req_OK, number_of_users, error)
    print(output_result)

def init_contacts(nr_contacts):

    req_OK = 0

    generated = 0
    while generated < nr_contacts:
        dname           = random.choice(districts)
        usersInDistrict = districtUsers[dname]
        
        if len(usersInDistrict) > 0:
            
            # generate request json
            username    = random.choice(usersInDistrict)  
            requestJson = { "district": dname, "username": username }
            districtContacts[dname].append(username)
            
            # make request
            r = requests.put(url_contact, data = json.dumps(requestJson), headers = headers)
            
            # check response
            if r.status_code == 200:
                districtContacts[dname].append(username)
                districtContacts[dname] = list(dict.fromkeys(districtContacts[dname]))
                req_OK = req_OK + 1
            
            generated = generated + 1

    error = nr_contacts-req_OK
    output_result = "(gen_contact) PID {} | OK (201): {}/{}, ERROR: {}".format(sys.argv[1], req_OK, nr_contacts, error)
    print(output_result)

def reg_contact_user(uname, dist, times):

    for t in range(0, times):
        requestJson = { "district": dist, "username": uname }
        r = requests.put(url_contact, data = json.dumps(requestJson), headers = headers)

def main():
    
    # pid | nr_users | init_user_name | nr_contacts
    init_user_name = str(sys.argv[3])
    nr_users = int(sys.argv[2])
    nr_contacts = int(sys.argv[4])

    #init_users(nr_users, init_user_name)
    #init_contacts(nr_contacts)
    
    reg_contact_user("user0-inst-0", "Aveiro", nr_contacts)

    #pprint(districtUsers)

if __name__ == '__main__':
    main()

