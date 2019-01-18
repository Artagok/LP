#!/usr/bin/python3

# ======= #
# IMPORTS #
# ======= #

import argparse
from ast import literal_eval
import urllib.request
import xml.etree.ElementTree as ET
import datetime
import re
import webbrowser
import math
import unicodedata

# ========= #
# ARGUMENTS #
# ========= #

parser = argparse.ArgumentParser(usage="cerca.py [-h|--help] --key KEY [--date DATE] [--distance DISTANCE]")
parser.add_argument("--key", required=True, help="""single-quote-surrounded string containing the query\n
                                                    conjunction represented by a list:  [A,B,C...]\n
                                                    disjunction represented by a tuple: (A,B,C,..)\n
                                                    \twhere A,B,C are double-quoted-surrounded strings to match in the query. --key is required""")
parser.add_argument("--date", help="""single-quote-surrounded dd/mm/yyyy formatted date to match only those events occurring on this date\n
                                        if --date is present --distance should not""")
parser.add_argument("--distance", default=500, type=int, help="a number expressing the maximum distance in m where there should be any bicing stations")
args = parser.parse_args()

keys = literal_eval(args.key)   # string
date = args.date                # string dd/mm/yyyy
distance = args.distance        # int NUM

# ============== #
# HTML GENERATOR #
# ============== #

def to_html(events, flag): # Flag = 1 ->  No bicing stations | Flag = 2 -> No date check (all events assumed for today)
    # Part Comuna: Header, etc
    file = open("index.html", "w")
    file.write("""<!DOCTYPE html>
    <html>
        <head>
            <title>Practica Python LP</title>
            <meta charset="UTF-8" />
            <style>
                html { font-family: Arial }
                table {border-top:1px solid black; border-bottom:1px solid black }
                th { font-size:16px; border-left:1px solid black; border-right:1px solid black; border-bottom:1px solid black; color:#ffffff; padding:5px 10px }
                td { font-size:12px; border-left:1px solid black; border-right:1px solid black; border-bottom:1px solid black; padding:5px }
            </style>
        </head>""")

    # MENSUALS / NO BICING
    if flag == 1:
        file.write("""<body>
            <table style="width:100%;border-collapse:collapse">
                <tr style="background-color:#777777">
                    <th>Nom Activitat</th>
                    <th>Lloc</th>
                    <th>Adreça</th>
                    <th>Data Inici</th>
                    <th>Data Fi</th>
                    <th>Hora</th>
                </tr>""")
        for e in events:
            file.write("<tr>")
            file.write("<td>{}</td>".format(e.name))
            file.write("<td>{}</td>".format(e.place_name))
            file.write("<td>{}</td>".format(e.place_street + " " + e.place_number + ", " + e.place_district))
            dateInAux = datetime.datetime.strftime(e.date_inici, "%d/%m/%Y")
            dateFiAux = datetime.datetime.strftime(e.date_fi, "%d/%m/%Y")
            horaAux = datetime.datetime.strftime(e.hora_inici, "%H.%M")
            file.write("<td>{}</td>".format(dateInAux))
            file.write("<td>{}</td>".format(dateFiAux if dateFiAux != "31/12/9999" else " "))
            file.write("<td>{}</td>".format(horaAux))
            file.write("</tr>")
        file.write("""</table>
        <div style= margin:20px 10px 10px 10px>
        <p style="font-family: Arial; font-size: 12px">Data Fi en blanc es deu a que la data de finalització a l'XML mensuals era 31/12/9999 i l'he considerada data invàlida.</p>
        <p style="font-family: Arial; font-size: 12px">Moltes hores apareixen com 00:00h ja que a l'XML mensuals aquest atribut (hora_inici) era buit.</p>
        </div>
        </body>
        </html>""")
    
    # DIARIS / SI BICINGS
    elif flag == 2:
        file.write("""<body>
        <table style="width:100%;border-collapse:collapse">
            <colgroup style="width:20%"></colgroup>
            <colgroup></colgroup>
            <colgroup></colgroup>
            <colgroup></colgroup>
            <colgroup span="2"></colgroup>
            <colgroup span="2"></colgroup>
            <tr style="background-color:#777777">
                <th rowspan="2">Nom Activitat</th>
                <th rowspan="2">Lloc</th>
                <th rowspan="2">Adreça</th>
                <th rowspan="2">Data i Hora</th>
                <th colspan="2">Bicing amb llocs disponibles</th>
                <th colspan="2">Bicing amb bicis disponibles</th>
            </tr>
            <tr style="background-color:#777777">
                <th style="font-size:14px">Adreça</th>
                <th style="font-size:14px; padding:2px">#Llocs</th>
                <th style="font-size:14px">Adreça</th>
                <th style="font-size:14px; padding:2px">#Bicis</th>
            </tr>
        """)
        for e in events:
            file.write("        <tr>")
            file.write("            <td rowspan='6'>{}</td>\n".format(e.name))
            file.write("            <td rowspan='6'>{}</td>\n".format(e.place_name))
            file.write("            <td rowspan='6'>{}</td>\n".format(e.place_street + " " + e.place_number + ", " + e.place_district))
            dateHora = datetime.datetime.strftime(e.date, "%d/%m/%Y %H.%M")
            file.write("            <td rowspan='6'>{}</td>\n".format(dateHora))
            file.write("        </tr>")

            for i in range(0,5):
                # Si border-bottom
                if i == 4:
                    file.write("<tr>")
                    file.write("<td>{}</td>".format( (e.bicing_slots[i].place_street + ", " + e.bicing_slots[i].place_number) if len(e.bicing_slots) > i else ""))
                    file.write("<td style='text-align:center'>{}</td>".format(e.bicing_slots[i].slots if len(e.bicing_slots) > i else ""))
                    file.write("<td>{}</td>".format( (e.bicing_bikes[i].place_street + ", " + e.bicing_bikes[i].place_number) if len(e.bicing_bikes) > i else ""))
                    file.write("<td style='text-align:center'>{}</td>".format(e.bicing_bikes[i].bikes if len(e.bicing_bikes) > i else ""))
                    file.write("</tr>")
                # No border-bottom
                else:
                    file.write("<tr>")
                    file.write("<td style='border-bottom:0px'>{}</td>".format( (e.bicing_slots[i].place_street + ", " + e.bicing_slots[i].place_number) if len(e.bicing_slots) > i else ""))
                    file.write("<td style='border-bottom:0px ; text-align:center'>{}</td>".format(e.bicing_slots[i].slots if len(e.bicing_slots) > i else ""))
                    file.write("<td style='border-bottom:0px'>{}</td>".format( (e.bicing_bikes[i].place_street + ", " + e.bicing_bikes[i].place_number) if len(e.bicing_bikes) > i else ""))
                    file.write("<td style='border-bottom:0px ; text-align:center'>{}</td>".format(e.bicing_bikes[i].bikes if len(e.bicing_bikes) > i else ""))
                    file.write("</tr>")
    
        file.write("""</table>
            </body>
            </html>""")
            
    # Part final comuna
    file.close()
    webbrowser.open("index.html")

# ================ #
# DISTANCE CHECKER #
# ================ #

D_RAD = math.pi / 180.0 
E_RAD = 6371000.0

def degToRad(deg):
    return deg * D_RAD

def check_dist(event, station , distance):
    lat_event = float(event.lat)
    lon_event = float(event.lon)
    lat_station = float(station.lat)
    lon_station = float(station.lon)
    phi1 = degToRad(90.0 - lat_event)
    phi2 = degToRad(90.0 - lat_station)
    the1 = degToRad(lon_event)
    the2 = degToRad(lon_station)
    cos = math.sin(phi1) * math.sin(phi2) * math.cos(the1 - the2) + math.cos(phi1) * math.cos(phi2)
    dist_event_station = math.acos(cos) * E_RAD
    return (float(dist_event_station) <= float(distance))


def text_patch(text):
    if text == None:
        return ""
    else:
        return text

def find_text(n, p):
    return text_patch(n.find(p).text)

# ====== #
# BICING #
# ====== #

class Bicing:

    def is_open(self): return self.status == "OPN"
    def has_slots(self): return self.slots != "0"
    def has_bikes(self): return self.bikes != "0"

    class Builder:

        def __init__(self):
            self.obj = Bicing()
        
        def coord(self, lat, lon):
            self.obj.lat = lat
            self.obj.lon = lon
            return self
        
        def place_street(self, place_street):
            self.obj.place_street = place_street
            return self
        
        def place_number(self, place_number):
            self.obj.place_number = place_number
            return self

        def info(self, status, slots, bikes):
            self.obj.status = status
            self.obj.slots = slots
            self.obj.bikes = bikes
            return self
        
        def build(self):
            if self.obj.lat == '': self.obj.lat = '0'
            if self.obj.lon == '': self.obj.lon = '0'
            return self.obj


def get_float_bicing(xml, at):
    try:
        return float(find_text(xml,"./" + at))
    except ValueError as err:
        return 0.0

def deserialize_bicing(xml):
    return Bicing.Builder() \
        .coord(get_float_bicing(xml, "lat"), get_float_bicing(xml, "long")) \
        .place_street(find_text(xml, "./street")) \
        .place_number(find_text(xml, "./streetNumber")) \
        .info(find_text(xml, "./status"), find_text(xml, "./slots"), find_text(xml, "./bikes")) \
        .build()

# ===== #
# EVENT #
# ===== #

class Event:

    def __init__(self):
        self.bicing_slots = []
        self.bicing_bikes = []

    def add_slots_bicing(self, station):
        if len(self.bicing_slots) < 5:
            self.bicing_slots.append(station)

    def add_bikes_bicing(self, station):
        if len(self.bicing_bikes) < 5:
            self.bicing_bikes.append(station)

    class Builder:

        def __init__(self):
            self.obj = Event()

        def name(self, name):
            self.obj.name = name
            return self
        
        ######### ADRECA/LLOC #########
        def place_name(self, place_name):
            self.obj.place_name = place_name
            return self

        def place_street(self, place_street):
            self.obj.place_street = place_street
            return self
        
        def place_number(self, place_number):
            self.obj.place_number = place_number
            return self

        def place_district(self, place_district):
            self.obj.place_district = place_district
            return self

        def coord(self, lat, lon):
            self.obj.lat = lat
            self.obj.lon = lon
            return self
        ################################
        
        def date(self, date):
            self.obj.date = date
            return self

        def date_range(self, date_inici, date_fi, hora_inici):
            self.obj.date_inici = date_inici
            self.obj.date_fi = date_fi
            self.obj.hora_inici = hora_inici
            return self

        def build(self):
            return self.obj

# ======= #
# MENSUAL #
# ======= #
# Mensual -> Do not use coords since distance/bikes are ignored
# Mensual -> Do use date_inici & data_fi since we have to check that date is between them
def deserialize_event_mensual(xml):
    date_format = "%d/%m/%Y"
    hora_format = "%H.%M"
    
    date_inici = str(find_text(xml, "./data/data_inici"))
    date_fi = str(find_text(xml, "./data/data_fi"))
    hora_inici = str(find_text(xml, "./data/hora_inici"))
    
    # FLAGS for empty time and date (9999 means invalid)
    if hora_inici == '':
        hora_inici = "00.00"
    if date_inici == '':
        date_inici = "31/12/9999"
    if date_fi == '':
        date_fi = "31/12/9999"

    return Event.Builder() \
        .name(find_text(xml, "./nom")) \
        .place_name(find_text(xml, "./lloc_simple/nom")) \
        .place_street(find_text(xml, "./lloc_simple/adreca_simple/carrer")) \
        .place_number(find_text(xml, "./lloc_simple/adreca_simple/numero")) \
        .place_district(find_text(xml, "./lloc_simple/adreca_simple/districte")) \
        .date_range(datetime.datetime.strptime(date_inici, date_format), 
                    datetime.datetime.strptime(date_fi, date_format), 
                    datetime.datetime.strptime(hora_inici, hora_format)) \
        .build()


def get_float(xml, at):
    try:
        return float(xml.find("./lloc_simple/adreca_simple/coordenades/googleMaps").attrib[at])
    except ValueError as err:
        return 0.0

# ===== #
# DIARI #
# ===== #
# Diari -> Data inici & Data fi ignored
# Diari -> Do get lat & lon for bicing distance checking
def deserialize_event_diari(xml):
    full_date = str(find_text(xml, "./data/data_proper_acte"))
    full_date_format = "%d/%m/%Y %H.%M"
    return Event.Builder() \
        .name(find_text(xml, "./nom")) \
        .place_name(find_text(xml, "./lloc_simple/nom")) \
        .place_street(find_text(xml, "./lloc_simple/adreca_simple/carrer")) \
        .place_number(find_text(xml, "./lloc_simple/adreca_simple/numero")) \
        .place_district(find_text(xml, "./lloc_simple/adreca_simple/districte")) \
        .date(datetime.datetime.strptime(full_date, full_date_format)) \
        .coord(get_float(xml,"lat"), get_float(xml,"lon")) \
        .build()


# ======= #
# FILTERS #
# ======= #

def strip_accents(s):
    return ''.join(c for c in unicodedata.normalize('NFD', s) if unicodedata.category(c) != 'Mn')

def filter_keys(exp,act):
    if isinstance(exp, type(None)):
        return True
    # case AND
    elif isinstance(exp, list):
        return all(filter_keys(e, act) for e in exp)
    # case OR
    elif isinstance(exp, tuple):
        return any(filter_keys(e, act) for e in exp)
    # case BASE
    elif isinstance(exp, str):
        inside = lambda t: re.search(exp, t, re.IGNORECASE) # Matching without caring about casing or accents
        # Nom Activitat, Lloc on es realitza, Districte on es realitza
        return inside(act.name) or inside(act.place_name) or inside(act.place_district) \
        or inside(strip_accents(act.name)) or inside(strip_accents(act.place_name)) or inside(strip_accents(act.place_district)) # Not caring about accents

# (data_inici <= date <= date_fi and data_inici != null)
def filter_date(date, act):
    # Means data_inici is NULL/Does not exist
    if act.date_inici == "31/12/9999": 
        return False
    date_param = datetime.datetime.strptime(date,"%d/%m/%Y")
    return (act.date_inici <= date_param and date_param <= act.date_fi)


# =========== #
# XML PARSING #
# =========== #

URL_EVENTS_DIARS = "http://w10.bcn.es/APPS/asiasiacache/peticioXmlAsia?id=199"
URL_EVENTS_MENSUALS = "http://w10.bcn.es/APPS/asiasiacache/peticioXmlAsia?id=103"
URL_BICING = "http://wservice.viabicing.cat/getstations.php?v=1"

def xml_handler(url):
    socket = urllib.request.urlopen(url)
    xml = ET.fromstring(socket.read())
    socket.close()
    return xml

# date parameter is present
# Only show info related to events. Ignore distance/bicing stations !!
# Using URL_EVENTS_MENSUALS
if date:
    events_xml = xml_handler(URL_EVENTS_MENSUALS)
    events = [ a for a in map(deserialize_event_mensual, events_xml.iter("acte")) ]
    # Filter by keys (matching ands [] and ors ()) and filter by date (data_inici <= date <= date_fi and data_inici != null)
    filtre = lambda act: filter_keys(keys, act) and filter_date(date, act)
    data_events_mensuals = [a for a in events if filtre(a) ]
    to_html(data_events_mensuals, 1) # FLAG = 1, meaning bicing stations part will NOT be printed in the HTML

# date parameter is NOT present
# Show info related to events, and info related to bicing (punts 2 i 3). Ignore date !!
# Using URL_EVENTS_DIARIS
else:
    events_xml = xml_handler(URL_EVENTS_DIARS)
    events = [ a for a in map(deserialize_event_diari, events_xml.iter("acte")) ]
    bicing_xml = xml_handler(URL_BICING)
    bicing = [ a for a in map(deserialize_bicing, bicing_xml.iter("station")) ]
    filtre = lambda act: filter_keys(keys,act) # no filtrem per data
    data_events_diaris = [a for a in events if filtre(a)]

    # For each event that didnt get filtered by keys add stations to it
    # Only stations that are opened and are at dist <= distance (--distance parameter)
    # If station has slots --> add it to stations with slots
    # If station has bikes --> add it to stations with bikes
    for a in data_events_diaris:
        for s in bicing:
            if s.is_open() and check_dist(a, s, distance):
                if s.has_slots(): a.add_slots_bicing(s)
                if s.has_bikes(): a.add_bikes_bicing(s)
    to_html(data_events_diaris, 2) # FLAG = 2, meaning date range will NOT be printed and Bicing Sations part WILL in the HTML
