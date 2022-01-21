"""
define a list of dictionaries with revalent fields as key
define function:
    import data
    go through datafile: FOR each observation, IF there is something for brith year AND death year AND death Cause:
        add each relevant section onto list of dictionaries
for loop (going through multiple json files):
    call function for each letter"_people.json"
for each dictionary in list of dictionaries, upload onto csv
"""
import json
# Creates the list that will contain the extracted relevant data, as well as the list of labels of relevant data
deadfamouspeople = []
variables = ["title","ontology/gender_label", "ontology/nationality_label", "ontology/occupation_label", "ontology/ethnicity_label", "ontology/birthYear","ontology/deathCause_label", "ontology/deathDate", "ontology/deathYear", "ontology/deathPlace_label"]
# Creates a function that dextracts revalent data from a file in the dataset, by dictionary. Relevant data is considered people that have both a birthyear and a death year, and which are not fictional characters
def extractdataset(name):
    with open(name) as jsonfile:
        data = json.load(jsonfile)
    for person in data:
        if "ontology/birthYear" in person.keys() and "ontology/deathYear" in person.keys() and "fictional character" not in person["http://www.w3.org/1999/02/22-rdf-syntax-ns#type_label"]:
                # making dictionary including revalent variables per dataset
                dictionary = {}
                for category in variables:
                    if category in person.keys():
                        dictionary[category] = person[category]
                deadfamouspeople.append(dictionary)
#Runs function for all the files in the dataset
for letter in "ABCDEFGHIJKLMNOPQRSTUVWXYZ":
   extractdataset(f"dataset\{letter}_people.json")
   print(letter)

#Writes list of relevant data into csv format
with open('famous_dead_people.csv', 'w', encoding="utf8") as writing_file:
    writing_file.write('Name,Gender,Nationality,Occupation,Ethnicity,birthYear,deathCause,deathDate,deathYear,deathPlace\n')
    for person in deadfamouspeople:
        line = ""
        for key in variables:
            if key in person.keys():
                csvformat = str(person[key]).replace(",","+")
                line = line + f'{csvformat},'
            else:
                line = line + ","
        line = line[:-1]
        writing_file.write(line + '\n')