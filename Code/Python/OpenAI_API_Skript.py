########################################################
####### BA, Tim Krabbe, 2872187                 ########
####### Qualitative Inhaltsanalyse mit ChatGPT  ########
########################################################

# Ggf nicht alles auf einmal durchführen, sondern in den vorgegebenen Abschnitten; mögliche Verbindungsfehler

import pandas
import os
import openai

## Mein OpenAI API Key
openAIkey = "blabla"
openai.api_key = openAIkey



# Öffnen und lesen der TXT-Datei
with open("sample_1.txt", "r", encoding="utf-8") as file:
    text = file.read()

# Auteilen in Abschnitte
sections = text.split("[")
chunks = []
chunk_size = 2  # Anzahl der Abschnitte pro Chunk

for i in range(0, len(sections), chunk_size):
    chunk = "[{}]{}".format(i + 1, "".join(sections[i:i + chunk_size]))
    chunks.append(chunk)
    

# Codebuch in Objekt abspeichern
codebook = """
  In diesem Prompt findest du ein Codebuch als Tabelle im Markdownformat. Darin stehen Codes im Sinne einer deduktiven Kodierung, wie sie in einer qualitativen Inhaltsanalyse verwendet wird. Links in der Tabelle findet sich der Code, in der Mitte eine Beschreibung des Codes und rechts ein einfaches Beispiel einer passenden Textstelle. Die Tabelle ist in 3 Abschnitte aufgeteilt: Evidence, Process und Response. Die Codes dieser Abschnitte sind damit unterteilt in: \"evidence scepticism\": Skepsis bzgl. des Status und der Existenz des menschengemachten Klimawandels, \"process scepticism\": Skepsis bzgl. wiss. Vorgänge, die politischen Entscheidungen und deren Entscheidungsprozessen zum menschengemachten Klimawandel zugrunde liegen, \"response scepticism\": Skepsis bzgl. des politischen Vorgehens zur Bekämpfung des menschengemachten Klimawandels.
  | Code | Description | Example |
  | - | - | - |
  | ==EVIDENCE== |  |  |
  | UXWM |Unexceptional warming | \"There have always been warmer periods in history!\" |
  | WAST |Warming stopped | \"If you look at the temperatures you can see it hasn't gotten warmer the last two years.\" |
  | DAIC |Data inconclusive | \"The data doesn't proof there is climate change!\" |
  | CHNP |Climate change is a hoax/No problem \– no response needed | \"Climate Change is made up by the Elites!\" |
  | NPIW |No post-industrial warming | \"The current warming period has started way before the industrial revolution.\" |
  | ETNC |Entirely \‘natural\’ causes | \"The sun is giving off more heat than before.\" |
  | TETT |Too early to tell | \"We don't know if it might be a short warming period like we had before.\" |
  | PDNC |Predominantly \‘natural\’ causes  | \"CO2 might have a small impact, but it is negligible.\" |
  | NCCM |No CO2 causal mechanism | \"CO2 is not a pollutant and has nothing to do with warming!\" |
  | AHNP |Anthropogenic climate change is a hoax/No problem \– no response needed | \"There has always been Climate Change, even without humans\" |
  | NGIS |Negative impacts speculative | \"Nobody knows if it's all going to be bad, it's just speculation.\" |
  | SCPI |Significant positive impact | \"There are so many resources going to be accessible when the ice melts, we will highly profit.\" | 
  | ==PROCESS== |  |  |
  | PBNV |Peer review by \‘buddies\’/not all views are heard | "Climate Science is made by a small group of people.\" |
  | LCIE |A lucrative climate industry now exists | \"The Climate Change science is funded by the Green Industrial Complex!\" |
  | SMHE |Scientists manipulate/ hide the evidence | \"Scientists only present the data that serves their interests.\" |
  | CAFM |Climate activists seek fame and money | \"All these activists just want the attention they never got at home.\" |
  | CMOU |Computer modelling overrated and unreliable | \"We don't have the technology to look into the future.\" |
  | WGNS |World government agendas/ National sovereignty under threat | \"We won't let the green agenda of others dictate how we run our economy.\" |
  | IAHR |Irrationalism (alarmist, hysteria, \‘climate religion\’) characterises the debate | "We should not make politics based on what a bunch of screaming kids want.\" |
  | WERD |Wealth redistribution | \"Climate Change is used as a vehicle for fundamental socioeconomic changes and we won't let that happen.\" |
  | SGCA |Socialists and Greens drive the climate agenda | \"Instead of the socialist aproach of the left, we should trust in the markets to solve the problem.\" |
  | PIII |Political interference in IPCC | \"We won't let the IPCC dictate our politics! It is the political arm of the green industrial complex.\" |
  | DMAH |Decision-makers are hypocrites | \"Tell the workers from the coal mines how combatting climate change will affect their jobs!\" |
  | MSDP |Media sensationalism distorts public opinion | \"Climate change is vastly exaggerated by made up horror stories in the media.\" |
  | ==RESPONSE== |  |  |
  | EJNH |Economy and jobs should not be harmed | \"First and foremost we have to look after our workers, before making fast decisions on climate.\" |
  | WANU |Wait for global agreement/ no unilateral response | \"Why should we fight climate change when China pollutes way more than us and does nothing?\" |
  | PMRB |A pragmatic and measured response is best | \"We have to keep our economic stability in mind and act accordingly.\" |
  | CMNE |Climate migration should not be enabled | \"Climate Change will create a new migration crisis. We have to prepare and close our borders.\" |
  | GPNE |Gender policies should not be enabled | \"The statement that climate change affects women more than men is complete nonsense and dedicated action is not necessary.\" |
  | CPMM |Carbon pricing: a money making scheme | \"Carbon pricing is only costing the people while not making a difference.\" |
  | CPFB |Carbon pricing favours big (not small/medium) companies | \"Our small businesses will all suffer and go bankrupt because of carbon pricing.\" |
  | OPAF |Our poor are forgotten! | \"We spend all this money on climate change to safe the world but can't even end homelessness in Europe.\" |
  | CMOB |The costs of mitigation outweigh the benefits  | \"The price for fighting climate change is too high, we will loose money.\" | 
  | BICA |Better to invest in climate adaptation | \"We can't turn around anymore, we have to find a way to live with the coming changes.\" |
"""


# prompt abspeichern
system_message = [
    {
        "role": "system",
        "content": codebook
    },
    {
        "role": "user",
        "content": """Ich werde dir nun Redebeiträge zum Klimawandel vorlegen, die du auf diese Art analysieren sollst. 
                        Du sollst darin nach Textpassagen suchen, die auf Formen von Skepsis gegenüber dem Klimawandel schließen lassen. 
                        Benutze dafür das bereits eingelesene Codebuch und begründe deine Codeauswahl. 
                        Bitte stelle sicher, dass die Ausgabe für jede vorgenommene Kodierung das folgende Format hat:

                        Code: "Code X"
                        Textstelle: "kodierte Textstelle [Textnummer]"
                        Begründung: "Kodierungsbrgründung"

                        Bitte halte dich an dieses Format, um die Konsistenz der Ausgabe sicherzustellen.
                        
                        
                        Du sollst mir also den Code geben, den Satz, den du kodiert hast sowie die Nummer des Textes aus dem er stammt, und deine Kodierung dieser Textstelle anschließend begründen. 
                        Die Beiträge sind alle nummeriert und es können mehrere Codes pro Text vorkommen. 
                        Verhalte dich wie ein Sozialwissenschaftler, der einen Text im Zuge einer qualitativen Inhaltsanalyse deduktiv kodiert. Finde so viele Codes und Textstellen wie du kannst"
                    """
    }
]
#Rechtschreibfehler: Kodierungsbegründung

responses = []

# Schleife zum Einlesen der Chunks
for i in range(11): # 11 Mal wiederholen, weil 2 Abschnitte pro Chunk mit insgesamt 20 Abschnitten (10 chunks), einmal "zur Sicherheit"
    # Benutzeranfragen mit den Redebeiträgen im aktuellen Chunk
    user_messages = [
        {
            "role": "user",
            "content": chunks[i]
        }
    ]
    
    # Prompt zusammenführen
    input_messages = system_message + user_messages
    
    # Modellspezifikationen
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=input_messages,
        temperature=1,
        max_tokens=1024,
        top_p=1,
        frequency_penalty=0,
        presence_penalty=0
    )
    
    responses.append(response)
    
    
for response in responses:
    model_response = response.choices[0].message['content']
    # Die Antwort ausgeben
    print(model_response)
    
# Ich habe die Antworten dann aus dem Interactive Window von VS Code in eine .txt Datei kopiert; hätte ich auch direkt in eine .txt Datei schreiben können:
# file_path = "deduktive_Kodierung_Temp_1.txt"

# with open(file_path, "w", encoding="utf-8") as file:
#     file.write(model_response)

##### das ganze Wiederholen mit unterschiedlichen Temperaturen:

##### Temp: 0.0
responses = []

# Schleife zum Einlesen der Chunks
for i in range(11): 
    # Benutzeranfragen mit den Redebeiträgen im aktuellen Chunk
    user_messages = [
        {
            "role": "user",
            "content": chunks[i]
        }
    ]
    
    # Prompt zusammenführen
    input_messages = system_message + user_messages
    
    # Modellspezifikationen
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=input_messages,
        temperature=0,
        max_tokens=1024,
        top_p=1,
        frequency_penalty=0,
        presence_penalty=0
    )
    
    responses.append(response)
    
    
for response in responses:
    model_response = response.choices[0].message['content']
    # Die Antwort ausgeben
    print(model_response)
    
########################
##### Temp: 0.5
responses = []

# Schleife zum Einlesen der Chunks
for i in range(11):
    # Benutzeranfragen mit den Redebeiträgen im aktuellen Chunk
    user_messages = [
        {
            "role": "user",
            "content": chunks[i]
        }
    ]
    
    # Prompt zusammenführen
    input_messages = system_message + user_messages
    
    # Modellspezifikationen
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=input_messages,
        temperature=0.5,
        max_tokens=1024,
        top_p=1,
        frequency_penalty=0,
        presence_penalty=0
    )
    
    responses.append(response)
    
    
for response in responses:
    model_response = response.choices[0].message['content']
    # Die Antwort ausgeben
    print(model_response)



#####################################################################
################################## Induktive Kategorienbildung ###### 
  
########## Temp: 0.5, kleines Sample ##################

with open("sample_1.txt", "r", encoding="utf-8") as file: #kleines Sample
    text = file.read()

# Auteilen in Abschnitte
sections = text.split("[")
chunks = []
chunk_size = 4  # Anzahl der Abschnitte pro Chunk

for i in range(0, len(sections), chunk_size):
    chunk = "[{}]{}".format("", "".join(sections[i:i + chunk_size]))
    chunks.append(chunk)
        


# Prompt
system_message = [
    {
        "role": "system",
        "content":  """Ich werde dir nun einzelne Textabschnitte vorlegen, die Redebeiträge von Europaparlamentsabgeordneten darstellen. Ich möchte, dass du im Stil einer qualitativen Inhaltsanalyse, wie sie in den Sozialwissenschaften angewandt wird, eine induktive Kategorienbildung vornimmst. Die Texte beziehen sich allesamt auf den Klimawandel und seine Konsequenzen. Ich möchte, dass du Kategorien bildest, die die verschiedenen Haltungen zu diesem Thema widerspiegeln. Geh dabei vor wie ein Sozialwissenschaftler, der eine induktive Inhaltsanalyse vornimmt.
                        Bitte stelle sicher, dass die Ausgabe für jede vorgenommene Kodierung das folgende Format hat:

                        Kategorie: "Name der Kategorie"
                        Textstelle: "betreffende Textstelle [Textnummer]"
                        Begründung: "Begründung der Kategorie"
                    """
    }
]
    
    
responses = []

# Schleife zum Einlesen der Chunks
for i in range(5): # fünf Mal wiederholen, weil ein Chunk 4 Abschnitte beinhaltet und es insgesamt 20 sind, als fünf chunks, die eingelesen werden müssen
    user_messages = [
        {
            "role": "user",
            "content": chunks[i]
        }
    ]
    
    # Zusammenführen der Inputs zu finalem Prompt
    input_messages = system_message + user_messages
    
    # Modellspezifikationen
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=input_messages,
        temperature=0.5,
        max_tokens=1024,
        top_p=1,
        frequency_penalty=0,
        presence_penalty=0
    )
    
    responses.append(response)
    
# Kategorien, Textstellen und Begründungen als .txt ablegen, um später darauf zugreifen zu können
file_path = "kleinesSample_05"
with open(file_path, "w", encoding="utf-8") as file:
    for response in responses:
     model_response = response.choices[0].message['content']
     file.write(model_response + "\n")
     

# Kategorien aus .txt Datei wieder einlesen
def read_categories_from_file(Kategorien_Durchlauf_1):
    with open("kleinesSample_05.txt", 'r', encoding='utf-8') as file:
        lines = file.readlines()

    categories = []

    for line in lines:
        line = line.strip()
        if line.startswith("Kategorie:"):
            category_name = line[len("Kategorie:"):].strip()
            categories.append(category_name)

    return categories


categories = read_categories_from_file(file_path)

# Ausgabe der extrahierten Kategorien zur Kontrolle
for category in categories:
    print(category)
    

# Funktion zum Zusammenführen von Kategorien mit GPT-3.5 Turbo
def merge_categories_with_gpt(categories):
    # Kategorien in Untergruppen, soweit nötig
    max_tokens_per_request = 4096  # Setze die maximale Tokenanzahl pro Anfrage
    subgroups = [categories[i:i+max_tokens_per_request] for i in range(0, len(categories), max_tokens_per_request)]

    merged_categories = []

    # Schleife zum Einlesen der Kategorien in das MOdell
    for subgroup in subgroups:
        # Prompt
        prompt = f"Fasse jeweils ähnliche Kategorien in diesem Dokument in neue Kategorien zusammen. Gehe dabei wie ein Sozialwissenschaftler vor, der eine induktive Kategorienbildung im Sinne der qualitativen Inhaltsanalyse vornimmt und versucht die Anzahl der gefundenen Kategorien zu begrenzen: {', '.join(subgroup)}"
        
        # MOdellspezifikationen
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "You are a helpful assistant."},
                {"role": "user", "content": prompt}
            ],
            temperature = 0.5
        )
        
        # Erstellgun einer Liste mit den neuen Kategorien
        generated_text = response['choices'][0]['message']['content']
        merged_categories.extend([category.strip() for category in generated_text.split(",")])

    return merged_categories

# Durchführung der Funktion: Automatische Zusammenführung der Kategorien mit GPT-3.5 Turbo
automatically_merged_categories = merge_categories_with_gpt(categories)

print("Automatisch zusammengeführte Kategorien:")
print(automatically_merged_categories)

# Abspeichern der neuen Kategorien in .txt. Datei
file_path = "merged_categories_kleines_sample_05.txt"

categories_str = "\n".join(automatically_merged_categories)
with open(file_path, "w", encoding="utf-8") as file:
    file.write(categories_str)

#########################################################################################################
# Das Ganze wiederholt sich dann mit hier mit dem großen Sample, und später dann mit anderen Temperaturen
#########################################################################################################

#############################################
############################# 05 großes Sample
with open("sample_2.txt", "r", encoding="utf-8") as file: #großes Sample
    text = file.read()

# Auteilen in Abschnitte
sections = text.split("[")
chunks = []
chunk_size = 4  # Anzahl der Abschnitte pro Chunk

for i in range(0, len(sections), chunk_size):
    chunk = "[{}]{}".format("", "".join(sections[i:i + chunk_size]))
    chunks.append(chunk)
        


# Prompt
system_message = [
    {
        "role": "system",
        "content":  """Ich werde dir nun einzelne Textabschnitte vorlegen, die Redebeiträge von Europaparlamentsabgeordneten darstellen. Ich möchte, dass du im Stil einer qualitativen Inhaltsanalyse, wie sie in den Sozialwissenschaften angewandt wird, eine induktive Kategorienbildung vornimmst. Die Texte beziehen sich allesamt auf den Klimawandel und seine Konsequenzen. Ich möchte, dass du Kategorien bildest, die die verschiedenen Haltungen zu diesem Thema widerspiegeln. Geh dabei vor wie ein Sozialwissenschaftler, der eine induktive Inhaltsanalyse vornimmt.
                        Bitte stelle sicher, dass die Ausgabe für jede vorgenommene Kodierung das folgende Format hat:

                        Kategorie: "Name der Kategorie"
                        Textstelle: "betreffende Textstelle [Textnummer]"
                        Begründung: "Begründung der Kategorie"
                    """
    }
]
    
    

responses = []

# Schleife zum Einlesen der Chunks
for i in range(13): # 13 Mal, 4 Texte je Chunk bie insgesamt 50 Texten
    user_messages = [
        {
            "role": "user",
            "content": chunks[i]
        }
    ]
    
    input_messages = system_message + user_messages
    
    # Modellspezifikationen
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=input_messages,
        temperature=0.5,
        max_tokens=1024,
        top_p=1,
        frequency_penalty=0,
        presence_penalty=0
    )
    
    responses.append(response)
    
# Zwischenspeichern der Ergebnisse
file_path = "großesSample_05.txt"
with open(file_path, "w", encoding="utf-8") as file:
    for response in responses:
     model_response = response.choices[0].message['content']
     file.write(model_response + "\n")
     

    
def read_categories_from_file(Kategorien_Durchlauf_1):
    with open("großesSample_05.txt", 'r', encoding='utf-8') as file:
        lines = file.readlines()

    categories = []

    for line in lines:
        line = line.strip()
        if line.startswith("Kategorie:"):
            category_name = line[len("Kategorie:"):].strip()
            categories.append(category_name)

    return categories


categories = read_categories_from_file(file_path)

# Ausgabe der extrahierten Kategorien
for category in categories:
    print(category)

categories = read_categories_from_file(file_path)

# Funktion zum Zusammenführen von Kategorien mit GPT-3.5 Turbo
def merge_categories_with_gpt(categories):
    
    max_tokens_per_request = 4096  
    subgroups = [categories[i:i+max_tokens_per_request] for i in range(0, len(categories), max_tokens_per_request)]


    merged_categories = []


    for subgroup in subgroups:
        #Prompt
        prompt = f"Fasse jeweils ähnliche Kategorien in diesem Dokument in neue Kategorien zusammen. Gehe dabei wie ein Sozialwissenschaftler vor, der eine induktive Kategorienbildung im Sinne der qualitativen Inhaltsanalyse vornimmt und versucht die Anzahl der gefundenen Kategorien zu begrenzen: {', '.join(subgroup)}"
        
        # MOdellspezifikation
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "You are a helpful assistant."},
                {"role": "user", "content": prompt}
            ],
            temperature = 0.5
        )
        
        generated_text = response['choices'][0]['message']['content']
        merged_categories.extend([category.strip() for category in generated_text.split(",")])

    return merged_categories

# Automatische Zusammenführung der Kategorien mit GPT-3.5 Turbo
automatically_merged_categories = merge_categories_with_gpt(categories)

print(automatically_merged_categories) 

file_path = "merged_categories_großes_sample_05.txt"

categories_str = "\n".join(automatically_merged_categories)
with open(file_path, "w", encoding="utf-8") as file:
    file.write(categories_str)
    
    
##############################################
# induktiv großes Sample, Temp 1.0

with open("sample_2.txt", "r", encoding="utf-8") as file:
    text = file.read()

# Auteilen in Abschnitte
sections = text.split("[")
chunks = []
chunk_size = 4 

for i in range(0, len(sections), chunk_size):
    chunk = "[{}]{}".format("", "".join(sections[i:i + chunk_size]))
    chunks.append(chunk)
        

system_message = [
    {
        "role": "system",
        "content":  """Ich werde dir nun einzelne Textabschnitte vorlegen, die Redebeiträge von Europaparlamentsabgeordneten darstellen. Ich möchte, dass du im Stil einer qualitativen Inhaltsanalyse, wie sie in den Sozialwissenschaften angewandt wird, eine induktive Kategorienbildung vornimmst. Die Texte beziehen sich allesamt auf den Klimawandel und seine Konsequenzen. Ich möchte, dass du Kategorien bildest, die die verschiedenen Haltungen zu diesem Thema widerspiegeln. Geh dabei vor wie ein Sozialwissenschaftler, der eine induktive Inhaltsanalyse vornimmt.
                        Bitte stelle sicher, dass die Ausgabe für jede vorgenommene Kodierung das folgende Format hat:

                        Kategorie: "Name der Kategorie"
                        Textstelle: "betreffende Textstelle [Textnummer]"
                        Begründung: "Begründung der Kategorie"
                    """
    }
]
    
    
responses = []

# Schleife zum Einlesen der Chunks
for i in range(13):

    user_messages = [
        {
            "role": "user",
            "content": chunks[i]
        }
    ]
    
    # finaler Prompt
    input_messages = system_message + user_messages
    
    # Modellspezifikationen
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=input_messages,
        temperature=1.0,
        max_tokens=1024,
        top_p=1,
        frequency_penalty=0,
        presence_penalty=0
    )
    
    responses.append(response)
    
file_path = "großesSample_1.txt"
with open(file_path, "w", encoding="utf-8") as file:
    for response in responses:
     model_response = response.choices[0].message['content']
     file.write(model_response + "\n")
     

def read_categories_from_file(Kategorien_Durchlauf_1):
    with open("großesSample_1.txt", 'r', encoding='utf-8') as file:
        lines = file.readlines()

    categories = []

    for line in lines:
        line = line.strip()
        if line.startswith("Kategorie:"):
            category_name = line[len("Kategorie:"):].strip()
            categories.append(category_name)

    return categories

for category in categories:
    print(category)

categories = read_categories_from_file(file_path)

# Funktion zum Zusammenführen von Kategorien mit GPT-3.5 Turbo
def merge_categories_with_gpt(categories):
    
    max_tokens_per_request = 4096  
    subgroups = [categories[i:i+max_tokens_per_request] for i in range(0, len(categories), max_tokens_per_request)]

    
    merged_categories = []

    
    for subgroup in subgroups:
        # Prompt
        prompt = f"Fasse jeweils ähnliche Kategorien in diesem Dokument in neue Kategorien zusammen. Gehe dabei wie ein Sozialwissenschaftler vor, der eine induktive Kategorienbildung im Sinne der qualitativen Inhaltsanalyse vornimmt und versucht die Anzahl der gefundenen Kategorien zu begrenzen: {', '.join(subgroup)}"
        
        # MOdellspezifikationen
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "You are a helpful assistant."},
                {"role": "user", "content": prompt}
            ],
            temperature = 1.0
        )
        
        
        generated_text = response['choices'][0]['message']['content']
        merged_categories.extend([category.strip() for category in generated_text.split(",")])

    return merged_categories

# Automatische Zusammenführung der Kategorien mit GPT-3.5 Turbo
automatically_merged_categories = merge_categories_with_gpt(categories)

print("Automatisch zusammengeführte Kategorien:")
print(automatically_merged_categories)

file_path = "merged_categories_großes_sample_1.txt"

categories_str = "\n".join(automatically_merged_categories)
with open(file_path, "w", encoding="utf-8") as file:
    file.write(categories_str)

#########################################  induktiv, Temp 1.0, kleines Sample

with open("sample_1.txt", "r", encoding="utf-8") as file:
    text = file.read()

sections = text.split("[")
chunks = []
chunk_size = 4  

for i in range(0, len(sections), chunk_size):
    chunk = "[{}]{}".format("", "".join(sections[i:i + chunk_size]))
    chunks.append(chunk)

system_message = [
    {
        "role": "system",
        "content":  """Ich werde dir nun einzelne Textabschnitte vorlegen, die Redebeiträge von Europaparlamentsabgeordneten darstellen. Ich möchte, dass du im Stil einer qualitativen Inhaltsanalyse, wie sie in den Sozialwissenschaften angewandt wird, eine induktive Kategorienbildung vornimmst. Die Texte beziehen sich allesamt auf den Klimawandel und seine Konsequenzen. Ich möchte, dass du Kategorien bildest, die die verschiedenen Haltungen zu diesem Thema widerspiegeln. Geh dabei vor wie ein Sozialwissenschaftler, der eine induktive Inhaltsanalyse vornimmt.
                        Bitte stelle sicher, dass die Ausgabe für jede vorgenommene Kodierung das folgende Format hat:

                        Kategorie: "Name der Kategorie"
                        Textstelle: "betreffende Textstelle [Textnummer]"
                        Begründung: "Begründung der Kategorie"
                    """
    }
]
    
responses = []

# Schleife zum Einlesen der Chunks
for i in range(5):
    
    user_messages = [
        {
            "role": "user",
            "content": chunks[i]
        }
    ]
    
    # finaler Prompt
    input_messages = system_message + user_messages
    
    # MOdellspezifikationen
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=input_messages,
        temperature=1,
        max_tokens=1024,
        top_p=1,
        frequency_penalty=0,
        presence_penalty=0
    )
    
    responses.append(response)
    
    
for response in responses:
    model_response = response.choices[0].message['content']
    # Die Antwort ausgeben
    print(model_response)


# Funktion zum Einlesen der Kategorien aus einer txt-Datei, nur Kategorien
def read_categories_from_file(Kategorien_Durchlauf_1):
    with open("Kategorien_Durchlauf_2.txt", 'r', encoding='utf-8') as file:
        lines = file.readlines()

    categories = []

    for line in lines:
        line = line.strip()
        if line.startswith("Kategorie:"):
            category_name = line[len("Kategorie:"):].strip()
            categories.append(category_name)

    return categories

# Ausgabe der extrahierten Kategorien
for category in categories:
    print(category)

# Funktion zum Zusammenführen von Kategorien mit GPT-3.5 Turbo
def merge_categories_with_gpt(categories):
    
    max_tokens_per_request = 4096 
    subgroups = [categories[i:i+max_tokens_per_request] for i in range(0, len(categories), max_tokens_per_request)]

    merged_categories = []

    
    for subgroup in subgroups:
        # Prompt
        prompt = f"Fasse jeweils ähnliche Kategorien in diesem Dokument in neue Kategorien zusammen. Gehe dabei wie ein Sozialwissenschaftler vor, der eine induktive Kategorienbildung im Sinne der qualitativen Inhaltsanalyse vornimmt und versucht die Anzahl der gefundenen Kategorien zu begrenzen: {', '.join(subgroup)}"
        
        # Modellspezifikationen
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "You are a helpful assistant."},
                {"role": "user", "content": prompt}
            ],
            temperature = 1.0
        )
        
        generated_text = response['choices'][0]['message']['content']
        merged_categories.extend([category.strip() for category in generated_text.split(",")])

    return merged_categories

# Automatische Zusammenführung der Kategorien mit GPT-3.5 Turbo
automatically_merged_categories = merge_categories_with_gpt(categories)

print("Automatisch zusammengeführte Kategorien:")
print(automatically_merged_categories)


file_path = "merged_categories_kleines_sample_1.txt"

categories_str = "\n".join(automatically_merged_categories)
with open(file_path, "w", encoding="utf-8") as file:
    file.write(categories_str)
    


##############################
# induktiv, kleines Sample 1.5

with open("sample_1.txt", "r", encoding="utf-8") as file:
    text = file.read()

# Auteilen in Abschnitte
sections = text.split("[")
chunks = []
chunk_size = 4  # Anzahl der Abschnitte pro Chunk

for i in range(0, len(sections), chunk_size):
    chunk = "[{}]{}".format("", "".join(sections[i:i + chunk_size]))
    chunks.append(chunk)
        
system_message = [
    {
        "role": "system",
        "content":  """Ich werde dir nun einzelne Textabschnitte vorlegen, die Redebeiträge von Europaparlamentsabgeordneten darstellen. Ich möchte, dass du im Stil einer qualitativen Inhaltsanalyse, wie sie in den Sozialwissenschaften angewandt wird, eine induktive Kategorienbildung vornimmst. Die Texte beziehen sich allesamt auf den Klimawandel und seine Konsequenzen. Ich möchte, dass du Kategorien bildest, die die verschiedenen Haltungen zu diesem Thema widerspiegeln. Geh dabei vor wie ein Sozialwissenschaftler, der eine induktive Inhaltsanalyse vornimmt.
                        Bitte stelle sicher, dass die Ausgabe für jede vorgenommene Kodierung das folgende Format hat:

                        Kategorie: "Name der Kategorie"
                        Textstelle: "betreffende Textstelle [Textnummer]"
                        Begründung: "Begründung der Kategorie"
                    """
    }
]
    
responses = []

# Schleife zum Einlesen der Chunks
for i in range(5):
    user_messages = [
        {
            "role": "user",
            "content": chunks[i]
        }
    ]
    
    input_messages = system_message + user_messages
    
    # MOdellspezifikationen
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=input_messages,
        temperature=1.5,
        max_tokens=1024,
        top_p=1,
        frequency_penalty=0,
        presence_penalty=0
    )
    
    responses.append(response)
    
file_path = "kleinesSample_15.txt"
with open(file_path, "w", encoding="utf-8") as file:
    for response in responses:
     model_response = response.choices[0].message['content']
     file.write(model_response + "\n")
     

    
def read_categories_from_file(Kategorien_Durchlauf_1):
    with open("kleinesSample_15.txt", 'r', encoding='utf-8') as file:
        lines = file.readlines()

    categories = []

    for line in lines:
        line = line.strip()
        if line.startswith("Kategorie:"):
            category_name = line[len("Kategorie:"):].strip()
            categories.append(category_name)

    return categories

categories = read_categories_from_file(file_path)

# Ausgabe der extrahierten Kategorien
for category in categories:
    print(category)


# Funktion zum Zusammenführen von Kategorien mit GPT-3.5 Turbo
def merge_categories_with_gpt(categories):
    
    max_tokens_per_request = 4096 
    subgroups = [categories[i:i+max_tokens_per_request] for i in range(0, len(categories), max_tokens_per_request)]

    merged_categories = []

    for subgroup in subgroups:
        # Prompt
        prompt = f"Fasse jeweils ähnliche Kategorien in diesem Dokument in neue Kategorien zusammen. Gehe dabei wie ein Sozialwissenschaftler vor, der eine induktive Kategorienbildung im Sinne der qualitativen Inhaltsanalyse vornimmt und versucht die Anzahl der gefundenen Kategorien zu begrenzen: {', '.join(subgroup)}"
        
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "You are a helpful assistant."},
                {"role": "user", "content": prompt}
            ],
            temperature = 1.5
        )
        
        generated_text = response['choices'][0]['message']['content']
        merged_categories.extend([category.strip() for category in generated_text.split(",")])

    return merged_categories

# Automatische Zusammenführung der Kategorien mit GPT-3.5 Turbo
automatically_merged_categories = merge_categories_with_gpt(categories)

print(automatically_merged_categories) 

file_path = "merged_categories_kleines_sample_15.txt"

categories_str = "\n".join(automatically_merged_categories)
with open(file_path, "w", encoding="utf-8") as file:
    file.write(categories_str)
    
    
    
##############################
# induktiv, großes Sample 1.5

with open("sample_2.txt", "r", encoding="utf-8") as file:
    text = file.read()

# Auteilen in Abschnitte
sections = text.split("[")
chunks = []
chunk_size = 4  # Anzahl der Abschnitte pro Chunk

for i in range(0, len(sections), chunk_size):
    chunk = "[{}]{}".format("", "".join(sections[i:i + chunk_size]))
    chunks.append(chunk)
        


# Prompt
system_message = [
    {
        "role": "system",
        "content":  """Ich werde dir nun einzelne Textabschnitte vorlegen, die Redebeiträge von Europaparlamentsabgeordneten darstellen. Ich möchte, dass du im Stil einer qualitativen Inhaltsanalyse, wie sie in den Sozialwissenschaften angewandt wird, eine induktive Kategorienbildung vornimmst. Die Texte beziehen sich allesamt auf den Klimawandel und seine Konsequenzen. Ich möchte, dass du Kategorien bildest, die die verschiedenen Haltungen zu diesem Thema widerspiegeln. Geh dabei vor wie ein Sozialwissenschaftler, der eine induktive Inhaltsanalyse vornimmt.
                        Bitte stelle sicher, dass die Ausgabe für jede vorgenommene Kodierung das folgende Format hat:

                        Kategorie: "Name der Kategorie"
                        Textstelle: "betreffende Textstelle [Textnummer]"
                        Begründung: "Begründung der Kategorie"
                    """
    }
]
    
    
    
    
responses = []

# Schleife zum Einlesen der Chunks
for i in range(13):
    # Benutzeranfragen mit den Redebeiträgen im aktuellen Chunk
    user_messages = [
        {
            "role": "user",
            "content": chunks[i]
        }
    ]

    input_messages = system_message + user_messages
    
    # Modellspezifikationen
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=input_messages,
        temperature=1.5,
        max_tokens=1024,
        top_p=1,
        frequency_penalty=0,
        presence_penalty=0
    )
    
    responses.append(response)
    
file_path = "großesSample_15.txt"
with open(file_path, "w", encoding="utf-8") as file:
    for response in responses:
     model_response = response.choices[0].message['content']
     file.write(model_response + "\n")
     

def read_categories_from_file(Kategorien_Durchlauf_1):
    with open("großesSample_15.txt", 'r', encoding='utf-8') as file:
        lines = file.readlines()

    categories = []

    for line in lines:
        line = line.strip()
        if line.startswith("Kategorie:"):
            category_name = line[len("Kategorie:"):].strip()
            categories.append(category_name)

    return categories

categories = read_categories_from_file(file_path)

# Ausgabe der extrahierten Kategorien
for category in categories:
    print(category)
    

categories = read_categories_from_file(file_path)

# Funktion zum Zusammenführen von Kategorien mit GPT-3.5 Turbo
def merge_categories_with_gpt(categories):
    max_tokens_per_request = 4096  
    subgroups = [categories[i:i+max_tokens_per_request] for i in range(0, len(categories), max_tokens_per_request)]

    merged_categories = []

    for subgroup in subgroups:
        # Prompt
        prompt = f"Fasse jeweils ähnliche Kategorien in diesem Dokument in neue Kategorien zusammen. Gehe dabei wie ein Sozialwissenschaftler vor, der eine induktive Kategorienbildung im Sinne der qualitativen Inhaltsanalyse vornimmt und versucht die Anzahl der gefundenen Kategorien zu begrenzen: {', '.join(subgroup)}"
        
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "You are a helpful assistant."},
                {"role": "user", "content": prompt}
            ],
            temperature = 1.5
        )
        
        generated_text = response['choices'][0]['message']['content']
        merged_categories.extend([category.strip() for category in generated_text.split(",")])

    return merged_categories

# Automatische Zusammenführung der Kategorien mit GPT-3.5 Turbo
automatically_merged_categories = merge_categories_with_gpt(categories)

print(automatically_merged_categories) 

file_path = "merged_categories_großes_sample_15.txt"

categories_str = "\n".join(automatically_merged_categories)
with open(file_path, "w", encoding="utf-8") as file:
    file.write(categories_str)