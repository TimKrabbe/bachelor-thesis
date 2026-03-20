###################################################
# Bachelorarbeit Tim Krabbe, 2872187              #
# ChatGPT in der qualitativen Sozialforschung     #
###################################################

# install.packages("RSelenium")
# install.packages("future.apply")

library(RSelenium)
library(tidyverse)
library(rvest)
library(stringr)
library(future.apply)
library(netstat)
library(tidyr)

##### Datensätze, diverse ZWischenstände etc.. Nicht im Anhang enthalten, außer das Endergebnis. 
# load("~/Studium/SoWi-Bachelor/Semester 6/Kolle/df.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/df_right.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/cl.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/exvote_links_3.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/exvote_links_wo.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/exvote_vorläufig.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/all_6.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/exvote_links_4.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/exvote_vorläufig2.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/df_7_right.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/df_7.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/cl_7.Rda")
# 
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/df_8_right.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/df_8.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/cl_8.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/titles2_df_8.csv")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/result_8.rda")


##################################################
######### Achte Legislaturperiode ################
##################################################

##### html-page einlesen
link = "https://www.europarl.europa.eu/meps/en/directory/8"
page = read_html(link)

##### Namen der Abgeordneten auslesen und in dataframe speichern
name = page %>% html_nodes("#docMembersList .t-item") %>% html_text()
df <- data.frame(name)

page %>% html_nodes(".t-y-block")

##### Namen vorbereiten für die Anspielung an die Link erpl_member-list-item-content mb-3 t-y-block
df$name <- gsub(" ", "_", df$name)

##### Links zu den Abgeordneten auslesen
links1 = page %>% html_nodes(".t-y-block")

#### 7 Links zu viel ausgelesen
links1[c(1,2,3,4,5,6,7)]
mep_links = page %>% html_nodes(".t-y-block") %>%
  html_attr("href")

#### Entsprechende Links rausschmeißen
mep_links <- mep_links[-c(1,2,3,4,5,6,7)]

###### Links für die 8. Legislaturperiode ranspielen
df$meplinks_8 <- mep_links

###### Vollständige Links auslesen, in Chunks für übersichtlichere Rechenzeiten
mep_test <- mep_links[seq(1,200,1)]
mep_test2 <- mep_links[seq(201,400,1)]
mep_test3 <- mep_links[seq(401,600,1)]
mep_test4 <- mep_links[seq(601,800,1)]
mep_test5 <- mep_links[seq(801,860,1)]

get_correct_link = function(mep_test) {
  mep_page = read_html(mep_test)
  correct_link = mep_page %>% html_nodes("link") %>% html_attr("href") %>% 
    paste(collapse = ", ") %>% 
    str_extract(., "[^,]*")
  return(correct_link)
}

get_correct_link2 = function(mep_test2) {
  mep_page = read_html(mep_test2)
  correct_link = mep_page %>% html_nodes("link") %>% html_attr("href") %>% 
    paste(collapse = ", ") %>% 
    str_extract(., "[^,]*")
  return(correct_link)
}

get_correct_link3 = function(mep_test3) {
  mep_page = read_html(mep_test3)
  correct_link = mep_page %>% html_nodes("link") %>% html_attr("href") %>% 
    paste(collapse = ", ") %>% 
    str_extract(., "[^,]*")
  return(correct_link)
}

get_correct_link4 = function(mep_test4) {
  mep_page = read_html(mep_test4)
  correct_link = mep_page %>% html_nodes("link") %>% html_attr("href") %>% 
    paste(collapse = ", ") %>% 
    str_extract(., "[^,]*")
  return(correct_link)
}

get_correct_link5 = function(mep_test5) {
  mep_page = read_html(mep_test5)
  correct_link = mep_page %>% html_nodes("link") %>% html_attr("href") %>% 
    paste(collapse = ", ") %>% 
    str_extract(., "[^,]*")
  return(correct_link)
}

correct_link = future_sapply(mep_test, FUN = get_correct_link)
correct_link2 = future_sapply(mep_test2, FUN = get_correct_link2)
correct_link3 = future_sapply(mep_test3, FUN = get_correct_link3)
correct_link4 = future_sapply(mep_test4, FUN = get_correct_link4)
correct_link5 = future_sapply(mep_test5, FUN = get_correct_link5)
##### Das ganze in Stückchen probieren, Internetverbindung bricht sonst ggf. ab

correct_links_complete <- c(correct_link,correct_link2,correct_link3,correct_link4, correct_link5)
df$meplinks_complete <- correct_links_complete

#save(df,file="df_8.Rda")

####################################################################
# Links ergänzen
####################################################################

mep_links_speech <- correct_links_complete %>% str_sub(0,-10)

wrongsends <- mep_links_speech[!str_ends(mep_links_speech, "/")]

mep_links_speech <- ifelse(!str_ends(mep_links_speech, "/") == TRUE,
                           paste(mep_links_speech, "/all-activities/plenary-speeches/8", sep = ""),
                           paste(mep_links_speech, "all-activities/plenary-speeches/8", sep = ""))


df$links_all_speeches <- mep_links_speech

# .mt-3 .btn-default
df$meplinks_complete <- gsub("home", "history/8#detailedcardmep", df$meplinks_complete)

df$meplinks_complete <- gsub("history/6", "history/8", df$meplinks_complete)
df$meplinks_complete <- gsub("history/7", "history/8", df$meplinks_complete)
df$meplinks_complete <- gsub("history/9", "history/8", df$meplinks_complete)

meplinks_complete <- df$meplinks_complete

mep_com1 <- meplinks_complete[seq(1,200,1)]
mep_com2 <- meplinks_complete[seq(201,400,1)]
mep_com3 <- meplinks_complete[seq(401,600,1)]
mep_com4 <- meplinks_complete[seq(601,800,1)]
mep_com5 <- meplinks_complete[seq(801,860,1)]


# Parteien der MEPs auslesen
get_party1 = function(mep_com1) {
  mep_page = read_html(mep_com1)
  party = mep_page %>% html_nodes(".erpl_meps-status:nth-child(2) li") %>% html_text() %>% 
    paste(collapse = ", ")
  return(party)
}

get_party2 = function(mep_com2) {
  mep_page = read_html(mep_com2)
  party = mep_page %>% html_nodes(".erpl_meps-status:nth-child(2) li") %>% html_text() %>% 
    paste(collapse = ", ")
  return(party)
}

get_party3 = function(mep_com3) {
  mep_page = read_html(mep_com3)
  party = mep_page %>% html_nodes(".erpl_meps-status:nth-child(2) li") %>% html_text() %>% 
    paste(collapse = ", ")
  return(party)
}

get_party4 = function(mep_com4) {
  mep_page = read_html(mep_com4)
  party = mep_page %>% html_nodes(".erpl_meps-status:nth-child(2) li") %>% html_text() %>% 
    paste(collapse = ", ")
  return(party)
}

get_party5 = function(mep_com5) {
  mep_page = read_html(mep_com5)
  party = mep_page %>% html_nodes(".erpl_meps-status:nth-child(2) li") %>% html_text() %>% 
    paste(collapse = ", ")
  return(party)
}


party1 = future_sapply(mep_com1, FUN = get_party1)
party2 = future_sapply(mep_com2, FUN = get_party2)
party3 = future_sapply(mep_com3, FUN = get_party3)
party4 = future_sapply(mep_com4, FUN = get_party4)
party5 = future_sapply(mep_com5, FUN = get_party5)

party <- c(party1, party2, party3, party4, party5)
df$party <- party
save(df,file="df_8.Rda")

# MEPs ohne scrapebare Partei manuell überprüfen
df_8_miss <- subset(df, party == "")
df_8_miss <- df_8_miss$meplinks_complete

df_8_miss <- df_8_miss %>% str_sub(0,-18)
df_8_miss <- df_8_miss %>% paste("8", sep = "")

df_8_miss <- data_frame(df_8_miss)

#############################################################
# Parteien filtern
#############################################################
# "Front National", "United Kingdom Independence Party", "Freiheitliche Partei Österreichs", "Lega Nord per l'indipendenza della Padania", "Les Patriotes"
# "Jobbik Magyarországért Mozgalom", "Dansk Folkeparti", "Slovenská národná strana", "Hrvatska stranka prava", "Naprzód Polsko", "Laikos Orthodoxos Synagermos",
# "Perussuomalaiset", "Liga Polskich Rodzin", "Vlaams Blok", "Vlaams Belang", "Artikel 50", "Brexit Party", "Partidul România Mare", "Laikos Syndesmos", 
# "British National Party", " Movimento Sociale Fiamma tricolore", "Elliniki Lisi", "Nationaldemokratische Partei Deutschlands"


party_strings <- c("Front national", "United Kingdom Independence Party", "Freiheitliche Partei Österreichs", "Lega Nord per l'indipendenza della Padania", 
                   "Les Patriotes", "Jobbik Magyarországért Mozgalom", "Dansk Folkeparti", "Slovenská národná strana", "Hrvatska stranka prava", 
                   "Naprzód Polsko", "Laikos Orthodoxos Synagermos","Perussuomalaiset", "Liga Polskich Rodzin", "Vlaams Blok", "Vlaams Belang", 
                   "Artikel 50", "Brexit Party", "Partidul România Mare", "Laikos Syndesmos", 
                   "British National Party", " Movimento Sociale Fiamma tricolore", "Elliniki Lisi", 
                   "Nationaldemokratische Partei Deutschlands", "Hrvatska konzervativna stranka", "Popular Orthodox Rally", "UK Independence Party", 
                   "Lega Nord", "Partij voor de Vrijheid", "Golden Dawn")

df$party_name <- substr(df$party, 26, nchar(df$party))
df_8_right <- df %>% filter(str_detect(party, paste(party_strings, collapse = "|")))

df_8_miss <- df_8_miss %>% filter(str_detect(df_7_miss, paste(party_strings, collapse = "|")))
#cool

save(df,file="df_8.Rda")
save(df_8_right,file="df_8_right.Rda")

dfp <- data.frame(party)

##############################################################
# Links zu den Reden 
##############################################################

# .mt-3 .btn-default
# 
meplinks_complete <- df_8_right$meplinks_complete
# meplinks_complete <- meplinks_complete %>% str_sub(0,-2)
# meplinks_complete <- meplinks_complete %>% paste("7", sep = "")

get_speech_link = function(meplinks_complete) {
  mep_page = read_html(meplinks_complete)
  speech_link = mep_page %>% html_nodes(".mt-3 .btn-default") %>% html_attr("href") %>% 
    paste(collapse = ", ")
  return(speech_link)
}

speech_links = future_sapply(meplinks_complete, FUN = get_speech_link)
df_8_right$speech_links <- df_8_right$links_all_speeches

save(df_8_right,file="df_8_right.Rda")

########################
# remote server
rs_driver_object <- rsDriver(
  browser = "firefox",
  chromever = NULL,
  verbose = F,
  port = free_port(random = T)
)

# browser öffnen
remDr <- rs_driver_object$client #das hier geöffnete Fenster muss geschlossen werden
remDr$open()
# 

#cookies akzeptieren
remDr$navigate(df_8_right$speech_links[1])
cookie <- remDr$findElement(using = "css selector", "button.epjs_agree:nth-child(2) > span:nth-child(1)")
cookie$clickElement()

# aufteilen in Seiten mit load more und ohne load more button
get_load = function(speech_links) {
  remDr$navigate(speech_links)
  if(length(remDr$findElements(using = "css selector", "button.btn")) != 0) {
    return(remDr$getCurrentUrl())
  } else {
    return(NA)
  }
}
################
df_8_right$speech_links[46] <- "https://www.europarl.europa.eu/meps/en/189065/FRANCE_JAMET/all-activities/plenary-speeches/8"
df_8_right$speech_links[51] <- "https://www.europarl.europa.eu/meps/en/192635/DANILO+OSCAR_LANCINI/all-activities/plenary-speeches/8"

df_8_right$ex_links[46] <- "https://www.europarl.europa.eu/meps/en/189065/FRANCE_JAMET/all-activities/written-explanations/8"
df_8_right$ex_links[51] <- "https://www.europarl.europa.eu/meps/en/192635/DANILO+OSCAR_LANCINI/all-activities/written-explanations/8"
df_8_right$ex_links[73] <- "https://www.europarl.europa.eu/meps/en/96996/GIANCARLO_SCOTTA/all-activities/written-explanations/8"


speech_links <- df_8_right$speech_links
load <- future_sapply(speech_links, FUN = get_load)
load_2 <- names(load)

df_8_right$speech_load <- load
df_8_right$speech_noload <- ifelse(is.na(df_8_right$speech_load), df_8_right$speech_links, NA)

#save(df_8_right,file="df_right_8.Rda")

noload_8 <- drop_na(df_8_right, speech_noload)
noload_8 <- noload_8$speech_noload

df_8_right$speech_load <- as.character(df_8_right$speech_load)
df_8_right$speech_load[df_8_right$speech_load == "NA"] <- NA
# 
load_8 <- drop_na(df_8_right, speech_load)
load_8 <- load_8$speech_load


##################
# Texte für MEPs deren Seiten keinen load more button haben runterladen

get_speech1 <- function(url) {
  remDr$navigate(url)
  
  max_index <- 1000
  element_data <- list()
  
  for (index in 1:max_index) {
    selector <- paste0("div.erpl_search-results-list-expandable-block:nth-child(", index, ") > div:nth-child(1) > div:nth-child(1) > h3:nth-child(1) > a:nth-child(1) > span:nth-child(1)")
    
    elem <- remDr$findElements(using = "css selector", selector)
    
    if (length(elem) == 0) {
      break
    }
    
    element_text <- sapply(elem, function(element) {
      element$getElementAttribute("innerText")[[1]]
    })
    
    link_selector <- paste0("div.erpl_search-results-list-expandable-block:nth-child(", index, ") > div:nth-child(1) > div:nth-child(1) > h3:nth-child(1) > a:nth-child(1)")
    
    link_elem <- remDr$findElements(using = "css selector", link_selector)
    
    element_link <- sapply(link_elem, function(element) {
      element$getElementAttribute("href")
    })

    element_data[[index]] <- data.frame(Text = element_text, Link = element_link)
  }
  
  result_df <- bind_rows(element_data)
  return(result_df)
}
##################
# Über den Vektor noload_7_vec itererieren und Ergebnisse in Liste
dataframe_list <- lapply(noload_8, get_speech1)

# Extrahiere nicht leere Texte und Links aus den Dataframes
all_texts <- character(0)
all_links <- character(0)

# data clean up, Daten entpacken
for (df in dataframe_list) {
  link_columns <- grep("^X\\.https", colnames(df), value = TRUE)
  
  not_empty_texts <- df$Text[!is.na(df$Text)]
  
  not_empty_links <- unlist(df[link_columns], use.names = FALSE)
  not_empty_links <- not_empty_links[!is.na(not_empty_links)]
  
  all_texts <- c(all_texts, not_empty_texts)
  all_links <- c(all_links, not_empty_links)
}

result_df_8 <- data.frame(Text = all_texts, Link = all_links)

######## selbes Spiel für die Seiten mit load more button
i = 1

get_speech2 = function(url) {
  remDr$navigate(url)
  button <- remDr$findElement(using = "css selector", "button.btn")
  try(for(i in 1:999) { # try Schleife, klickt auf load button, bis er nicht mehr da ist und dann wird erst runtergeladen
    button$clickElement()
    button1 <- remDr$findElement(using = "css selector", "button.btn")
    button1$clickElement()
    i = i+1
  })
  max_index <- 10000
  element_data <- list()
  for (index in 1:max_index) {
    selector <- paste0("div.erpl_search-results-list-expandable-block:nth-child(", index, ") > div:nth-child(1) > div:nth-child(1) > h3:nth-child(1) > a:nth-child(1) > span:nth-child(1)")
    
    elem <- remDr$findElements(using = "css selector", selector)
    
    if (length(elem) == 0) {
      break
    }
    
    element_text <- sapply(elem, function(element) {
      element$getElementAttribute("innerText")[[1]]
    })
    
    link_selector <- paste0("div.erpl_search-results-list-expandable-block:nth-child(", index, ") > div:nth-child(1) > div:nth-child(1) > h3:nth-child(1) > a:nth-child(1)")
    
    link_elem <- remDr$findElements(using = "css selector", link_selector)
    
    element_link <- sapply(link_elem, function(element) {
      element$getElementAttribute("href")
    })
    
    element_data[[index]] <- data.frame(Text = element_text, Link = element_link)
  }
  
  result_df <- bind_rows(element_data)
  return(result_df)
}
#############

load_8 <- load_8[-73]
dataframe_list_8_2 <- future_lapply(load_8, get_speech2)

all_texts <- character(0)
all_links <- character(0)

for (df in dataframe_list_8_2) {
  link_columns <- grep("^X\\.https", colnames(df), value = TRUE)
  
  not_empty_texts <- df$Text[!is.na(df$Text)]
  
  not_empty_links <- unlist(df[link_columns], use.names = FALSE)
  not_empty_links <- not_empty_links[!is.na(not_empty_links)]
  
  all_texts <- c(all_texts, not_empty_texts)
  all_links <- c(all_links, not_empty_links)
}


result_df_8_2 <- data.frame(Text = all_texts, Link = all_links)
result_df_8_c <- rbind(result_df_8, result_df_8_2)

#save(result_df_8_c, file = "result_8.rda")


############## Titel filtern

keywords <- c("CO2", "co2", "CO²", "co²", "Co2", "Co²", "carbon dioxide", "Carbon dioxide", "Carbon Dioxide", "global warming", "Global Warming", "Global warming", "Climate", "climate", "Greenhouse", "greenhouse",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "Kohlendioxid", "kohlendioxid", "globale Erwärmung", "Globale Erwärmung", "globale erwärmung", "Klima", "klima", "Treibhaus", "treibhaus",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "dioxyde de carbone", "Dioxyde de carbone", "dioxyde de Carbone", "réchauffement climatique", "Réchauffement climatique", "réchauffement climatique", "climat", "Climat", "serre", "Serre",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "dióxido de carbono", "Dióxido de carbono", "dióxido de Carbono", "calentamiento global", "Calentamiento global", "calentamiento global", "clima", "Clima", "invernadero", "Invernadero",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "anidride carbonica", "Anidride carbonica", "anidride Carbonica", "riscaldamento globale", "Riscaldamento globale", "riscaldamento Globale", "clima", "Clima", "serra", "Serra",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "dwutlenek węgla", "Dwutlenek węgla", "dwutlenek Węgla", "globalne ocieplenie", "Globalne ocieplenie", "globalne Ocieplenie", "klimat", "Klimat", "szklarnia", "Szklarnia",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "dióxido de carbono", "Dióxido de carbono", "dióxido de Carbono", "aquecimento global", "Aquecimento global", "aquecimento Global", "clima", "Clima", "estufa", "Estufa",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "koolstofdioxide", "Koolstofdioxide", "koolstofdioxide", "opwarming van de aarde", "Opwarming van de aarde", "opwarming van de aarde", "klimaat", "Klimaat", "kas", "Kas",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "koldioxid", "Koldioxid", "koldioxid", "global uppvärmning", "Global uppvärmning", "global uppvärmning", "klimat", "Klimat", "växthus", "Växthus",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "въглероден диоксид", "Въглероден диоксид", "ВЪГЛЕРОДЕН ДИОКСИД", "глобално затопляне", "Глобално затопляне", "ГЛОБАЛНО ЗАТОПЛЯНЕ", "климат", "Климат", "топлица", "Топлица",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "kuldioxid", "Kuldioxid", "KULDIOXID", "global opvarmning", "Global opvarmning", "GLOBAL OPVARMNING", "klima", "Klima", "drivhus", "Drivhus",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "süsihappegaas", "Süsihappegaas", "SÜSIHAPPEGAAS", "globaalne soojenemine", "Globaalne soojenemine", "GLOBAALNE SOOJENEMINE", "kliima", "Kliima", "kasvuhoone", "Kasvuhoone",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "süsihappegaas", "Süsihappegaas", "SÜSIHAPPEGAAS", "globaalne soojenemine", "Globaalne soojenemine", "GLOBAALNE SOOJENEMINE", "kliima", "Kliima", "kasvuhoone", "Kasvuhoone",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "διοξείδιο του άνθρακα", "Διοξείδιο του άνθρακα", "ΔΙΟΞΕΙΔΙΟ ΤΟΥ ΑΝΘΡΑΚΑ", "παγκόσμια θέρμανση", "Παγκόσμια θέρμανση", "ΠΑΓΚΟΣΜΙΑ ΘΕΡΜΑΝΣΗ", "κλίμα", "Κλίμα", "θερμοκήπιο", "Θερμοκήπιο",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "ugljični dioksid", "Ugljični dioksid", "UGLJIČNI DIOKSID", "globalno zatopljenje", "Globalno zatopljenje", "GLOBALNO ZATOPLJENJE", "klima", "Klima", "staklenik", "Staklenik",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "ogļskābā gāze", "Ogļskābā gāze", "OGĻSKĀBĀ GĀZE", "globālā sasilšana", "Globālā sasilšana", "GLOBĀLĀ SASILŠANA", "klimats", "Klimats", "siltumnīca", "Siltumnīca",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "anglies dioksidas", "Anglies dioksidas", "ANGLIES DIOKSIDAS", "pasaulinis atšilimas", "Pasaulinis atšilimas", "PASAULINIS ATŠILIMAS", "klimatas", "Klimatas", "šiltnamis", "Šiltnamis",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "karbonju diossidu", "Karbonju diossidu", "KARBONJU DIOSSIDU", "global warming", "Global warming", "GLOBAL WARMING", "klima", "Klima", "glasshouse", "Glasshouse",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "dioxid de carbon", "Dioxid de carbon", "Dioxid de Carbon", "încălzire globală", "Încălzire globală", "Încălzire Globală", "climă", "Climă", "seră", "Seră",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "oxid uhličitý", "Oxid uhličitý", "Oxid Uhličitý", "globálne otepľovanie", "Globálne otepľovanie", "Globálne Otepľovanie", "klíma", "Klíma", "skleník", "Skleník",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "ogljikov dioksid", "Ogljikov dioksid", "Ogljikov Dioksid", "globalno segrevanje", "Globalno segrevanje", "Globalno Segrevanje", "podnebje", "Podnebje", "rastlinjak", "Rastlinjak",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "oxid uhličitý", "Oxid uhličitý", "Oxid Uhličitý", "globální oteplování", "Globální oteplování", "Globální Oteplování", "klima", "Klima", "skleník", "Skleník",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "széndioxid", "Széndioxid", "Széndioxid", "globális felmelegedés", "Globális felmelegedés", "Globális Felmelegedés", "klíma", "Klíma", "üvegház", "Üvegház",
              "díleitride charbóin", "Díleitride charbóin", "DÍLEITRIDE CHARBÓIN", "Meáchan Domhanda", "meáchan domhanda", "Meáchan domhanda", "Aimsir", "aimsir", "Aimsir", "Fuinseog ghlas", "fuinseog ghlas", "Fuinseog Ghlas")
titles_left_8 <- result_df_8_c %>% filter(str_detect(Text, paste(keywords, collapse = "|")))
save(titles_left_8,file="titles_8.Rda")


remDr$open()


# get_text1 = function(url) {
#   remDr$navigate(url)
#   
#   text_selector <- "table.doc_box_header:nth-child(4) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(3) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(3)"
#   
#   element <- remDr$findElements(using = "css selector", text_selector)
#   text <- element$getElementAttribute("innerText")[[1]]
#   
#   return(text)
# }

get_text1 = function(url) {
  remDr$navigate(url)
  
  text_selector <- "table.doc_box_header:nth-child(4) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(3) > tbody:nth-child(1)"
  element <- remDr$findElement(using = "css selector", text_selector)
  
  if (!is.null(element)) {
    text <- element$getElementAttribute("innerText")[[1]]
    return(text)
  } else {
    return(NA)
  }
}

urls <- titles_left_8$Link
text_8_1 <- lapply(urls, FUN = get_text1)

text_8_unlist <- data.frame(unlist(text_8_1))

##################################### 
# Legislaturperiode 8 ist anders strukturiert, Explanations of Vote sind einfacher und mit Titel runterzuladen, aber an anderer Stelle als normale Beiträge,
# deshalb selbes Vorgehen wie bei normalen Texten

# falls nötig
#rs_driver_object <- rsDriver(
#  browser = "firefox",
#  chromever = NULL,
#  verbose = F,
#  port = free_port(random = T)
#)

#remDr <- rs_driver_object$client
remDr$open()


i = 1

remDr$navigate(df_8_right$speech_links[1])
cookie <- remDr$findElement(using = "css selector", "button.epjs_agree:nth-child(2) > span:nth-child(1)")
cookie$clickElement()

get_load = function(speech_links) {
  remDr$navigate(speech_links)
  if(length(remDr$findElements(using = "css selector", "button.btn")) != 0) {
    return(remDr$getCurrentUrl())
  } else {
    return(NA)
  }
}


ex_links <- df_8_right$ex_links
ex_load <- future_sapply(ex_links, FUN = get_load)


df_8_right$ex_load <- ex_load
df_8_right$ex_noload <- ifelse(is.na(df_8_right$ex_load), df_8_right$ex_links, NA)

save(df_8_right,file="df_right_8.Rda")

ex_noload <- drop_na(df_8_right, ex_noload)
ex_noload <- ex_noload$ex_noload

df_8_right$ex_load <- as.character(df_8_right$ex_load)
df_8_right$ex_load[df_8_right$ex_load == "NA"] <- NA
# 
ex_load <- drop_na(df_8_right, ex_load)
ex_load <- ex_load$ex_load



get_speech1 <- function(url) {
  remDr$navigate(url)
  
  max_index <- 1000
  element_data <- list()
  
  for (index in 1:max_index) {
    title_selector <- paste0("div.erpl_search-results-list-expandable-block:nth-child(", index, ") > div:nth-child(1) > div:nth-child(1) > h3:nth-child(1) > span:nth-child(1)")
   
    title_elem <- remDr$findElements(using = "css selector", title_selector)
    
    if (length(title_elem) == 0) {
      break
    }
    
    element_title <- sapply(title_elem, function(element) {
      element$getElementAttribute("innerText")[[1]]
    })
    
    text_selector <- paste0("div.erpl_search-results-list-expandable-block:nth-child(", index, ") > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > p:nth-child(1)")
    
    text_elem <- remDr$findElements(using = "css selector", text_selector)
    
    element_text <- sapply(text_elem, function(element) {
      element$getElementAttribute("innerText")[[1]]
    })

    element_data[[index]] <- data.frame(Title = element_title, Text = element_text)
  }

  result_df <- bind_rows(element_data)
  return(result_df)
}
##################

dataframe_list_ex <- lapply(ex_noload, get_speech1)

all_titles <- character(0)
all_texts <- character(0)


for (df in dataframe_list_ex) {
  if ("Title" %in% colnames(df)) {
    not_empty_titles <- df$Title[!is.na(df$Title)]
    all_titles <- c(all_titles, not_empty_titles)
  }
  
  if ("Text" %in% colnames(df)) {
    not_empty_texts <- df$Text[!is.na(df$Text)]
    all_texts <- c(all_texts, not_empty_texts)
  }
}

result_df_8_ex <- data.frame(Title = all_titles, Text = all_texts)

########
get_speech2 = function(url) {
  remDr$navigate(url)
  button <- remDr$findElement(using = "css selector", "button.btn")
  try(for(i in 1:999) {
    button$clickElement()
    button1 <- remDr$findElement(using = "css selector", "button.btn")
    button1$clickElement()
    i = i+1
  })
  max_index <- 10000
  element_data <- list()
  for (index in 1:max_index) {
    title_selector <- paste0("div.erpl_search-results-list-expandable-block:nth-child(", index, ") > div:nth-child(1) > div:nth-child(1) > h3:nth-child(1) > span:nth-child(1)")
    
    title_elem <- remDr$findElements(using = "css selector", title_selector)
    
    if (length(title_elem) == 0) {
      break
    }
    
    element_title <- sapply(title_elem, function(element) {
      element$getElementAttribute("innerText")[[1]]
    })
    
    text_selector <- paste0("div.erpl_search-results-list-expandable-block:nth-child(", index, ") > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > p:nth-child(1)")
    
    text_elem <- remDr$findElements(using = "css selector", text_selector)
    
    element_text <- sapply(text_elem, function(element) {
      element$getElementAttribute("innerText")[[1]]
    })

    element_data[[index]] <- data.frame(Title = element_title, Text = element_text)
  }

  result_df <- bind_rows(element_data)
  return(result_df)
}
#############

dataframe_list_ex_2 <- lapply(ex_load, get_speech2)

all_titles <- character(0)
all_texts <- character(0)

for (df in dataframe_list_ex_2) {
  if ("Title" %in% colnames(df)) {
    not_empty_titles <- df$Title[!is.na(df$Title)]
    all_titles <- c(all_titles, not_empty_titles)
  }
  
  if ("Text" %in% colnames(df)) {
    not_empty_texts <- df$Text[!is.na(df$Text)]
    all_texts <- c(all_texts, not_empty_texts)
  }
}

result_df_8_ex_2 <- data.frame(Title = all_titles, Text = all_texts)
result_df_8_ex <- rbind(result_df_8_ex, result_df_8_ex_2)


#save(result_df_8_ex, file = "result_8_ex.rda")


############## Keywords filtern

keywords <- c("CO2", "co2", "CO²", "co²", "Co2", "Co²", "carbon dioxide", "Carbon dioxide", "Carbon Dioxide", "global warming", "Global Warming", "Global warming", "Climate", "climate", "Greenhouse", "greenhouse",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "Kohlendioxid", "kohlendioxid", "globale Erwärmung", "Globale Erwärmung", "globale erwärmung", "Klima", "klima", "Treibhaus", "treibhaus",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "dioxyde de carbone", "Dioxyde de carbone", "dioxyde de Carbone", "réchauffement climatique", "Réchauffement climatique", "réchauffement climatique", "climat", "Climat", "serre", "Serre",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "dióxido de carbono", "Dióxido de carbono", "dióxido de Carbono", "calentamiento global", "Calentamiento global", "calentamiento global", "clima", "Clima", "invernadero", "Invernadero",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "anidride carbonica", "Anidride carbonica", "anidride Carbonica", "riscaldamento globale", "Riscaldamento globale", "riscaldamento Globale", "clima", "Clima", "serra", "Serra",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "dwutlenek węgla", "Dwutlenek węgla", "dwutlenek Węgla", "globalne ocieplenie", "Globalne ocieplenie", "globalne Ocieplenie", "klimat", "Klimat", "szklarnia", "Szklarnia",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "dióxido de carbono", "Dióxido de carbono", "dióxido de Carbono", "aquecimento global", "Aquecimento global", "aquecimento Global", "clima", "Clima", "estufa", "Estufa",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "koolstofdioxide", "Koolstofdioxide", "koolstofdioxide", "opwarming van de aarde", "Opwarming van de aarde", "opwarming van de aarde", "klimaat", "Klimaat", "kas", "Kas",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "koldioxid", "Koldioxid", "koldioxid", "global uppvärmning", "Global uppvärmning", "global uppvärmning", "klimat", "Klimat", "växthus", "Växthus",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "въглероден диоксид", "Въглероден диоксид", "ВЪГЛЕРОДЕН ДИОКСИД", "глобално затопляне", "Глобално затопляне", "ГЛОБАЛНО ЗАТОПЛЯНЕ", "климат", "Климат", "топлица", "Топлица",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "kuldioxid", "Kuldioxid", "KULDIOXID", "global opvarmning", "Global opvarmning", "GLOBAL OPVARMNING", "klima", "Klima", "drivhus", "Drivhus",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "süsihappegaas", "Süsihappegaas", "SÜSIHAPPEGAAS", "globaalne soojenemine", "Globaalne soojenemine", "GLOBAALNE SOOJENEMINE", "kliima", "Kliima", "kasvuhoone", "Kasvuhoone",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "süsihappegaas", "Süsihappegaas", "SÜSIHAPPEGAAS", "globaalne soojenemine", "Globaalne soojenemine", "GLOBAALNE SOOJENEMINE", "kliima", "Kliima", "kasvuhoone", "Kasvuhoone",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "διοξείδιο του άνθρακα", "Διοξείδιο του άνθρακα", "ΔΙΟΞΕΙΔΙΟ ΤΟΥ ΑΝΘΡΑΚΑ", "παγκόσμια θέρμανση", "Παγκόσμια θέρμανση", "ΠΑΓΚΟΣΜΙΑ ΘΕΡΜΑΝΣΗ", "κλίμα", "Κλίμα", "θερμοκήπιο", "Θερμοκήπιο",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "ugljični dioksid", "Ugljični dioksid", "UGLJIČNI DIOKSID", "globalno zatopljenje", "Globalno zatopljenje", "GLOBALNO ZATOPLJENJE", "klima", "Klima", "staklenik", "Staklenik",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "ogļskābā gāze", "Ogļskābā gāze", "OGĻSKĀBĀ GĀZE", "globālā sasilšana", "Globālā sasilšana", "GLOBĀLĀ SASILŠANA", "klimats", "Klimats", "siltumnīca", "Siltumnīca",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "anglies dioksidas", "Anglies dioksidas", "ANGLIES DIOKSIDAS", "pasaulinis atšilimas", "Pasaulinis atšilimas", "PASAULINIS ATŠILIMAS", "klimatas", "Klimatas", "šiltnamis", "Šiltnamis",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "karbonju diossidu", "Karbonju diossidu", "KARBONJU DIOSSIDU", "global warming", "Global warming", "GLOBAL WARMING", "klima", "Klima", "glasshouse", "Glasshouse",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "dioxid de carbon", "Dioxid de carbon", "Dioxid de Carbon", "încălzire globală", "Încălzire globală", "Încălzire Globală", "climă", "Climă", "seră", "Seră",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "oxid uhličitý", "Oxid uhličitý", "Oxid Uhličitý", "globálne otepľovanie", "Globálne otepľovanie", "Globálne Otepľovanie", "klíma", "Klíma", "skleník", "Skleník",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "ogljikov dioksid", "Ogljikov dioksid", "Ogljikov Dioksid", "globalno segrevanje", "Globalno segrevanje", "Globalno Segrevanje", "podnebje", "Podnebje", "rastlinjak", "Rastlinjak",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "oxid uhličitý", "Oxid uhličitý", "Oxid Uhličitý", "globální oteplování", "Globální oteplování", "Globální Oteplování", "klima", "Klima", "skleník", "Skleník",
              "CO2", "co2", "CO²", "co²", "Co2", "Co²", "széndioxid", "Széndioxid", "Széndioxid", "globális felmelegedés", "Globális felmelegedés", "Globális Felmelegedés", "klíma", "Klíma", "üvegház", "Üvegház",
              "díleitride charbóin", "Díleitride charbóin", "DÍLEITRIDE CHARBÓIN", "Meáchan Domhanda", "meáchan domhanda", "Meáchan domhanda", "Aimsir", "aimsir", "Aimsir", "Fuinseog ghlas", "fuinseog ghlas", "Fuinseog Ghlas")
titles_left_8_ex <- result_df_8_ex %>% filter(str_detect(Title, paste(keywords, collapse = "|")))

titles_left_8_ex$Link <- NA
titles_left_8 <- titles_left_8 %>% rename(Title = Text)
titles_left_8$Text <- text_8_unlist$unlist.text_8_1.

# Datansätze zusammenführen
complete_8 <- rbind(titles_left_8, titles_left_8_ex)

#save(complete_8, file = "complete_8.Rda")


