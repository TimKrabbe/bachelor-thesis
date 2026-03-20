###################################################
# Bachelorarbeit Tim Krabbe, 2872187              #
# ChatGPT in der qualitativen Sozialforschung     #
###################################################

# install.packages("RSelenium")
# install.packages("future.apply")
# install.packages("quanteda")

library(RSelenium)
library(tidyverse)
library(rvest)
library(stringr)
library(future.apply)
library(netstat)
library(tidyr)
library(quanteda)

#####################################################################
##### Sechste Legislaturperiode #####################################
#####################################################################

##### Datensätze, diverse ZWischenstände etc.. Nicht im Anhang enthalten, außer das Endergebnis. 
# load("~/Studium/SoWi-Bachelor/Semester 6/Kolle/df.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/df_right_6.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/cl.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/exvote_links_3.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/ex_titles_6.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/result_6.Rda")

# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/exvote_links_wo.Rda")
# load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/exvote_vorläufig.Rda")
#load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/all_6.Rda")
#load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/exvote_links_4.Rda")
#load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/exvote_vorläufig2.Rda")
#load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/df_7_right.Rda")
#load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/df_7.Rda")
#load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/cl_7.Rda")

#load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/df_8_right.Rda")
#load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/df_8.Rda")
#load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/cl_8.Rda")
#load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/titles2_df_8.csv")

#load("~/Studium/Sowi-Bachelor/Semester 6/Kolle/complete_c.Rda")


##############################################################################
##### html-page einlesen
# link = "https://www.europarl.europa.eu/meps/en/directory/6"
# page = read_html(link)

##### Namen der Abgeordneten auslesen und in dataframe speichern
# name = page %>% html_nodes("#docMembersList .t-item") %>% html_text()
# df <- data.frame(name)
#
# page %>% html_nodes(".t-y-block")
#
# ##### Namen vorbereiten für die Anspielung an die Link erpl_member-list-item-content mb-3 t-y-block
# df$name <- gsub(" ", "_", df$name)
#
# ##### Links zu den Abgeordneten auslesen
# links1 = page %>% html_nodes(".t-y-block")
# #links1 <- data.frame(links1)

# #### 7 Links zu viel ausgelesen
# links1[c(1,2,3,4,5,6,7)]
# mep_links = page %>% html_nodes(".t-y-block") %>%
#   html_attr("href")

# #### Entsprechende Links rausschmeißen
# mep_links <- mep_links[-c(1,2,3,4,5,6,7)]
#
# ###### Links für die 6. Legislaturperiode ranspielen
# #mep_links <- mep_links %>% paste(., "/", df$name, "/history/6", sep = "")
# df$meplinks_6 <- mep_links
#
# test = read_html("https://www.europarl.europa.eu/meps/en/28469/ADAMOS_ADAMOU/history/6")
#
# ##########################################################
# Mep_Links auslesen; manche Links waren nicht korrekt, 
# Links in Abschnitten auslesen, falls Verbindung instabil
# ##########################################################
#

mep_test <- mep_links[seq(1,200,1)]
mep_test2 <- mep_links[seq(201,400,1)]
mep_test3 <- mep_links[seq(401,600,1)]
mep_test4 <- mep_links[seq(601,800,1)]
mep_test5 <- mep_links[seq(801,944,1)]

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
#correct_link = sapply(mep_links, FUN = get_correct_link)

correct_link = future_sapply(mep_test, FUN = get_correct_link)
correct_link2 = future_sapply(mep_test2, FUN = get_correct_link2)
correct_link3 = future_sapply(mep_test3, FUN = get_correct_link3)
correct_link4 = future_sapply(mep_test4, FUN = get_correct_link4)
correct_link5 = future_sapply(mep_test5, FUN = get_correct_link5)
# ##### Das ganze in Stückchen probieren, Internetverbindung bricht sonst ggf. ab
# 
correct_links_complete <- c(correct_link,correct_link2,correct_link3,correct_link4, correct_link5)
df$meplinks_complete <- correct_links_complete

# c_link <- for(i in 1:200) {
#  get_correct_link(mep_test)
# }

# save(df,file="df.Rda")
#
# ####################################################################
# Parteien der MEPs auslesen, um nach rechten Parteien zu filtern
#
# ####################################################################
#
mep_links_speech <- mep_links %>% str_sub(0,-10)
mep_links_speech <- mep_links_speech %>% paste(., "all-activities/plenary-speeches/6", sep = "")
df$links_all_speeches <- mep_links_speech

# .mt-3 .btn-default

mep_links_1 <- mep_links[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
mep_links_1[13] <- "https://www.europarl.europa.eu/meps/en/28276/Laima+Liucija_ANDRIKIENĖ/history/6"
# 
toomuch_ <- mep_links[str_count(mep_links, "_") > 1]

df$meplinks_complete <- gsub("home", "history/6#detailedcardmep", df$meplinks_complete)
df$meplinks_complete <- gsub("history/7", "history/6", df$meplinks_complete)
df$meplinks_complete <- gsub("history/8", "history/6", df$meplinks_complete)
df$meplinks_complete <- gsub("history/9", "history/6", df$meplinks_complete)

meplinks_complete <- df$meplinks_complete

mep_com1 <- meplinks_complete[seq(1,200,1)]
mep_com2 <- meplinks_complete[seq(201,400,1)]
mep_com3 <- meplinks_complete[seq(401,600,1)]
mep_com4 <- meplinks_complete[seq(601,800,1)]
mep_com5 <- meplinks_complete[seq(801,944,1)]

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
save(df,file="df.Rda")


#
# #############################################################
# Parteien nach rechten Parteien filtern
#
# #############################################################
#
#

party_strings <- c("Front national", "United Kingdom Independence Party", "Freiheitliche Partei Österreichs", "Lega Nord per l'indipendenza della Padania", 
                   "Les Patriotes", "Jobbik Magyarországért Mozgalom", "Dansk Folkeparti", "Slovenská národná strana", "Hrvatska stranka prava", 
                   "Naprzód Polsko", "Laikos Orthodoxos Synagermos","Perussuomalaiset", "Liga Polskich Rodzin", "Vlaams Blok", "Vlaams Belang", 
                   "Artikel 50", "Brexit Party", "Partidul România Mare", "Laikos Syndesmos", 
                   "British National Party", " Movimento Sociale Fiamma tricolore", "Elliniki Lisi", 
                   "Nationaldemokratische Partei Deutschlands", "Hrvatska konzervativna stranka", "Popular Orthodox Rally", "UK Independence Party", 
                   "Lega Nord", "Partij voor de Vrijheid", "Golden Dawn")

df$party_name <- substr(df$party, 26, nchar(df$party))
df_right <- df %>% filter(str_detect(party, paste(party_strings, collapse = "|")))

save(df,file="df.Rda")
save(df_right,file="df_right_6.Rda")
# 
# ##############################################################
# Links zu den Beiträgen auslesen
#
# ##############################################################


# node:
# .mt-3 .btn-default

meplinks_complete <- df_right$meplinks_complete
get_speech_link = function(meplinks_complete) {
     mep_page = read_html(meplinks_complete)
     speech_link = mep_page %>% html_nodes(".mt-3 .btn-default") %>% html_attr("href") %>%
       paste(collapse = ", ")
     return(speech_link)
}

speech_links = future_sapply(meplinks_complete, FUN = get_speech_link)
df_right$speech_links <- speech_links

save(df_right,file="df_right_6.Rda")
 

# RSelenium remote server erstellen um auf die EU-Parlamentsseite zuzugreifen und die Texte runterzuladen
rs_driver_object <- rsDriver(
  browser = "firefox",
  chromever = NULL,
  verbose = F,
  port = free_port(random = T)
)

remDr <- rs_driver_object$client #das hier geöffnete Fenster muss geschlossen werden
remDr$open()

# cookies akzeptieren
remDr$navigate(df_right$speech_links[1])
cookie <- remDr$findElement(using = "css selector", "button.epjs_agree:nth-child(2) > span:nth-child(1)")
cookie$clickElement()

# alle MEP-Seiten finden, auf denen ein "load more" Button geklickt werden muss
get_load = function(speech_links) {
  remDr$navigate(speech_links)
  if(length(remDr$findElements(using = "css selector", "button.btn")) != 0) {
    return(remDr$getCurrentUrl())
  } else {
    return(NA)
  }
}

# aufteilen in Seiten mit und ohne Button
speech_links <- df_right$speech_links
load <- future_sapply(speech_links, FUN = get_load)
load_2 <- names(load)

df_right$speech_load <- load
df_right$speech_noload <- ifelse(is.na(df_right$speech_load), df_right$speech_links, NA)

save(df_right,file="df_right_6.Rda")

noload <- drop_na(df_right, speech_noload)
noload <- noload$speech_noload

df_right$speech_load <- as.character(df_right$speech_load)
df_right$speech_load[df_right$speech_load == "NA"] <- NA

load <- drop_na(df_right, speech_load)
load <- load$speech_load

# Redebeiträge runterladen
get_speech1 <- function(url) {
  remDr$navigate(url)
  
  max_index <- 1000  # maximales Indexlimit, so oft läuft die Schleife (theoretisch)
  element_data <- list()
  
  for (index in 1:max_index) {
    selector <- paste0("div.erpl_search-results-list-expandable-block:nth-child(", index, ") > div:nth-child(1) > div:nth-child(1) > h3:nth-child(1) > a:nth-child(1) > span:nth-child(1)")
    
    elem <- remDr$findElements(using = "css selector", selector)
    
    if (length(elem) == 0) {
      break
    }
    # Text runterladen
    element_text <- sapply(elem, function(element) {
      element$getElementAttribute("innerText")[[1]]
    })
    
    link_selector <- paste0("div.erpl_search-results-list-expandable-block:nth-child(", index, ") > div:nth-child(1) > div:nth-child(1) > h3:nth-child(1) > a:nth-child(1)")
    
    link_elem <- remDr$findElements(using = "css selector", link_selector)
    
    element_link <- sapply(link_elem, function(element) {
      element$getElementAttribute("href")
    })
    
    # Texte in Listen packen
    element_data[[index]] <- data.frame(Text = element_text, Link = element_link)
  }
  
  # Listen in Dataframe
  result_df <- bind_rows(element_data)
  return(result_df)
}

# Dataframe mit den gesamten Redbeiträgen der MEPs mit "load more" button
dataframe_list <- lapply(noload, get_speech1)

# Dataframe clean-up
all_texts <- character(0)
all_links <- character(0)

for (df in dataframe_list) {
  link_columns <- grep("^X\\.https", colnames(df), value = TRUE)
  not_empty_texts <- df$Text[!is.na(df$Text)]
  
  not_empty_links <- unlist(df[link_columns], use.names = FALSE)
  not_empty_links <- not_empty_links[!is.na(not_empty_links)]
  
  all_texts <- c(all_texts, not_empty_texts)
  all_links <- c(all_links, not_empty_links)
}

# finaler Dataframe (MEP-Seiten mit load more button)
result_df_6 <- data.frame(Text = all_texts, Link = all_links)


################

# remote server öffnen
remDr$open()

i = 1

# cookies akzeptieren falls nötig
remDr$navigate(df_right$speech_links[1])
cookie <- remDr$findElement(using = "css selector", "button.epjs_agree:nth-child(2) > span:nth-child(1)")
cookie$clickElement()

# Funktion um alle MEP-Seiten mit "load more" Button
get_speech2 = function(load_7) {
  remDr$navigate(load_7)
  button <- remDr$findElement(using = "css selector", "button.btn")
  try(for(i in 1:999) { # Schleife läuft 999 mal oder bis kein Button mehr da ist
    button$clickElement()
    button1 <- remDr$findElement(using = "css selector", "button.btn")
    button1$clickElement()
    i = i+1
  })
  max_index <- 10000  # Maximales Indexlimit
  element_data <- list() # Texte werden runtergeladen
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
    
    # Daten in Listen speichern
    element_data[[index]] <- data.frame(Text = element_text, Link = element_link)
  }
  
  # Listen zu Dataframe
  result_df <- bind_rows(element_data)
  
  # Gebe den finalen Dataframe aus
  return(result_df)
}


dataframe_list_2 <- future_lapply(load, get_speech2)

## data clean-up

all_texts <- character(0)
all_links <- character(0)

for (df in dataframe_list_2) {
  link_columns <- grep("^X\\.https", colnames(df), value = TRUE)
  not_empty_texts <- df$Text[!is.na(df$Text)]

  not_empty_links <- unlist(df[link_columns], use.names = FALSE)
  not_empty_links <- not_empty_links[!is.na(not_empty_links)]
  
  all_texts <- c(all_texts, not_empty_texts)
  all_links <- c(all_links, not_empty_links)
}

# finaler Datensatz (mit load more Button)
result_df_6_2 <- data.frame(Text = all_texts, Link = all_links)

# Datensätze zusammenführen
result_df_6_c <- rbind(result_df_6, result_df_6_2)


save(result_df_6_c, file = "result_6.rda")

# Schlüsselwörter zur Filterung der Texte abspeichern
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

# Titel filtern
titles_left <- result_df_6_c %>% filter(str_detect(Text, paste(keywords, collapse = "|")))

####### Texte mit dem Titel Explanations of Vote, gehören zu Debatten, deren Titel zum Filtern benötigt wird
# Explanations of Vote filtern
exvote <- result_df_6_c %>% filter(str_detect(Text, paste("Explanations of vote", collapse = "|")))
exvote_vec <- exvote$Link

# Titel der zugehörigen Debatten scrapen
get_title_ex = function(url_list) {
  result_list <- list()  # Liste zur Speicherung von Dataframes

  for (url in url_list) {
    remDr$navigate(url)

    title_selector <- ".erpl_title-h1"
    link_selector <- ".doc_subtitle_level2 > a:nth-child(1)"

    link_elements <- remDr$findElements(using = "css selector", link_selector)

    if (length(link_elements) != 0) {
      links <- sapply(link_elements, function(element) {
        element$getElementAttribute("href")
      })

      titles <- sapply(link_elements, function(element) {
        link <- element$getElementAttribute("href")
        remDr$navigate(link)
        title_element <- tryCatch(
          {
            remDr$findElement(using = "css selector", title_selector)
          },
          error = function(e) {
            return(NULL)
          }
        )
        if (!is.null(title_element)) {
          title_element$getElementAttribute("innerText")[[1]]
        } else {
          NA
        }
      })

      result_list[[length(result_list) + 1]] <- data.frame(Link = links, Title = titles)
    } else {
      result_list[[length(result_list) + 1]] <- data.frame(Link = NA, Title = NA)
    }
  }

  result_df <- do.call(rbind, result_list)

  return(result_df)
}

######## links zu den Titeln nicht überall vorhanden, zuerst filtern und dann extra behandeln. Titel sind über weiteren Link verfügbar

exvote_l_t <- future_lapply(exvote_vec, FUN = get_title_ex)
exvote_l_t <- data.frame(unlist(exvote_l_t))

exvote_l <- drop_na(exvote_l_t)
exvote_l <- exvote_l %>% rename(Link = unlist.exvote_l_t.)

# link 1
ex_title_links <- exvote_l$Link

get_ex_link = function(url) {
  link_selector <- ".doc_subtitle_level2 > a:nth-child(1)"
  
  remDr$navigate(url)
  
  if(length(remDr$findElements(using = "css selector", link_selector)) != 0) {
    return(remDr$getCurrentUrl())
  } else {
    return(NA)
  }
}

links <- future_sapply(exvote_vec, FUN = get_ex_link)
links_df <- data.frame(unlist(links))
links_df <- drop_na(links_df)

urls <- links_df$unlist.links.

# link 2
get_ex_link2 = function(url) {
  link_selector = ".doc_subtitle_level2 > a:nth-child(1)"
  
  remDr$navigate(url)
  link <- remDr$findElement(using = "css selector", link_selector)
  link$getElementAttribute("href")
}

ex_title_links <- future_lapply(urls, FUN = get_ex_link2)
urls_title <- unlist(ex_title_links)

# Titel auslesen
get_ex_title = function(url) {
  title_selector <- ".erpl_title-h1"
  
  remDr$navigate(url)
  
  tryCatch({
    title_elem <- remDr$findElement(using = "css selector", title_selector)
    title <- title_elem$getElementAttribute("innerText")[[1]]
    return(title)
  }, error = function(e) {
    return(NA)
  })
}

ex_titles <- future_lapply(urls_title, FUN = get_ex_title)

# neuer Datensatz
ex_titles_6 <- data.frame(unlist(ex_titles))
ex_titles_6$Link <- urls_title
ex_titles_6$Link2 <- urls
ex_titles_6 <- ex_titles_6 %>% rename(Title = unlist.ex_titles.)

# KEyowrds filtern für Explanations of Vote
titles_left_ex <- ex_titles_6 %>% filter(str_detect(Title, paste(keywords, collapse = "|")))


titles_left <- titles_left %>% rename(Link2 = Link)
titles_left <- titles_left %>% rename(Title = Text)

titles_left$Link <- NA

titles_c <- rbind(titles_left, titles_left_ex)


urls_c <- titles_c$Link2

get_text_c <- function(url) {
  text_selector = "table.doc_box_header:nth-child(4) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(3) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(3)"
  
  remDr$navigate(url)
  text_elem <- remDr$findElement(using = "css selector", text_selector)
  text <- text_elem$getElementAttribute("innerText")[[1]]
  
  return(text)
}

text_c <- future_lapply(urls_c, FUN = get_text_c)
text_c <- data.frame(unlist(text_c))

complete_6 <- titles_c
complete_6$Text <- text_c$unlist.text_c.

save(complete_6, file = "complete_6.Rda")

########################################################################
### alle finalen Datensätze der drei Legislaturperioden zusammenfügen
complete_6 <- complete_6 %>% rename(Link = Link2, Link2 = Link)
complete_8$Link2 <- NA

complete_6$Leg <- "Leg 6"
complete_7$Leg <- "Leg 7"
complete_8$Leg <- "Leg 8"
complete_c_final <- rbind(complete_6, complete_7, complete_8)



save(complete_c_final, file = "complete_c_final.Rda")



#################### Ausgabe aller Texte als Vektor, anschließend aus der Konsole herauskopiert. 
#################### Schnelle, einfache Möglichkeit und auf diese Weise bleiben die Nummern erhalten. Abgespeichert in alletexte.txt
texts <- complete_c_final$Text
texts[seq(1,947)]




###############################################

