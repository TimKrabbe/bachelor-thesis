library(DescTools)
library(tidyverse)
library(readxl)
library(RColorBrewer)

###################################################################################################
######## Berechnung von Krippendorffs Alpha und Erstellung der zugehörigen Grafiken  ##############
######## Bachelorarbeit, Tim Krabbe, 2872187                                         ##############
###################################################################################################



###################################################################
##Kodierung von ChatGPT bei Temperatur 0.5 
# Datensatz einlesen
data_temp_05 <- read_excel("data_temp_05.xlsx")
View(data_temp_05)

#Datensatz umbennen und überflüssige Spalten und Reihen löschen
d_05 <- data_temp_05 
d2_05 <- d_05[-c(21),-1]

# Nur die Spalten, in denen eine 1 vorkommt, um danach manuell zu filtern
d3 <- d2_05 %>% select_if(~ any(. == 1))

# Datensätze pro Code erstellen
d_WANU <- d2_05[, c(1, 18, 35, 52, 69)]
d_CPFB <- d2_05[, c(2, 19, 36, 53, 70)]
d_EJHN <- d2_05[, c(3, 20, 37, 54, 71)]
d_CPMM <- d2_05[, c(4, 21, 38, 55, 72)]
d_NPIW <- d2_05[, c(6, 23, 40, 57, 74)]
d_GPNE <- d2_05[, c(8, 25, 42, 59, 76)]
d_PMRB <- d2_05[, c(9, 26, 43, 60, 77)]
d_BICA <- d2_05[, c(10, 27, 44, 61, 78)]
d_CMOB <- d2_05[, c(11, 28, 45, 62, 79)]
d_UXWM <- d2_05[, c(12, 29, 46, 63, 80)]
d_PDNC <- d2_05[, c(14, 31, 48, 65, 82)]
d_AHNP <- d2_05[, c(15, 32, 49, 66, 83)]
d_IAHR <- d2_05[, c(16, 33, 50, 67, 84)]
d_DMAH <- d2_05[, c(17, 34, 51, 68, 85)]


#Datensätz pro Code in Matrizen umwandeln, Vorbereitung auf Errechnung von Krippendorfs Alpha
dm_WANU <- t(as.matrix(d_WANU))
dm_CPFB <- t(as.matrix(d_CPFB))
dm_EJHN <- t(as.matrix(d_EJHN))
dm_CPMM <- t(as.matrix(d_CPMM))
dm_NPIW <- t(as.matrix(d_NPIW))
dm_GPNE <- t(as.matrix(d_GPNE))
dm_PMRB <- t(as.matrix(d_PMRB))
dm_BICA <- t(as.matrix(d_BICA))
dm_CMOB <- t(as.matrix(d_CMOB))
dm_UXWM <- t(as.matrix(d_UXWM))
dm_PDNC <- t(as.matrix(d_PDNC))
dm_AHNP <- t(as.matrix(d_AHNP))
dm_IAHR <- t(as.matrix(d_IAHR))
dm_DMAH <- t(as.matrix(d_DMAH))

# Krippendorfs Alpha pro Code errechnen
k01 <- KrippAlpha(dm_WANU, method = "nominal")
k02 <- KrippAlpha(dm_CPFB, method = "nominal")
k03 <- KrippAlpha(dm_EJHN, method = "nominal")
k04 <- KrippAlpha(dm_CPMM, method = "nominal")
k05 <- KrippAlpha(dm_NPIW, method = "nominal")
k06 <- KrippAlpha(dm_GPNE, method = "nominal")
k07 <- KrippAlpha(dm_PMRB, method = "nominal")
k08 <- KrippAlpha(dm_BICA, method = "nominal")
k09 <- KrippAlpha(dm_CMOB, method = "nominal")
k10 <- KrippAlpha(dm_UXWM, method = "nominal")
k11 <- KrippAlpha(dm_PDNC, method = "nominal")
k12 <- KrippAlpha(dm_AHNP, method = "nominal")
k13 <- KrippAlpha(dm_IAHR, method = "nominal")
k14 <- KrippAlpha(dm_DMAH, method = "nominal")

#neuen Datensatz erstellen, in dem nur die Werte für Krippendorfs Alpha pro Code abgebildet sind
r <- data.frame(k01$value, k02$value, k03$value, k04$value, k05$value, k06$value, k07$value, k08$value, k09$value, k10$value, k11$value, k12$value, k13$value, k14$value)

#Umbenennen der Spalten
r <- r %>% rename(WANU = k01.value, CPFB = k02.value, EJHN = k03.value, CPMM = k04.value, NPIW = k05.value, 
                  GPNE = k06.value, PMRB = k07.value, BICA = k08.value, CMOB = k09.value, UXWM = k10.value, 
                  PDNC = k11.value, AHNP = k12.value, IAHR = k13.value, DMAH = k14.value)

#Umbenennen der Reihe
rownames(r) <- "Temperatur: 0.5"

#Tausche von Spalten und Reihen
r <- tidyr::gather(r, key = "Code", value = "Temperatur: 0.5")

r_mean <- mean(r$`Temperatur: 0.5`)

##########################################################################
######## Kodierung von ChatGPT bei Temperatur 0.0

# Datensatz einlesen
data_temp_00 <- read_excel("data_temp_00.xlsx")
View(data_temp_00)

#Datensatz umbennen und überflüssige Spalten und Reihen löschen
d_00 <- data_temp_00
d2_00 <- d_00[,-1]

d3_00 <- d2_00 %>% select_if(~ any(. == 1))


# Datensätze pro Code erstellen, # markieren Codes die in der Kodierung nicht genutzt wurden; als Merkhilfe für weitere Befehle, um ungenutzte Codes unten auszuschließen 
d_WANU <- d2_00[, c(1, 19, 37, 55, 73)] #
d_CPFB <- d2_00[, c(2, 20, 38, 56, 74)] #
d_EJHN <- d2_00[, c(3, 21, 39, 57, 75)] #
d_CPMM <- d2_00[, c(4, 22, 40, 58, 76)] #
d_NPIW <- d2_00[, c(6, 24, 42, 60, 78)] 
d_GPNE <- d2_00[, c(8, 26, 44, 62, 80)] #
d_PMRB <- d2_00[, c(9, 27, 45, 63, 81)] #
d_BICA <- d2_00[, c(10, 28, 46, 64, 82)] # 
d_CMOB <- d2_00[, c(11, 29, 47, 65, 83)]
d_UXWM <- d2_00[, c(12, 30, 48, 66, 84)]
d_PDNC <- d2_00[, c(14, 32, 50, 68, 86)]
d_AHNP <- d2_00[, c(15, 33, 51, 69, 87)]
d_IAHR <- d2_00[, c(16, 34, 52, 70, 88)]
d_DMAH <- d2_00[, c(17, 35, 53, 71, 89)]
d_SGCA <- d2_00[, c(18, 36, 54, 72, 90)]

#Datensätz pro Code in Matrizen umwandeln, Vorbereitung auf Errechnung von Krippendorfs Alpha
dm_WANU <- t(as.matrix(d_WANU))
dm_CPFB <- t(as.matrix(d_CPFB))
dm_EJHN <- t(as.matrix(d_EJHN))
dm_CPMM <- t(as.matrix(d_CPMM))
dm_NPIW <- t(as.matrix(d_NPIW))
dm_GPNE <- t(as.matrix(d_GPNE))
dm_PMRB <- t(as.matrix(d_PMRB))
dm_BICA <- t(as.matrix(d_BICA))
dm_CMOB <- t(as.matrix(d_CMOB))
dm_UXWM <- t(as.matrix(d_UXWM))
dm_PDNC <- t(as.matrix(d_PDNC))
dm_AHNP <- t(as.matrix(d_AHNP))
dm_IAHR <- t(as.matrix(d_IAHR))
dm_DMAH <- t(as.matrix(d_DMAH))
dm_SGCA <- t(as.matrix(d_SGCA))

# Krippendorfs Alpha pro Code errechnen
k01 <- KrippAlpha(dm_WANU, method = "nominal")
k02 <- KrippAlpha(dm_CPFB, method = "nominal")
k03 <- KrippAlpha(dm_EJHN, method = "nominal")
k04 <- KrippAlpha(dm_CPMM, method = "nominal")
k05 <- KrippAlpha(dm_NPIW, method = "nominal")
k06 <- KrippAlpha(dm_GPNE, method = "nominal")
k07 <- KrippAlpha(dm_PMRB, method = "nominal")
k08 <- KrippAlpha(dm_BICA, method = "nominal")
k09 <- KrippAlpha(dm_CMOB, method = "nominal")
k10 <- KrippAlpha(dm_UXWM, method = "nominal")
k11 <- KrippAlpha(dm_PDNC, method = "nominal")
k12 <- KrippAlpha(dm_AHNP, method = "nominal")
k13 <- KrippAlpha(dm_IAHR, method = "nominal")
k14 <- KrippAlpha(dm_DMAH, method = "nominal")
k15 <- KrippAlpha(dm_SGCA, method = "nominal")

#neuen Datensatz erstellen, in dem nur die Werte für Krippendorfs Alpha pro Code abgebildet sind
r2 <- data.frame(k01$value, k02$value, k03$value, k04$value, k05$value, k06$value, k07$value, k08$value, k09$value, k10$value, k11$value, k12$value, k13$value, k14$value, k15$value)

#Umbenennen der Spalten
r2 <- r2 %>% rename(WANU = k01.value, CPFB = k02.value, EJHN = k03.value, CPMM = k04.value, NPIW = k05.value, 
                  GPNE = k06.value, PMRB = k07.value, BICA = k08.value, CMOB = k09.value, UXWM = k10.value, 
                  PDNC = k11.value, AHNP = k12.value, IAHR = k13.value, DMAH = k14.value, SGCA = k15.value)

#Umbenennen der Reihe
rownames(r2) <- "Temperatur: 0.0"

#Tausche von Spalten und Reihen
r2 <- tidyr::gather(r2, key = "Code", value = "Temperatur: 0.0")

# arithmetisches Mittel Krippendorfs Alpha bei Temperatur 0.0
r2_mean <- mean(r2$`Temperatur: 0.0`)

############################################################################################## 
#Kodierung von ChatGPT bei Temperatur 1.0

# Datensatz einlesen
data_temp_01 <- read_excel("data_temp_01.xlsx")
#View(data_temp_0100)

#Datensatz umbennen und überflüssige Spalten und Reihen löschen
d_01 <- data_temp_01
d2_01 <- d_01[,-1]

d3_01 <- d2_01 %>% select_if(~ any(. == 1))
# Datensätze pro Code erstellen
d_WANU <- d2_01[, c(1, 24, 47, 70, 93)]
d_CPFB <- d2_01[, c(2, 25, 48, 71, 94)]
d_EJHN <- d2_01[, c(3, 26, 49, 72, 95)]
d_CPMM <- d2_01[, c(4, 27, 50, 73, 96)]
d_PBNV <- d2_01[, c(5, 28, 51, 74, 95)]
d_NPIW <- d2_01[, c(6, 29, 52, 75, 98)]
d_PIII <- d2_01[, c(7, 30, 53, 76, 99)]
d_GPNE <- d2_01[, c(8, 31, 54, 77, 100)]
d_PMRB <- d2_01[, c(9, 32, 55, 78, 101)]
d_BICA <- d2_01[, c(10, 33, 56, 79, 102)]
d_CMOB <- d2_01[, c(11, 34, 57, 80, 103)]
d_UXWM <- d2_01[, c(12, 35, 58, 81, 104)]
d_PDNC <- d2_01[, c(13, 36, 59, 82, 105)] #
d_AHNP <- d2_01[, c(14, 37, 60, 83, 106)]
d_NCCM <- d2_01[, c(15, 38, 61, 84, 107)]
d_IAHR <- d2_01[, c(16, 39, 62, 85, 108)] #
d_DMAH <- d2_01[, c(17, 40, 63, 86, 109)] #
d_SGCA <- d2_01[, c(18, 41, 64, 87, 110)]
d_LCIE <- d2_01[, c(19, 42, 65, 88, 111)]
d_ETNC <- d2_01[, c(20, 43, 66, 89, 112)]
d_SCPI <- d2_01[, c(21, 44, 67, 90, 113)]
d_NGIS <- d2_01[, c(22, 45, 68, 91, 114)]
d_CMNE <- d2_01[, c(23, 46, 69, 92, 115)]

#Datensätz pro Code in Matrizen umwandeln, Vorbereitung auf Errechnung von Krippendorfs Alpha
dm_WANU <- t(as.matrix(d_WANU))
dm_CPFB <- t(as.matrix(d_CPFB))
dm_EJHN <- t(as.matrix(d_EJHN))
dm_CPMM <- t(as.matrix(d_CPMM))
dm_PBNV <- t(as.matrix(d_PBNV))
dm_NPIW <- t(as.matrix(d_NPIW))
dm_PIII <- t(as.matrix(d_PIII)) 
dm_GPNE <- t(as.matrix(d_GPNE))
dm_PMRB <- t(as.matrix(d_PMRB))
dm_BICA <- t(as.matrix(d_BICA))
dm_CMOB <- t(as.matrix(d_CMOB))
dm_UXWM <- t(as.matrix(d_UXWM))
dm_PDNC <- t(as.matrix(d_PDNC)) #
dm_AHNP <- t(as.matrix(d_AHNP))
dm_NCCM <- t(as.matrix(d_NCCM))
dm_IAHR <- t(as.matrix(d_IAHR)) #
dm_DMAH <- t(as.matrix(d_DMAH)) #
dm_SGCA <- t(as.matrix(d_SGCA))
dm_LCIE <- t(as.matrix(d_LCIE))
dm_ETNC <- t(as.matrix(d_ETNC))
dm_SCPI <- t(as.matrix(d_SCPI))
dm_NGIS <- t(as.matrix(d_NGIS))
dm_CMNE <- t(as.matrix(d_CMNE))

# Krippendorfs Alpha pro Code errechnen
k01 <- KrippAlpha(dm_WANU, method = "nominal")
k02 <- KrippAlpha(dm_CPFB, method = "nominal")
k03 <- KrippAlpha(dm_EJHN, method = "nominal")
k04 <- KrippAlpha(dm_CPMM, method = "nominal")
k05 <- KrippAlpha(dm_NPIW, method = "nominal")
k06 <- KrippAlpha(dm_PIII, method = "nominal")
k07 <- KrippAlpha(dm_GPNE, method = "nominal")
k08 <- KrippAlpha(dm_PMRB, method = "nominal")
k09 <- KrippAlpha(dm_BICA, method = "nominal")
k10 <- KrippAlpha(dm_CMOB, method = "nominal")
k11 <- KrippAlpha(dm_UXWM, method = "nominal")
k12 <- KrippAlpha(dm_PDNC, method = "nominal") #
k13 <- KrippAlpha(dm_AHNP, method = "nominal")
k14 <- KrippAlpha(dm_NCCM, method = "nominal")
k15 <- KrippAlpha(dm_IAHR, method = "nominal") #
k16 <- KrippAlpha(dm_DMAH, method = "nominal") #
k17 <- KrippAlpha(dm_SGCA, method = "nominal")
k18 <- KrippAlpha(dm_LCIE, method = "nominal")
k19 <- KrippAlpha(dm_ETNC, method = "nominal")
k20 <- KrippAlpha(dm_SCPI, method = "nominal")
k21 <- KrippAlpha(dm_NGIS, method = "nominal")
k22 <- KrippAlpha(dm_CMNE, method = "nominal")

#neuen Datensatz erstellen, in dem nur die Werte für Krippendorfs Alpha pro Code abgebildet sind
r3 <- data.frame(k01$value, k02$value, k03$value, k04$value, k05$value, k06$value, 
                 k07$value, k08$value, k09$value, k10$value, k11$value, 
                 k13$value, k14$value, k17$value, k18$value,
                 k19$value, k20$value, k21$value, k22$value)

#Umbenennen der Spalten
r3 <- r3 %>% rename(WANU = k01.value, CPFB = k02.value, EJHN = k03.value, CPMM = k04.value, NPIW = k05.value, PIII = k06.value, 
                    GPNE = k07.value, PMRB = k08.value, BICA = k09.value, CMOB = k10.value, UXWM = k11.value, 
                    AHNP = k13.value, NCCM = k14.value, SGCA = k17.value,
                    LCIE = k18.value, ETNC = k19.value, SCPI = k20.value, NGIS = k21.value, CMNE = k22.value)

#Umbenennen der Reihe
rownames(r3) <- "Temperatur: 1.0"

#Tausche von Spalten und Reihen
r3 <- tidyr::gather(r3, key = "Code", value = "Temperatur: 1.0")

# arithmetisches Mittel Krippendorffs Alpha bei Temperatur 1.0
r3_mean <- mean(r3$`Temperatur: 1.0`)

rg <- full_join(r2, r, by = "Code")
rg <- full_join(rg, r3, by = "Code")

rg_l <- gather(rg, key = "Temperatur", value = "Krippendorffs_Alpha", -Code)

rg_l$Krippendorffs_Alpha2 <- ifelse(is.na(rg_l$Krippendorffs_Alpha) == TRUE, "NA", "")



gg_k <- ggplot(rg_l, aes(x = Krippendorffs_Alpha, y = Code, fill = Temperatur)) +
  geom_col(width = 0.5, position = position_dodge(), show.legend = FALSE) +
  facet_wrap(~Temperatur) +
  labs(title = "Abb. 2: Krippendorffs Alpha nach Codes, deduktive Inhaltsanalyse mit GPT3.5 Turbo", 
       x = "",
       y = "",
       caption = "Krippendorffs Alpha nach Codes und Temperatur. NA für Codes, die bei der Kodierung nicht verwendet wurden") + 
  scale_fill_manual(values = c("#FFCC33", "#FF9933", "#CC3300")) +
  theme_dark() +
  geom_text(aes(x = Inf, label = Krippendorffs_Alpha2), 
            family = "Helvetica", fontface = "bold", vjust = 0.5, hjust = 13, 
            show.legend = FALSE, size = 3.5, colour = "grey20") +
  theme(text = element_text(family = "Helvetica"), 
        plot.title = element_text(family = "Helvetica"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12))

gg_k

#ggsave("gg_k_test.png", plot = gg_k)

library(officer)
library(flextable)

rg$`Temperatur: 0.0` <- round(rg$`Temperatur: 0.0`, digits = 3)
rg$`Temperatur: 0.5` <- round(rg$`Temperatur: 0.5`, digits = 3)
rg$`Temperatur: 1.0` <- round(rg$`Temperatur: 1.0`, digits = 3)

doc <- read_docx()
doc <- doc %>% body_add_par("Datensatz", style = "heading 1")
doc <- doc %>% body_add_table(data.frame((rg)))
print(doc, target = "Datensatz_Word.docx")

#######################################################################################
#Expertenkodierung
d_ex <- read_excel("Daten_Experten.xlsx")
View(d_ex)

d_ex <- d_ex[,-1]


d_WANU <- d_ex[, c(1, 26)]
d_CPFB <- d_ex[, c(2, 27)]
d_EJHN <- d_ex[, c(3, 28)]
d_CPMM <- d_ex[, c(4, 29)]
d_PBNV <- d_ex[, c(5, 30)]
d_NPIW <- d_ex[, c(6, 31)]
d_PIII <- d_ex[, c(7, 32)]
d_GPNE <- d_ex[, c(8, 33)]
d_PMRB <- d_ex[, c(9, 34)]
d_BICA <- d_ex[, c(10, 35)]
d_CMOB <- d_ex[, c(11, 36)]
d_UXWM <- d_ex[, c(12, 37)]
d_PDNC <- d_ex[, c(13, 38)]
d_AHNP <- d_ex[, c(14, 39)]
d_NCCM <- d_ex[, c(15, 40)]
d_IAHR <- d_ex[, c(16, 41)]
d_DMAH <- d_ex[, c(17, 42)]
d_SGCA <- d_ex[, c(18, 43)]
d_LCIE <- d_ex[, c(19, 44)]
d_ETNC <- d_ex[, c(20, 45)]
d_SCPI <- d_ex[, c(21, 46)]
d_NGIS <- d_ex[, c(22, 47)]
d_CMNE <- d_ex[, c(23, 48)]
d_WGNS <- d_ex[, c(24, 49)]
d_WERD <- d_ex[, c(25, 50)]


dm_WANU <- t(as.matrix(d_WANU))
dm_CPFB <- t(as.matrix(d_CPFB))
dm_EJHN <- t(as.matrix(d_EJHN))
dm_CPMM <- t(as.matrix(d_CPMM))
dm_PBNV <- t(as.matrix(d_PBNV))
dm_NPIW <- t(as.matrix(d_NPIW))
dm_PIII <- t(as.matrix(d_PIII)) 
dm_GPNE <- t(as.matrix(d_GPNE))
dm_PMRB <- t(as.matrix(d_PMRB))
dm_BICA <- t(as.matrix(d_BICA))
dm_CMOB <- t(as.matrix(d_CMOB))
dm_UXWM <- t(as.matrix(d_UXWM))
dm_PDNC <- t(as.matrix(d_PDNC)) #
dm_AHNP <- t(as.matrix(d_AHNP))
dm_NCCM <- t(as.matrix(d_NCCM))
dm_IAHR <- t(as.matrix(d_IAHR)) #
dm_DMAH <- t(as.matrix(d_DMAH)) #
dm_SGCA <- t(as.matrix(d_SGCA))
dm_LCIE <- t(as.matrix(d_LCIE))
dm_ETNC <- t(as.matrix(d_ETNC))
dm_SCPI <- t(as.matrix(d_SCPI))
dm_NGIS <- t(as.matrix(d_NGIS))
dm_CMNE <- t(as.matrix(d_CMNE))
dm_WGNS <- t(as.matrix(d_WGNS))
dm_WERD <- t(as.matrix(d_WERD))


k01 <- KrippAlpha(dm_WANU, method = "nominal") 
k02 <- KrippAlpha(dm_CPFB, method = "nominal")
k03 <- KrippAlpha(dm_EJHN, method = "nominal")
k04 <- KrippAlpha(dm_CPMM, method = "nominal")
k05 <- KrippAlpha(dm_NPIW, method = "nominal") #
k06 <- KrippAlpha(dm_PIII, method = "nominal") #
k07 <- KrippAlpha(dm_GPNE, method = "nominal") #
k08 <- KrippAlpha(dm_PMRB, method = "nominal") 
k09 <- KrippAlpha(dm_BICA, method = "nominal") #
k10 <- KrippAlpha(dm_CMOB, method = "nominal") 
k11 <- KrippAlpha(dm_UXWM, method = "nominal") #
k12 <- KrippAlpha(dm_PDNC, method = "nominal") #
k13 <- KrippAlpha(dm_AHNP, method = "nominal") #
k14 <- KrippAlpha(dm_NCCM, method = "nominal") #
k15 <- KrippAlpha(dm_IAHR, method = "nominal") 
k16 <- KrippAlpha(dm_DMAH, method = "nominal") 
k17 <- KrippAlpha(dm_SGCA, method = "nominal") 
k18 <- KrippAlpha(dm_LCIE, method = "nominal") 
k19 <- KrippAlpha(dm_ETNC, method = "nominal") #
k20 <- KrippAlpha(dm_SCPI, method = "nominal") #
k21 <- KrippAlpha(dm_NGIS, method = "nominal") #
k22 <- KrippAlpha(dm_CMNE, method = "nominal") 
k23 <- KrippAlpha(dm_WGNS, method = "nominal") 
k24 <- KrippAlpha(dm_WERD, method = "nominal") 


d_ex_coded <- d_ex %>% select_if(~ any(. == 1))
colnames(d_ex_coded)

#neuen Datensatz erstellen, in dem nur die Werte für Krippendorfs Alpha pro Code abgebildet sind
r4 <- data.frame(k01$value, k02$value, k03$value, k04$value, k08$value, k10$value, 
                 k15$value, k16$value, k17$value, k18$value, k22$value, k23$value, k24$value)

#Umbenennen der Spalten
r4 <- r4 %>% rename(WANU = k01.value, CPFB = k02.value, EJHN = k03.value, CPMM = k04.value,
                    PMRB = k08.value, CMOB = k10.value, IAHR = k15.value, DMAH = k16.value, 
                    SGCA = k17.value, LCIE = k18.value, CMNE = k22.value, WGNS = k23.value, WERD = k24.value)


r4 <- tidyr::gather(r4, key = "Code", value = "Krippendorffs_Alpha")

r4_mean <- mean(r4$Krippendorffs_Alpha)


g4_k <- ggplot(r4, aes(x = Krippendorffs_Alpha, y = Code)) + 
  geom_bar(stat = "identity", color = "skyblue") + 
  labs(title = "Balkendiagramm", x = "Krippendorfs Alpha", y = "Code") + theme_minimal()

g4_k



######### Grafik für Krippendorfs Alpha Mittelwerte
k_means <- data.frame(r2_mean, r_mean, r3_mean, r4_mean)
k_means <- k_means %>% rename(`GPT Temperatur: 0.5` = r_mean, `GPT Temperatur: 0.0` = r2_mean, `GPT Temperatur: 1.0` = r3_mean, `Experten` = r4_mean)

rownames(k_means) = "Krippendorffs_Alpha"
k_means <- k_means %>% select(Experten, `GPT Temperatur: 0.0`, `GPT Temperatur: 0.5`, `GPT Temperatur: 1.0`)

k_means2 <- tidyr::gather(k_means, key = "Coder", value = "Krippendorffs_Alpha")

k_übersicht <- ggplot(k_means2, aes(x = Coder, y = Krippendorffs_Alpha, fill = Coder)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Abb. 3: Durchschnitt von Krippendorffs Alpha je Codergruppe", 
       x = "", 
       y = "", 
       caption = "Durchschnitt gebildet aus Krippendorfs Alpha der Codes je Codergruppe.") +
  scale_fill_manual(values = c("#99CCFF", "#FFCC33", "#FF9933", "#CC3300")) +
  theme_dark() +
  theme(text = element_text(family = "Helvetica"), 
        plot.title = element_text(family = "Helvetica"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12))

k_übersicht

ggsave("k_übersicht.png", plot = k_übersicht)

doc <- doc %>% body_add_par("Datensatz", style = "heading 1")
doc <- doc %>% body_add_table(data.frame((k_means)))
print(doc, target = "Datensatz_Word.docx")



#################################################################################################
#Gepoolte Expertencodes mit ChatGPT Temperatur 0.5


d_ex_p <- read_excel("Experten_Pool.xlsx")
View(d_ex_p)

d_ex_p <- d_ex_p[,-1]

d2_05 <- d2_05 %>% mutate(WANU = ifelse(rowSums(select(.,contains("WANU"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(CPFB = ifelse(rowSums(select(.,contains("CPFB"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(EJHN = ifelse(rowSums(select(.,contains("EJHN"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(CPMM = ifelse(rowSums(select(.,contains("CPMM"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(PBNV = ifelse(rowSums(select(.,contains("PBNV"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(NPIW = ifelse(rowSums(select(.,contains("NPIW"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(PMRB = ifelse(rowSums(select(.,contains("PMRB"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(PIII = ifelse(rowSums(select(.,contains("PIII"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(GPNE = ifelse(rowSums(select(.,contains("GPNE"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(BICA = ifelse(rowSums(select(.,contains("BICA"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(CMOB = ifelse(rowSums(select(.,contains("CMOB"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(UXWM = ifelse(rowSums(select(.,contains("UXWM"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(PDNC = ifelse(rowSums(select(.,contains("PDNC"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(AHNP = ifelse(rowSums(select(.,contains("AHNP"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(NCCM = ifelse(rowSums(select(.,contains("NCCM"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(IAHR = ifelse(rowSums(select(.,contains("IAHR"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(DMAH = ifelse(rowSums(select(.,contains("DMAH"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(SGCA = ifelse(rowSums(select(.,contains("SGCA"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(LCIE = ifelse(rowSums(select(.,contains("LCIE"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(ETNC = ifelse(rowSums(select(.,contains("ETNC"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(SCPI = ifelse(rowSums(select(.,contains("SCPI"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(NGIS = ifelse(rowSums(select(.,contains("NGIS"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(CMNE = ifelse(rowSums(select(.,contains("CMNE"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(WGNS = ifelse(rowSums(select(.,contains("WGNS"))) > 0, 1, 0))
d2_05 <- d2_05 %>% mutate(WERD = ifelse(rowSums(select(.,contains("WERD"))) > 0, 1, 0))


GPT_05 <- d2_05 %>% select(-matches("GPT"))
GPT_05$ID <- seq(1,20,1)
d_ex_p$ID <- seq(1,20,1)

Ex_GPT_05 <- full_join(GPT_05, d_ex_p, by = "ID")
Ex_GPT_05 <- Ex_GPT_05[,-c(25)]

d_WANU <- Ex_GPT_05 %>% select(matches("WANU"))
d_CPFB <- Ex_GPT_05 %>% select(matches("CPFB"))
d_EJHN <- Ex_GPT_05 %>% select(matches("EJHN"))
d_CPMM <- Ex_GPT_05 %>% select(matches("CPMM"))
d_PBNV <- Ex_GPT_05 %>% select(matches("PBNV"))
d_NPIW <- Ex_GPT_05 %>% select(matches("NPIW"))
d_PIII <- Ex_GPT_05 %>% select(matches("PIII"))
d_GPNE <- Ex_GPT_05 %>% select(matches("GPNE"))
d_PMRB <- Ex_GPT_05 %>% select(matches("PMRB"))
d_BICA <- Ex_GPT_05 %>% select(matches("BICA"))
d_CMOB <- Ex_GPT_05 %>% select(matches("CMOB"))
d_UXWM <- Ex_GPT_05 %>% select(matches("UXWM"))
d_PDNC <- Ex_GPT_05 %>% select(matches("PDNC"))
d_AHNP <- Ex_GPT_05 %>% select(matches("AHNP"))
d_NCCM <- Ex_GPT_05 %>% select(matches("NCCM"))
d_IAHR <- Ex_GPT_05 %>% select(matches("IAHR"))
d_DMAH <- Ex_GPT_05 %>% select(matches("DMAH"))
d_SGCA <- Ex_GPT_05 %>% select(matches("SGCA"))
d_LCIE <- Ex_GPT_05 %>% select(matches("LCIE"))
d_ETNC <- Ex_GPT_05 %>% select(matches("ETNC"))
d_SCPI <- Ex_GPT_05 %>% select(matches("SCPI"))
d_NGIS <- Ex_GPT_05 %>% select(matches("NGIS"))
d_CMNE <- Ex_GPT_05 %>% select(matches("CMNE"))
d_WGNS <- Ex_GPT_05 %>% select(matches("WGNS"))
d_WERD <- Ex_GPT_05 %>% select(matches("WERD"))
d_WERD$WERD.x <- 0

dm_WANU <- t(as.matrix(d_WANU))
dm_CPFB <- t(as.matrix(d_CPFB))
dm_EJHN <- t(as.matrix(d_EJHN))
dm_CPMM <- t(as.matrix(d_CPMM))
dm_PBNV <- t(as.matrix(d_PBNV))
dm_NPIW <- t(as.matrix(d_NPIW))
dm_PIII <- t(as.matrix(d_PIII)) 
dm_GPNE <- t(as.matrix(d_GPNE))
dm_PMRB <- t(as.matrix(d_PMRB))
dm_BICA <- t(as.matrix(d_BICA))
dm_CMOB <- t(as.matrix(d_CMOB))
dm_UXWM <- t(as.matrix(d_UXWM))
dm_PDNC <- t(as.matrix(d_PDNC)) #
dm_AHNP <- t(as.matrix(d_AHNP))
dm_NCCM <- t(as.matrix(d_NCCM))
dm_IAHR <- t(as.matrix(d_IAHR)) #
dm_DMAH <- t(as.matrix(d_DMAH)) #
dm_SGCA <- t(as.matrix(d_SGCA))
dm_LCIE <- t(as.matrix(d_LCIE))
dm_ETNC <- t(as.matrix(d_ETNC))
dm_SCPI <- t(as.matrix(d_SCPI))
dm_NGIS <- t(as.matrix(d_NGIS))
dm_CMNE <- t(as.matrix(d_CMNE))
dm_WGNS <- t(as.matrix(d_WGNS))
dm_WERD <- t(as.matrix(d_WERD))


k01 <- KrippAlpha(dm_WANU, method = "nominal") 
k02 <- KrippAlpha(dm_CPFB, method = "nominal")
k03 <- KrippAlpha(dm_EJHN, method = "nominal")
k04 <- KrippAlpha(dm_CPMM, method = "nominal")
k05 <- KrippAlpha(dm_NPIW, method = "nominal") 
k06 <- KrippAlpha(dm_PIII, method = "nominal") #
k07 <- KrippAlpha(dm_GPNE, method = "nominal") 
k08 <- KrippAlpha(dm_PMRB, method = "nominal") 
k09 <- KrippAlpha(dm_BICA, method = "nominal") 
k10 <- KrippAlpha(dm_CMOB, method = "nominal") 
k11 <- KrippAlpha(dm_UXWM, method = "nominal") 
k12 <- KrippAlpha(dm_PDNC, method = "nominal") 
k13 <- KrippAlpha(dm_AHNP, method = "nominal") 
k14 <- KrippAlpha(dm_NCCM, method = "nominal") #
k15 <- KrippAlpha(dm_IAHR, method = "nominal") 
k16 <- KrippAlpha(dm_DMAH, method = "nominal") 
k17 <- KrippAlpha(dm_SGCA, method = "nominal") 
k18 <- KrippAlpha(dm_LCIE, method = "nominal") 
k19 <- KrippAlpha(dm_ETNC, method = "nominal") #
k20 <- KrippAlpha(dm_SCPI, method = "nominal") #
k21 <- KrippAlpha(dm_NGIS, method = "nominal") #
k22 <- KrippAlpha(dm_CMNE, method = "nominal") 
k23 <- KrippAlpha(dm_WGNS, method = "nominal") 
k24 <- KrippAlpha(dm_WERD, method = "nominal")


test <- Ex_GPT_05 %>% select_if(~ any(. == 1))
colnames(test)

#neuen Datensatz erstellen, in dem nur die Werte für Krippendorfs Alpha pro Code abgebildet sind
r5 <- data.frame(k01$value, k02$value, k03$value, k04$value, k05$value, k07$value, k08$value, k09$value, k10$value, 
                 k11$value, k12$value, k13$value, k15$value, k16$value, k17$value, k18$value, k22$value, k23$value, 
                 k24$value)

#Umbenennen der Spalten
r5 <- r5 %>% rename(WANU = k01.value, CPFB = k02.value, EJHN = k03.value, CPMM = k04.value, NPIW = k05.value, GPNE = k07.value,
                    PMRB = k08.value, BICA = k09.value, CMOB = k10.value, UXWM = k11.value, PDNC = k12.value, AHNP = k13.value,
                    IAHR = k15.value, DMAH = k16.value, SGCA = k17.value, LCIE = k18.value, CMNE = k22.value, WGNS = k23.value, 
                    WERD = k24.value)


r5 <- tidyr::gather(r5, key = "Code", value = "Krippendorffs_Alpha")

r5_mean <- mean(r5$Krippendorffs_Alpha)




############################################################################################################################
# Gepoolte Expertencodes mit gepoolten ChatGPT Temperatur 0.0 Ergebnissen

d2_00 <- d2_00 %>% mutate(WANU = ifelse(rowSums(select(.,contains("WANU"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(CPFB = ifelse(rowSums(select(.,contains("CPFB"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(EJHN = ifelse(rowSums(select(.,contains("EJHN"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(CPMM = ifelse(rowSums(select(.,contains("CPMM"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(PBNV = ifelse(rowSums(select(.,contains("PBNV"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(NPIW = ifelse(rowSums(select(.,contains("NPIW"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(PMRB = ifelse(rowSums(select(.,contains("PMRB"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(PIII = ifelse(rowSums(select(.,contains("PIII"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(GPNE = ifelse(rowSums(select(.,contains("GPNE"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(BICA = ifelse(rowSums(select(.,contains("BICA"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(CMOB = ifelse(rowSums(select(.,contains("CMOB"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(UXWM = ifelse(rowSums(select(.,contains("UXWM"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(PDNC = ifelse(rowSums(select(.,contains("PDNC"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(AHNP = ifelse(rowSums(select(.,contains("AHNP"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(NCCM = ifelse(rowSums(select(.,contains("NCCM"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(IAHR = ifelse(rowSums(select(.,contains("IAHR"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(DMAH = ifelse(rowSums(select(.,contains("DMAH"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(SGCA = ifelse(rowSums(select(.,contains("SGCA"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(LCIE = ifelse(rowSums(select(.,contains("LCIE"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(ETNC = ifelse(rowSums(select(.,contains("ETNC"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(SCPI = ifelse(rowSums(select(.,contains("SCPI"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(NGIS = ifelse(rowSums(select(.,contains("NGIS"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(CMNE = ifelse(rowSums(select(.,contains("CMNE"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(WGNS = ifelse(rowSums(select(.,contains("WGNS"))) > 0, 1, 0))
d2_00 <- d2_00 %>% mutate(WERD = ifelse(rowSums(select(.,contains("WERD"))) > 0, 1, 0))

GPT_00 <- d2_00 %>% select(-matches("GPT"))
GPT_00$ID <- seq(1,20,1)
#d_ex_p$ID <- seq(1,20,1)

Ex_GPT_00 <- full_join(GPT_00, d_ex_p, by = "ID")
Ex_GPT_00 <- Ex_GPT_00[,-c(26)]

d_WANU <- Ex_GPT_00 %>% select(matches("WANU"))
d_CPFB <- Ex_GPT_00 %>% select(matches("CPFB"))
d_EJHN <- Ex_GPT_00 %>% select(matches("EJHN"))
d_CPMM <- Ex_GPT_00 %>% select(matches("CPMM"))
d_PBNV <- Ex_GPT_00 %>% select(matches("PBNV"))
d_NPIW <- Ex_GPT_00 %>% select(matches("NPIW"))
d_PIII <- Ex_GPT_00 %>% select(matches("PIII"))
d_GPNE <- Ex_GPT_00 %>% select(matches("GPNE"))
d_PMRB <- Ex_GPT_00 %>% select(matches("PMRB"))
d_BICA <- Ex_GPT_00 %>% select(matches("BICA"))
d_CMOB <- Ex_GPT_00 %>% select(matches("CMOB"))
d_UXWM <- Ex_GPT_00 %>% select(matches("UXWM"))
d_PDNC <- Ex_GPT_00 %>% select(matches("PDNC"))
d_AHNP <- Ex_GPT_00 %>% select(matches("AHNP"))
d_NCCM <- Ex_GPT_00 %>% select(matches("NCCM"))
d_IAHR <- Ex_GPT_00 %>% select(matches("IAHR"))
d_DMAH <- Ex_GPT_00 %>% select(matches("DMAH"))
d_SGCA <- Ex_GPT_00 %>% select(matches("SGCA"))
d_LCIE <- Ex_GPT_00 %>% select(matches("LCIE"))
d_ETNC <- Ex_GPT_00 %>% select(matches("ETNC"))
d_SCPI <- Ex_GPT_00 %>% select(matches("SCPI"))
d_NGIS <- Ex_GPT_00 %>% select(matches("NGIS"))
d_CMNE <- Ex_GPT_00 %>% select(matches("CMNE"))
d_WGNS <- Ex_GPT_00 %>% select(matches("WGNS"))
d_WERD <- Ex_GPT_00 %>% select(matches("WERD"))


dm_WANU <- t(as.matrix(d_WANU))
dm_CPFB <- t(as.matrix(d_CPFB))
dm_EJHN <- t(as.matrix(d_EJHN))
dm_CPMM <- t(as.matrix(d_CPMM))
dm_PBNV <- t(as.matrix(d_PBNV))
dm_NPIW <- t(as.matrix(d_NPIW))
dm_PIII <- t(as.matrix(d_PIII)) 
dm_GPNE <- t(as.matrix(d_GPNE))
dm_PMRB <- t(as.matrix(d_PMRB))
dm_BICA <- t(as.matrix(d_BICA))
dm_CMOB <- t(as.matrix(d_CMOB))
dm_UXWM <- t(as.matrix(d_UXWM))
dm_PDNC <- t(as.matrix(d_PDNC)) #
dm_AHNP <- t(as.matrix(d_AHNP))
dm_NCCM <- t(as.matrix(d_NCCM))
dm_IAHR <- t(as.matrix(d_IAHR)) #
dm_DMAH <- t(as.matrix(d_DMAH)) #
dm_SGCA <- t(as.matrix(d_SGCA))
dm_LCIE <- t(as.matrix(d_LCIE))
dm_ETNC <- t(as.matrix(d_ETNC))
dm_SCPI <- t(as.matrix(d_SCPI))
dm_NGIS <- t(as.matrix(d_NGIS))
dm_CMNE <- t(as.matrix(d_CMNE))
dm_WGNS <- t(as.matrix(d_WGNS))
dm_WERD <- t(as.matrix(d_WERD))


k01 <- KrippAlpha(dm_WANU, method = "nominal") 
k02 <- KrippAlpha(dm_CPFB, method = "nominal")
k03 <- KrippAlpha(dm_EJHN, method = "nominal")
k04 <- KrippAlpha(dm_CPMM, method = "nominal")
k05 <- KrippAlpha(dm_NPIW, method = "nominal") #
k06 <- KrippAlpha(dm_PIII, method = "nominal") #
k07 <- KrippAlpha(dm_GPNE, method = "nominal") 
k08 <- KrippAlpha(dm_PMRB, method = "nominal") 
k09 <- KrippAlpha(dm_BICA, method = "nominal") 
k10 <- KrippAlpha(dm_CMOB, method = "nominal") 
k11 <- KrippAlpha(dm_UXWM, method = "nominal") #
k12 <- KrippAlpha(dm_PDNC, method = "nominal") 
k13 <- KrippAlpha(dm_AHNP, method = "nominal") 
k14 <- KrippAlpha(dm_NCCM, method = "nominal") #
k15 <- KrippAlpha(dm_IAHR, method = "nominal") 
k16 <- KrippAlpha(dm_DMAH, method = "nominal") 
k17 <- KrippAlpha(dm_SGCA, method = "nominal") 
k18 <- KrippAlpha(dm_LCIE, method = "nominal") 
k19 <- KrippAlpha(dm_ETNC, method = "nominal") #
k20 <- KrippAlpha(dm_SCPI, method = "nominal") #
k21 <- KrippAlpha(dm_NGIS, method = "nominal") #
k22 <- KrippAlpha(dm_CMNE, method = "nominal") 
k23 <- KrippAlpha(dm_WGNS, method = "nominal") 
k24 <- KrippAlpha(dm_WERD, method = "nominal") 


test <- Ex_GPT_00 %>% select_if(~ any(. == 1))
colnames(test)

#neuen Datensatz erstellen, in dem nur die Werte für Krippendorfs Alpha pro Code abgebildet sind
r6 <- data.frame(k01$value, k02$value, k03$value, k04$value, k07$value, k08$value, k09$value, k10$value, 
                 k12$value, k13$value, k15$value, k16$value, k17$value, k18$value, k22$value, k23$value, 
                 k24$value)

#Umbenennen der Spalten
r6 <- r6 %>% rename(WANU = k01.value, CPFB = k02.value, EJHN = k03.value, CPMM = k04.value, GPNE = k07.value,
                    PMRB = k08.value, BICA = k09.value, CMOB = k10.value, PDNC = k12.value, AHNP = k13.value,
                    IAHR = k15.value, DMAH = k16.value, SGCA = k17.value, LCIE = k18.value, CMNE = k22.value, WGNS = k23.value, 
                    WERD = k24.value)


r6<- tidyr::gather(r6, key = "Code", value = "Krippendorffs_Alpha")

r6_mean <- mean(r6$Krippendorffs_Alpha)


#########################################################################################################################
#Gepoolte Expertencodes mit gepoolten ChatGPT Temperatur 0.0 Ergebnissen
d2_01 <- d2_01 %>% mutate(WANU = ifelse(rowSums(select(.,contains("WANU"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(CPFB = ifelse(rowSums(select(.,contains("CPFB"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(EJHN = ifelse(rowSums(select(.,contains("EJHN"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(CPMM = ifelse(rowSums(select(.,contains("CPMM"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(PBNV = ifelse(rowSums(select(.,contains("PBNV"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(NPIW = ifelse(rowSums(select(.,contains("NPIW"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(PMRB = ifelse(rowSums(select(.,contains("PMRB"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(PIII = ifelse(rowSums(select(.,contains("PIII"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(GPNE = ifelse(rowSums(select(.,contains("GPNE"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(BICA = ifelse(rowSums(select(.,contains("BICA"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(CMOB = ifelse(rowSums(select(.,contains("CMOB"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(UXWM = ifelse(rowSums(select(.,contains("UXWM"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(PDNC = ifelse(rowSums(select(.,contains("PDNC"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(AHNP = ifelse(rowSums(select(.,contains("AHNP"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(NCCM = ifelse(rowSums(select(.,contains("NCCM"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(IAHR = ifelse(rowSums(select(.,contains("IAHR"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(DMAH = ifelse(rowSums(select(.,contains("DMAH"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(SGCA = ifelse(rowSums(select(.,contains("SGCA"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(LCIE = ifelse(rowSums(select(.,contains("LCIE"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(ETNC = ifelse(rowSums(select(.,contains("ETNC"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(SCPI = ifelse(rowSums(select(.,contains("SCPI"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(NGIS = ifelse(rowSums(select(.,contains("NGIS"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(CMNE = ifelse(rowSums(select(.,contains("CMNE"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(WGNS = ifelse(rowSums(select(.,contains("WGNS"))) > 0, 1, 0))
d2_01 <- d2_01 %>% mutate(WERD = ifelse(rowSums(select(.,contains("WERD"))) > 0, 1, 0))

GPT_01 <- d2_01 %>% select(-matches("GPT"))
GPT_01$ID <- seq(1,20,1)


Ex_GPT_01 <- full_join(GPT_01, d_ex_p, by = "ID")
Ex_GPT_01 <- Ex_GPT_01[,-c(26)]

d_WANU <- Ex_GPT_01 %>% select(matches("WANU"))
d_CPFB <- Ex_GPT_01 %>% select(matches("CPFB"))
d_EJHN <- Ex_GPT_01 %>% select(matches("EJHN"))
d_CPMM <- Ex_GPT_01 %>% select(matches("CPMM"))
d_PBNV <- Ex_GPT_01 %>% select(matches("PBNV"))
d_NPIW <- Ex_GPT_01 %>% select(matches("NPIW"))
d_PIII <- Ex_GPT_01 %>% select(matches("PIII"))
d_GPNE <- Ex_GPT_01 %>% select(matches("GPNE"))
d_PMRB <- Ex_GPT_01 %>% select(matches("PMRB"))
d_BICA <- Ex_GPT_01 %>% select(matches("BICA"))
d_CMOB <- Ex_GPT_01 %>% select(matches("CMOB"))
d_UXWM <- Ex_GPT_01 %>% select(matches("UXWM"))
d_PDNC <- Ex_GPT_01 %>% select(matches("PDNC"))
d_AHNP <- Ex_GPT_01 %>% select(matches("AHNP"))
d_NCCM <- Ex_GPT_01 %>% select(matches("NCCM"))
d_IAHR <- Ex_GPT_01 %>% select(matches("IAHR"))
d_DMAH <- Ex_GPT_01 %>% select(matches("DMAH"))
d_SGCA <- Ex_GPT_01 %>% select(matches("SGCA"))
d_LCIE <- Ex_GPT_01 %>% select(matches("LCIE"))
d_ETNC <- Ex_GPT_01 %>% select(matches("ETNC"))
d_SCPI <- Ex_GPT_01 %>% select(matches("SCPI"))
d_NGIS <- Ex_GPT_01 %>% select(matches("NGIS"))
d_CMNE <- Ex_GPT_01 %>% select(matches("CMNE"))
d_WGNS <- Ex_GPT_01 %>% select(matches("WGNS"))
d_WERD <- Ex_GPT_01 %>% select(matches("WERD"))


dm_WANU <- t(as.matrix(d_WANU))
dm_CPFB <- t(as.matrix(d_CPFB))
dm_EJHN <- t(as.matrix(d_EJHN))
dm_CPMM <- t(as.matrix(d_CPMM))
dm_PBNV <- t(as.matrix(d_PBNV))
dm_NPIW <- t(as.matrix(d_NPIW))
dm_PIII <- t(as.matrix(d_PIII)) 
dm_GPNE <- t(as.matrix(d_GPNE))
dm_PMRB <- t(as.matrix(d_PMRB))
dm_BICA <- t(as.matrix(d_BICA))
dm_CMOB <- t(as.matrix(d_CMOB))
dm_UXWM <- t(as.matrix(d_UXWM))
dm_PDNC <- t(as.matrix(d_PDNC)) #
dm_AHNP <- t(as.matrix(d_AHNP))
dm_NCCM <- t(as.matrix(d_NCCM))
dm_IAHR <- t(as.matrix(d_IAHR)) #
dm_DMAH <- t(as.matrix(d_DMAH)) #
dm_SGCA <- t(as.matrix(d_SGCA))
dm_LCIE <- t(as.matrix(d_LCIE))
dm_ETNC <- t(as.matrix(d_ETNC))
dm_SCPI <- t(as.matrix(d_SCPI))
dm_NGIS <- t(as.matrix(d_NGIS))
dm_CMNE <- t(as.matrix(d_CMNE))
dm_WGNS <- t(as.matrix(d_WGNS))
dm_WERD <- t(as.matrix(d_WERD))


k01 <- KrippAlpha(dm_WANU, method = "nominal") 
k02 <- KrippAlpha(dm_CPFB, method = "nominal")
k03 <- KrippAlpha(dm_EJHN, method = "nominal")
k04 <- KrippAlpha(dm_CPMM, method = "nominal")
k05 <- KrippAlpha(dm_NPIW, method = "nominal") 
k06 <- KrippAlpha(dm_PIII, method = "nominal") 
k07 <- KrippAlpha(dm_GPNE, method = "nominal") 
k08 <- KrippAlpha(dm_PMRB, method = "nominal") 
k09 <- KrippAlpha(dm_BICA, method = "nominal") 
k10 <- KrippAlpha(dm_CMOB, method = "nominal") 
k11 <- KrippAlpha(dm_UXWM, method = "nominal") 
k12 <- KrippAlpha(dm_PDNC, method = "nominal") # 
k13 <- KrippAlpha(dm_AHNP, method = "nominal") 
k14 <- KrippAlpha(dm_NCCM, method = "nominal") 
k15 <- KrippAlpha(dm_IAHR, method = "nominal") 
k16 <- KrippAlpha(dm_DMAH, method = "nominal") 
k17 <- KrippAlpha(dm_SGCA, method = "nominal") 
k18 <- KrippAlpha(dm_LCIE, method = "nominal") 
k19 <- KrippAlpha(dm_ETNC, method = "nominal") 
k20 <- KrippAlpha(dm_SCPI, method = "nominal") 
k21 <- KrippAlpha(dm_NGIS, method = "nominal") 
k22 <- KrippAlpha(dm_CMNE, method = "nominal") 
k23 <- KrippAlpha(dm_WGNS, method = "nominal") 
k24 <- KrippAlpha(dm_WERD, method = "nominal") 
k25 <- KrippAlpha(dm_PBNV, method = "nominal")


test <- Ex_GPT_01 %>% select_if(~ any(. == 1))
colnames(test)

#neuen Datensatz erstellen, in dem nur die Werte für Krippendorfs Alpha pro Code abgebildet sind
r7 <- data.frame(k01$value, k02$value, k03$value, k04$value, k05$value, k06$value, k07$value, k08$value, k09$value, k10$value, k11$value,
                 k13$value, k14$value, k15$value, k16$value, k17$value, k18$value, k19$value, k20$value, k21$value, k22$value, k23$value, 
                 k24$value, k25$value)

#Umbenennen der Spalten
r7 <- r7 %>% rename(WANU = k01.value, CPFB = k02.value, EJHN = k03.value, CPMM = k04.value, NPIW = k05.value, PIII = k06.value, GPNE = k07.value,
                    PMRB = k08.value, BICA = k09.value, CMOB = k10.value, UXWM = k11.value, AHNP = k13.value, NCCM = k14.value, IAHR = k15.value, 
                    DMAH = k16.value, SGCA = k17.value, LCIE = k18.value, ETNC = k19.value, SCPI = k20.value, NGIS = k21.value, CMNE = k22.value, 
                    WGNS = k23.value, WERD = k24.value, PBNV = k25.value)


r7<- tidyr::gather(r7, key = "Code", value = "Krippendorffs_Alpha")

rg2 <- full_join(r5, r6, by = "Code")
rg2 <- full_join(rg2, r7, by = "Code")

rg2 <- rg2 %>% rename(`Temp = 0.5 & Experten` = Krippendorffs_Alpha.x , `Temp = 0.0 & Experten` = Krippendorffs_Alpha.y, `Temp = 1.0 & Experten` = Krippendorffs_Alpha)

rg_2 <- gather(rg2, key = "Temperatur", value = "Krippendorffs_Alpha", -Code)

r7_mean <- mean(r7$Krippendorffs_Alpha)


rg_2$Krippendorffs_Alpha2 <- ifelse(is.na(rg_2$Krippendorffs_Alpha) == TRUE, "NA", "")



gg_ex_gpt <- ggplot(rg_2, aes(x = Krippendorffs_Alpha, y = Code, fill = Temperatur)) +
  geom_col(width = 0.5, position = position_dodge(), show.legend = FALSE) +
  facet_wrap(~Temperatur) +
  labs(title = "Abb. 4: Krippendorffs Alpha nach Codes, deduktive Inhaltsanalyse mit GPT3.5 Turbo", 
       x = "", 
       y = "", 
       caption = "Krippendorffs Alpha von GPT3.5 Turbo und Experten, nach Temperatur und Code. NA für Codes, die bei der Kodierung nicht verwendet wurden") + 
  scale_fill_manual(values = c("#FFCC33", "#FF9933", "#CC3300")) + 
  theme_dark() +
  geom_text(aes(x = Inf, label = Krippendorffs_Alpha2), 
            family = "Helvetica",fontface = "bold", vjust = 0.5, hjust = 11, 
            show.legend = FALSE, size = 3.5, colour = "grey20") +
  theme(text = element_text(family = "Helvetica"), 
        plot.title = element_text(family = "Helvetica"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12))
print(gg_ex_gpt)

ggsave("gg_ex_gpt_final_finak.png", plot = gg_ex_gpt) 


rg2$`Temp = 0.0 & Experten` <- round(rg2$`Temp = 0.0 & Experten`, digits = 3)
rg2$`Temp = 0.5 & Experten` <- round(rg2$`Temp = 0.5 & Experten`, digits = 3)
rg2$`Temp = 1.0 & Experten` <- round(rg2$`Temp = 1.0 & Experten`, digits = 3)

doc <- doc %>% body_add_par("Datensatz", style = "heading 1")
doc <- doc %>% body_add_table(data.frame((rg2)))
print(doc, target = "Datensatz_Word.docx")


###########################################################################################################
#Grafik für Krippendorfs Alpha Mittelwerte Expert+GPT
k_means_g <- data.frame(r5_mean, r6_mean, r7_mean)
k_means_g <- k_means_g %>% rename(`GPT Temperatur: 0.5 & Experten` = r5_mean, `GPT Temperatur: 0.0 & Experten` = r6_mean, `GPT Temperatur: 1.0 & Experten` = r7_mean)

rownames(k_means_g) = "Krippendorffs_Alpha"
#k_means_g <- k_means_g %>% select(Experten, `GPT Temperatur: 0.0`, `GPT Temperatur: 0.5`, `GPT Temperatur: 1.0`)

k_means2_g <- tidyr::gather(k_means_g, key = "Coder", value = "Krippendorffs_Alpha")

k_übersicht_g <- ggplot(k_means2_g, aes(x = stringr::str_wrap(Coder, 20), y = Krippendorffs_Alpha, fill = Coder)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Abb. 3: Durchschnitt von Krippendorffs Alpha - Experten und GPT3.5 Turbo nach Temperatur", 
       x = "", 
       y = "", 
       caption = "Durchschnitt gebildet aus Krippendorfs Alpha der Codes zwischen Experten und GPT3.5 Turbo") +
  scale_fill_manual(values = c("#FFCC33", "#FF9933", "#CC3300")) +
  ylim(-0.05, 0.2) +
  theme_dark() + 
  theme(text = element_text(family = "Helvetica"), 
        plot.title = element_text(family = "Helvetica"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12))

k_übersicht_g

ggsave("k_übersicht_G.png", plot = k_übersicht_g)

doc <- doc %>% body_add_par("Datensatz", style = "heading 1")
doc <- doc %>% body_add_table(data.frame((k_means)))
print(doc, target = "Datensatz_Word.docx")
