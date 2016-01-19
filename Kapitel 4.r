# Schendera 2007 Datenqualität

##############################

# Kapitel 4: Einheitlichkeit

##############################

# S. 41

# Datensatz erstellen (und als "dat" ablegen)
dat <- data.frame(ID = c(1,2,3,4,5,6,7),
                  Befund =c("positiv", "+", "negativ", "-", "positiv", "psosiiv", "+"),
                  stringsAsFactors = FALSE)
dat

# BEFCODE mit Befunden als Faktor erstellen
BEFCODE <- rep(999, nrow(dat))              # BEFCODE erzeugen und mit "999" füllen
BEFCODE[dat$Befund == "positiv"] <- 1       # "positiv" als 1 kodieren
BEFCODE[dat$Befund == "+"]       <- 1       # "+" als 1 kodieren
BEFCODE[dat$Befund == "negativ"] <- 0       # "negativ" als 0 kodieren
BEFCODE[dat$Befund == "-"]       <- 0       # "-" als 0 kodieren
BEFCODE <- as.factor(BEFCODE)               # BEFCODE in Faktor umwandeln
levels(BEFCODE) <- c("negativ", "positiv")  # Wertelabels vergeben
# Bewirkt die Fehlermeldung "...number of levels differs."
levels(BEFCODE)                             # Levels prüfen
table(dat$Befund[BEFCODE == 999])
levels(BEFCODE) <- c("negativ", "positiv", "999")

BEFCODE

ls()
dat <- cbind(dat,BEFCODE)
dat
rm(BEFCODE)
ls()

##################
# S. 43 f.
##################

library(foreign)
dat  <- read.spss("C:/IHREDATEN.sav")
dat  <- read.spss("E:\\Dokumente und Einstellungen\\toshi\\Eigene Dateien\\R\\Datenqualität\\IHREDATEN.sav")
dat  <- data.frame(dat, stringsAsFactors = FALSE)
subset(dat, dat$GENDER != "m       " & dat$GENDER != "w       ") # Achtung: Beim Import von SPSS werden Stringvariablen bis zur in SPSS eingestellten Variablenlänge mit Leerzeichen aufgefüllt. Wenn man die SPSS-Daten als tabulatorgetrennte Textdatei speichert und mit read.table(c:IHREDATEN.txt, header = TRUE, stringsAsFactors = FALSE) nach R importiert, hat man dieses Problem nicht.
dat$GENDER <- sub(" +$", "", dat$GENDER)  # löscht endständige Leerzeichen (vergl. ?regexp)
subset(dat, dat$GENDER != "m" & dat$GENDER != "w")


dat[-grep("^(ISEF|JIOJ|KERK|LPMS|MEDE|OJFW|OWJG|P2PC|PMKO|STST|TEKL|UHNS|WERE)$", dat$statusg),]

dat[-which(grepl("^(ISEF|JIOJ|KERK|LPMS|MEDE|OJFW|OWJG|P2PC|PMKO|STST|TEKL|UHNS|WERE)$", dat$statusg) | grepl("^(m|w)$", dat$GENDER)),]

# Verdeutlicht NICHT A &  NICHT B = NICHT (A | B)
daten <- data.frame(c("ja", "ja", "nein", "nein"), c("ja", "nein", "ja", "nein"))
names(daten) <- c("A", "B")
daten
daten[which(grepl("ja", daten$A) | grepl("ja", daten$B)),]
daten[-which(grepl("ja", daten$A) | grepl("ja", daten$B)),]
daten[which(grepl("ja", daten$A) & grepl("ja", daten$B)),]
daten[-which(grepl("ja", daten$A) & grepl("ja", daten$B)),]
rm(daten)

# Äquivalent zum Lösungsansatz mit Labels
status.korr <- dat$statusg
status.korr[grep("^(ISEF|JIOJ|KERK|LPMS|MEDE|OJFW|OWJG|P2PC|PMKO|STST|TEKL|UHNS|WERE)$", dat$statusg)] <- "korrekt"
status.korr


###########################
# S. 46 ff.
############################

(TESTWORT <- "HELLO")
(Test_RP1 <- sub("L", "y", TESTWORT))

(TESTWORT <- "HEyLO")
(Test_RP2 <- gsub("L", "y", TESTWORT))

(TESTWORT <- "HEL:LO")
(Test_RP3 <- gsub(":", "-", TESTWORT))

(TESTWORT <- "    ")
(Test_RP4 <- gsub("    ", "_BLANK!_", TESTWORT))

(TESTWORT <- "123456789012345678901234567890")
(Test_RP5 <- gsub("12345678", "---", TESTWORT))

TESTWORT <- "1234567890123456789012345678901234567890"
Test_RP6 <- TESTWORT
for(i in 1:2){
Test_RP6 <- sub("1234567890", "eins bis zehn ", Test_RP6)}
Test_RP6


#########################
4.4 - Seite 47 ff.
#########################

dat <- data.frame(ID = c(1,2,3,4,5,6,7,8,9,10), Wein = c("Müller-Thurgau", "  Müller-Thurgau", "Mueller-Thurgau", "Müller Thrugau", "MÜLLER THURGAU", "Riesling", "RIESLING", "Silvaner", "SILVANER", "Silvahner"), stringsAsFactors = FALSE)
dat

dat$Wein[agrep("Müller-Thurgau", dat$Wein, ignore.case = TRUE)]  <- "Müller-Thurgau"
dat$Wein[agrep("Riesling", dat$Wein, ignore.case = TRUE)]  <- "Riesling"
dat$Wein[agrep("Silvaner", dat$Wein, ignore.case = TRUE)]  <- "Silvaner"
dat

sub("^ +", "", "    vorangestellte Leerzeichen löschen")     # entspricht LTRIM
toupper("wandelt den String in Großbuchstaben um")
tolower("WANDELT DEN STRING IN KLEINBUCHSTABEN UM")


################
#4.5 - Seite 51 f.
################

dat <- data.frame(ID = c(1,2,3,4,5), PREISE = c("$100000", "$10000", "$1000", "$100", "$10"), stringsAsFactors = FALSE)
dat

dat$PREISE2 <- sub("[$]", "€", dat$PREISE)


###

PARAGRAF <- c(123, 456, 789)
PARAMIT <- paste("$", PARAGRAF, sep = "")
data.frame(PARAGRAF, PARAMIT)
  


################
#4.6 - Seite 52 f.
################

dat <- data.frame(JAHR = c(1998, 1999, 2000, 2001, 2002, 2003, 2004), PREIS = c(1, 10, 100, 1000, 5000, 10000, 15000))
dat

dat$EPREIS <- dat$PREIS
dat$EPREIS[dat$JAHR < 2002] <- round(dat$PREIS[dat$JAHR < 2002]/1.95583, 2)


###############
# 4.7 - S. 53 ff.
###############

dat <- data.frame(ID = c(1,2,3,4,5), KONZERN = c("Daimler-Benz", "Mercedes-Benz", "Daimler-Chrysler", "MB", "Benz-Daimler"))
KONZERN2 <- as.character(dat$KONZERN)
KONZERN2[grep("Daimler|Mercedes", dat$KONZERN)] <- "Daimler-Chrysler"
data.frame(dat, KONZERN2)


KONZERN <-  c("IBM", "Industrial Business Machines", "IBM Ltd.", 
              "Industrial Business Machines International", "MB", 
              "Daimler-Benz", "DaimlerChrysler", "Mercedes-Benz", "Daimler-Chrysler")
AKRONYM <- KONZERN
AKRONYM[grep("Industrial Business Machines|IBM", KONZERN)] <- "IBM"
AKRONYM[grep("MB|Daimler|Benz|Mercedes", KONZERN)] <- "DC"
data.frame(KONZERN, AKRONYM)


####################
# 4.8 - S. 55 f.
####################

STRING <- c("STRINGA", "STRINGB000", "STRINGC0000", "STRINGD00000")
STRING2 <- gsub("0+$", "", STRING)
data.frame(STRING, STRING2)


##################
#4.9.1 - S.56 f.
#################

dat <- data.frame(ID = c(1,2,3,4,5), KONZERN = c("Daimler", "Benz-Daimler", "Daimler-Chrysler", "MB-Daimler", " Daimler-Benz"))
ZAEHLER <- rep(NA, 5)
ZAEHLER[grep("Daimler|Chrysler|MB", dat$KONZERN)] <- 1
data.frame(dat, ZAEHLER)

####################
# 4.9.2 - S. 57 f.
##################

dat <- data.frame(ID = c(1,2,3,3,4,5,6), 
                  KONZERN = c("Daimler", "Benz-Daimler", "Mercedes-Benz", 
                              "Daimler-Chrysler", "MB-Daimler", " DAIMLER-Benz", "    Benz"))
ZAEHLER <- rep(NA, 7)
ZAEHLER[grep("Daimler|Merceds|Benz|Chrysler", dat$KONZERN, ignore.case = TRUE)] <- 1
data.frame(dat, ZAEHLER)


#################
# 4.10.1 - S. 59
#################

TNUMMER <- c("(491) 234-567", "(+4912) 3-4567", "+49/123-4567", 
             "+49+123-4567 PRIV", "49 123 4567", "49-1234567", "+49 123-4567",
             "+491234567")
TNALT <- TNUMMER
TNUMMER <- gsub("[[:space:]]|[[:punct:]]|[[:alpha:]]", "", TNUMMER)
data.frame(TNALT, TNUMMER)


#################
# 4.10.2 - S. 60 f.
#################

TNALT <- c("(+49) 123-4567", "(+49)123-4567", "+49/123-4567", 
           "123-4567 PRIV", "123-4567/8976", "123 4567", "1234567", 
           "+49 123-4567", "491234567")
TNUMMER <- TNALT
TNUMMER <- gsub("49|/....$|[[:space:]]|[[:punct:]]|[[:alpha:]]", "", TNUMMER)
TNUMMER <- paste(substr(TNUMMER, 1, 3), "-", substr(TNUMMER, 4, 7), sep = "")
data.frame(TNALT, TNUMMER)


################
# 4.11.1 - S. 63
################

MIXDATEN <- c("1/7/99", "12/11/98", "1/12/01", "10/1/98", "12-8-2002", 
              "10-7-00", "1.12.99")
DATEN2 <- gsub("[[:punct:]]", "/", MIXDATEN)
DATEUS <- strptime(DATEN2, "%d/%m/%y")  
DATEUS[grep("/[[:digit:]]{4}$", DATEN2)] <- strptime(DATEN2[grep("/[[:digit:]]{4}$", DATEN2)], "%d/%m/%Y")
DATEEU <- strptime(DATEUS, "%Y-%d-%m")
data.frame(MIXDATEN, DATEUS, DATEEU)


ZEIT1 = strptime(c("13/08/90"), "%d/%m/%y")
ZEIT2 = strptime(c("21/10/90"), "%d/%m/%y")
FEHLER = ZEIT1 <= dat$ZEIT2


#######################
# 4.12.1 S. 67 ff.
#######################

NOKOMMA <- c(1, 123, 12345, 12345678)
KOMMA <- NOKOMMA * 10^(3-nchar(NOKOMMA))
cbind(NOKOMMA, KOMMA)


NOKOMMA <- c(1, 123, 12345, 12345678)
NSTRING2 <- formatC(as.character(NOKOMMA))
NSTRING2 <- gsub(" ", "0", NSTRING2)
data.frame(NOKOMMA, NSTRING2)


NOKOMMA <- c(1, 123, 12345, 12345678)
KOMMAS <- formatC(NOKOMMA * 10^(3 - nchar(NOKOMMA)), digits = 5, format = "f")
KOMMAS <- paste(substr(KOMMAS, 1, 6), ".", substr(KOMMAS, 7, 8), sep = "")
KOMMAS2 <- gsub(".", ":", KOMMAS, fixed = TRUE)
data.frame(NOKOMMA, KOMMAS, KOMMAS2)


dat <- data.frame(VARX = 123:128, VARY = 123:128)
(LABEL2 <- paste(dat$VARX, ":", dat$VARY, sep = ""))
plot(dat$VARX, dat$VARY)
text(dat$VARX, dat$VARY + 0.1, labels = LABEL2, cex = 0.7, adj = c(0.5, 0))


dat <- matrix(c("AA", "AA", "AA", "BB", "BB", "BB", "CC", "CC", "CC", "AA", "BB", "CC",
         "AA", "BB", "CC", "AA", "BB", "BB", "BB", "CC", "AA", "AA", "BB", "AA",
         "AA", "AA", "AA", "BB", "BB", "BB", "CC", "CC", "CC", "AA", "BB", "CC",
         "AA", "BB", "CC", "AA", "BB", "BB", "BB", "CC", "AA", "AA", "BB", "AA"),
       ncol = 4, byrow = TRUE)
KODE <- factor(paste(dat[,1], ":", dat[,2], ":", dat[,3], ":", dat[,4], sep = ""))
KODENUM <- unclass(KODE)
data.frame(KODE, KODENUM)


##################
# 4.12.2 - S. 73 ff.
##################

KOMMA <- c("1:2.3456", "12.:3456", "1.23:45.6", "1.234:56", "1.23.45:6")
KOMMA2 <- KOMMA
KOMMA <- gsub("[[:punct:]]", "", KOMMA)
data.frame(KOMMA, KOMMA2)


KOMMA <- c("1:23456", "12:3456", "123:456", "1234:56", "12345:6")
# Einfache Version
OHNEKOMMA <- gsub(":", "", KOMMA)
data.frame(KOMMA, OHNEKOMMA)
# Komplizierte Version (wie im Buch)
STELLE <- regexpr(":", KOMMA)
VORKOMMA <- substr(KOMMA, 1, STELLE-1)
NACHKOMMA <- substr(KOMMA, STELLE+1, nchar(KOMMA))
OHNEKOMMA <- paste(VORKOMMA, NACHKOMMA, sep = "")
data.frame(KOMMA, STELLE, VORKOMMA, NACHKOMMA, OHNEKOMMA)


KOMMA <- c("1:23456", "12:3456", "123:456", "1:234:56", "12345:6")
OHNEKOMMA <- sub(":", "", KOMMA)
data.frame(KOMMA, OHNEKOMMA)

MEHRERE <- grepl(":.*:", KOMMA)
STELLE <- regexpr(":", KOMMA)
STELLE[MEHRERE == TRUE] <- regexpr(":.*:", KOMMA[MEHRERE == TRUE]) + 
                           attr(regexpr(":.*:", KOMMA[MEHRERE == TRUE]), 
                                        "match.length") -1
OHNEKOMMA <- paste(substr(KOMMA, 1, STELLE-1),
                   substr(KOMMA, STELLE+1, nchar(KOMMA)), sep = "")
data.frame(KOMMA, MEHRERE, STELLE, OHNEKOMMA)


###################
# 4.12.3 - S. 77 f.
###################

MIXDATUM <- c("14/8/2005", "14-8-2005", "14.8.2005", "14.8/2005", 
              "14.8-2005", "14-8/2005", "14/8.2005", "14/8/2005", 
              "14-8.2005", "14.8-2005", "14/8.2005")
DATUM <- strptime(gsub("[[:punct:]]", ".", MIXDATUM), "%d.%m.%Y")

TAG   <- DATUM$mday
MONAT <- DATUM$mon +1
JAHR  <- DATUM$year + 1900
data.frame(MIXDATUM, TAG, MONAT, JAHR, DATUM)


####################
# 4.13 - S. 78 f.
###################

dat <- data.frame(ID = 1:4,
                  VAR_1 = c(99,2,98,4), 
                  VAR_2 = c(1,99,3,4), 
                  VAR_STRG = c("eins", "zwei", "", "vier"))
dat.orig <- dat
dat$VAR_1[dat$VAR_1 == 98 | dat$VAR_1 == 99] <- NA
dat$VAR_2[dat$VAR_2 == 99] <- NA
dat$VAR_STRG[dat$VAR_STRG == ""]  <- NA
cbind(dat.orig, dat)


dat <- data.frame(ID = 1:4,
                  VAR_1 = c(99,2,98,4), 
                  VAR_2 = c(1,99,3,4),
                  VAR_3 = c(1,98,3,99), 
                  VAR_STRG = c("eins", "zwei", "Missing", "vier"))
dat.orig <- dat
dat[dat == 99 | dat == 98 | dat == "Missing"]   <- NA
cbind(dat, dat.orig)

dat <- data.frame(ID = 1:4,
                  VAR_1 = c(0,2,0,""), 
                  VAR_2 = c(0,"",3,0))
dat.orig <- dat
dat[grepl("^0*$",dat$VAR_1) == TRUE & 
    grepl("^0*$", dat$VAR_2) == TRUE, 2:3] <- NA
cbind(dat, dat.orig)


##############
# 4.14 - S. 81
##############

options()

##############
# 4.14.2 - S. 84 ff.
##############

options("OutDec" = ",")

