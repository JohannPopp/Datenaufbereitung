# R-Kode f�r Schendera 2007. Datenqualit�t mit SPSS
##################################

# 3. Kapitel - Vollst�ndigkeit

#################################

# S. 32

# Anzahl der der Missings in jeder Zeile des Datensatzes berchnen und in row.missing ablegen
missings.row <- apply(datensatz, 1, function(x) sum(is.na(x)))

## Bei Schenderas Ansatz mit COUNT kann man es zwar durch die Formulierung "erste_Variable to letzte_Variable" vermeiden, alle Variablen einzeln anzugeben. COUNT funktioniert aber nicht, wenn der Datensatz sowohl numerische Variablen als auch Stringvariablen enth�lt. Bei dem Ansatz mit COMPUTE muss jede Variable einzeln angegeben werden. Das wird bei gro�en Datens�tzen sehr M�hsam.

# Z�hlt die Anzahl der Zellen pro Zeile, die eine 99 enthalten.
mis.row <- apply(datensatz, 1, function(x) sum(x == 99, na.rm = TRUE))


# S. 33 f.
# Datensatz erzeugen
datensatz <- data.frame(ID = c(111, 111, 111, 111, 222, 222, 333, 333, 333),
                        PLZ = c(NA, 20245, NA, NA, 60598, NA, 81669, NA, NA), 
                        ORT = c("Hamburg", "Hamburg", NA, "Hamburg", "Frankfurt", "Frankfurt", "M�nchen", "M�nchen", "M�nchen"), 
                        PRODNR = c(541, 655, NA, 652, 3412, 3221, 65464, 64623, 65435))

# Auff�llen - weicht etwas von der Logik des SPSS-Files ab, hat aber das gleiche Ergebnis
first <- rep(0, nrow(datensatz))                              # Markevariable, um den ersten Fall der jeweiligen ID zu markieren
dat <- cbind(datensatz[order(datensatz$ID,-datensatz$PLZ),], first)               # Sortieren nach ID(aufw�rts) und PLZ(abw�rts) und variable "first" hinzuf�gen
dat[!duplicated(dat$ID), 5] <- 1                                  # first auf 1 setzten f�r den ersten der jeweiligen ID
dat$PLZ <- rep(dat[which(dat$first == 1), 2], table(dat$ID))  # PLZ des jeweils ersten auf die anderen mit gleicher ID �bertragen
dat                                                         # Ergebnis ansehen


# Objekte wieder l�schen
rm(missings.row, mis.row, datensatz, first, dat)