# Schendera 2007 Datenqualität

##############################

# Kapitel 6: Missings

##############################

#########
# 6.1 - S. 129 f.
#########

dat <- data.frame(wert1 = c(1,1,1,1,1), miss1 = c(1,NA,NA,NA,1),
                  wert2 = c(1,1,1,1,1), miss2 = c(1,1,NA,1,1))
                  
apply(dat,1 , sum)
apply(dat,1 , sum)/4

apply(dat, 1, function(x) sum(x, na.rm = TRUE))
apply(dat, 1, function(x) sum(x, na.rm = TRUE)) / 4
apply(dat, 1, function(x) sum(x, na.rm = TRUE) / length(x))

apply(dat, 1, function(x) sum(x, na.rm = TRUE) / sum(table(x)))

apply(dat, 1, mean)
apply(dat, 1, function(x) mean(x, na.rm = TRUE))


############
# 6.3.2 - S. 138 ff.
############

dat <- data.frame(wert1 = c(1,1,1,1,NA,NA,1), miss1 = c(1,NA,NA,NA,NA,NA,1),
                  wert2 = c(1,1,1,1,NA,NA,1), miss2 = c(1,1,NA,1,NA,NA,1))

N_MISS <- apply(dat, 1, function(x) sum(is.na(x)))
PROZENT <- N_MISS / apply(dat, 1, length) * 100
cbind(N_MISS, PROZENT)

dat[PROZENT < 100,]

dat[apply(dat, 1, function(x) sum(is.na(x))/length(x)) < 1,]


##
plot(PROZENT)

library(mice)
md.pattern(dat)

image(1:ncol(dat), 1:nrow(dat), is.na(t(dat))== FALSE)

popp.miss.plot <- function(x, gridlines = TRUE, Farbe = "darkviolet", 
                           Name = deparse(substitute(x)), 
                           varlab = names(x), ...){
  image(1:ncol(x), 1:nrow(x), is.na(t(x)),
        col = c("grey80", Farbe),
        xaxt = "n", xlab = "", ylab = "Fallnummer", 
        main = paste("Missings im Datensatz\n", Name))
  if(ncol(x) > 200) gridlines <- FALSE
  if(gridlines == TRUE) segments((1:ncol(x)-1)+0.5, 0, 
                                 (1:ncol(x)-1)+ 0.5, 
                                 nrow(x)+1, col = "grey90")
  ifelse(ncol(x) > 60, lascode <- 1, lascode <- 2)
  axis(1, at = 1:ncol(x), labels = varlab, 
       las = lascode, cex.axis = 0.7, tick = gridlines)
}


#########
# 6.4.1 - S. 142
#########

dat <- data.frame(VAR1 = c(NA,5,7), VAR2 = c(11,NA,15),
                  VAR3 = c(65,67,NA), VAR4 = c(NA,92,94))

dat[is.na(dat)]  <- 3
dat


###########
# 6.4.2 - S. 143 ff.
###########

dat <- data.frame(REFERENZ = c(21,31,41,21,23,12,11),
                  MIT_MISS = c( 2,NA, 3,NA, 2, 1,NA))
                  
OHNE_MISS <- dat$MIT_MISS
OHNE_MISS[is.na(OHNE_MISS)] <- sample(1:3, length(OHNE_MISS[is.na(OHNE_MISS)]), replace = TRUE)
data.frame(dat, OHNE_MISS)

OHNE_MISS <- dat$MIT_MISS
ZUFALL <- rep(NA, length(OHNE_MISS))
ZUFALL[is.na(OHNE_MISS)] <- runif(length(OHNE_MISS[is.na(OHNE_MISS)]))
OHNE_MISS[ZUFALL <= 0.333] <- 1
OHNE_MISS[ZUFALL > 0.333 & ZUFALL <= 0.666] <- 2
OHNE_MISS[ZUFALL > 0.666] <- 3
cbind(dat, OHNE_MISS, ZUFALL)

OHNE_MISS <- dat$MIT_MISS
OHNE_MISS[is.na(OHNE_MISS)] <- sample(1:100,
                                length(OHNE_MISS[is.na(OHNE_MISS)]),
                                 replace = TRUE)
                                 

dat <- data.frame(ITEM1 = c(1,1,1,4,4,4,1), ITEM2 = c(2,2,2,1,1,1,2),
                  ITEM3 = c(3,4,3,2,2,3,3), ITEM4 = c(5,5,5,NA,3,3,NA))

dat$VORHER <- dat$ITEM4
dat <- dat[order(dat$ITEM1, dat$ITEM2, dat$ITEM3),]
INDEX <- 1:length(dat$ITEM4)
dat$ITEM4[is.na(dat$ITEM4)] <- 
  apply(cbind(dat$ITEM4[INDEX[is.na(dat$ITEM4)]-1], 
              dat$ITEM4[INDEX[is.na(dat$ITEM4)]+1]), 
        1, median)
dat


#######
# 6.4.3 S. 146 ff.
#######

dat <- data.frame(ID = 1:5, SEX = c("M", NA, "M", NA, "W"), 
                  SCHWANGER = c("NEIN", "JA", "NEIN", "JA", "NEIN"))
dat$SEX[dat$SCHWANGER == "JA"] <- "W"
dat


OHNE_MISS <- c(21, 31, 41, 21, 23, 12, 11)
MIT_MISS  <- c("21-40", NA, "41-60", NA, "21-40", "0-20", NA)

ORIGINAL  <- MIT_MISS
MIT_MISS[is.na(MIT_MISS) & OHNE_MISS >  0 & OHNE_MISS <= 20]  <- "0-20"
MIT_MISS[is.na(MIT_MISS) & OHNE_MISS > 20 & OHNE_MISS <= 40]  <- "21-40"
MIT_MISS[is.na(MIT_MISS) & OHNE_MISS > 40 & OHNE_MISS <= 60]  <- "41-60"
data.frame(OHNE_MISS, ORIGINAL, MIT_MISS)


#########
# 6.4.5 - S. 149 f.
#########

dat <- data.frame(ID = 1:5, ALTER = c(5, 10, NA, 60, 75))
dat$MEANALT <- dat$ALTER
dat$MEANALT[is.na(dat$ALTER)] <- mean(dat$ALTER, na.rm = TRUE)
dat$MEANLIN <- approx(dat$ALTER, n = length(dat$ALTER))$y
dat$MEANMED <- dat$ALTER
dat$MEANMED[is.na(dat$ALTER)] <- median(dat$ALTER, na.rm = TRUE)
dat


###########
# 6.4.6 - S. 152 ff.
###########

dat <- data.frame(ITEM1 = c(1,1,1,4,4,4,1), ITEM2 = c(2,2,2,1,1,1,2), 
                  ITEM3 = c(3,3,3,2,2,2,3), ITEM4 = c(5,5,5,NA,3,3,NA))
dat <- dat[order(dat$ITEM1,dat$ITEM2, dat$ITEM3, dat$ITEM4),]
INDEX <- 1:length(dat$ITEM4)
dat$ITEM4[duplicated(dat[,1:3]) & is.na(dat$ITEM4)] <- 
  dat$ITEM4[INDEX[duplicated(dat[,1:3]) & is.na(dat$ITEM4)]-1]
dat


# SPSS-Datensatz importieren
library(foreign)
dat <- data.frame(read.spss("C:\\Programme\\SPSSInc\\Statistics17\\Samples\\German\\Employee data.sav"))
str(dat)

dat2 <- data.frame(respnr = dat$id, stratum = dat$ausbild, x = dat$gehalt)
dat2$x[1:40] <- NA
with(dat2, cbind(N.gültig = length(x[!is.na(x)]), Fehlend = length(x[is.na(x)]),
                 Min = min(x, na.rm = TRUE), 
                 Max = max(x, na.rm = TRUE), Mean = mean(x, na.rm = TRUE),
                 SD = sd(x, na.rm = TRUE)))

#Daten sortieren und die erste und zweite Zeile jedes Stratums mit 1 bzw. 2 inder Variable "seqstr" markieren.                 
dat2$xnew <- as.numeric(is.na(dat2$x))
dat2$xmis <- as.numeric(is.na(dat2$x))
dat2$seqnbeg <- 1:length(dat[,1])
dat2 <- dat2[order(dat2$stratum, is.na(dat2$x), dat2$seqnbeg),]
strat <- c(NA, dat2$stratum[1:(length(dat2$stratum)-1)])
dat2$seqnstr[dat2$stratum != strat] <- 1
dat2$seqnstr[1] <- 1
volg <- c(NA, dat2$seqnstr[1:(length(dat2$seqnstr)-1)])
dat2$seqnstr[is.na(dat2$seqnstr)] <- volg[is.na(dat2$seqnstr)] +1

# Das Gleiche noch einmal mit nur 3 Zeilen R-Kode
dat2 <- dat2[order(dat2$stratum, is.na(dat2$x), row.names(dat2)),]
dat2$sq.st[!duplicated(dat2$stratum)] <- 1
dat2$sq.st[!is.na(c(NA, dat2$sq.st[1:(length(dat2$x)-1)]))] <- 2


with(dat2, cbind(Missing = table(stratum, is.na(x))[,2], N = table(stratum)))
dat2$stratn <- rep(table(dat2$stratum), table(dat2$stratum))
dat2$stratm <- rep(table(dat2$stratum, is.na(dat2$x))[,2], table(dat2$stratum)) 

# 4 Zufalls-Indexnummern in seqnstr erzeugen, die den jeweiligen Strata entsprechen 
dat2$seqnstr[is.na(dat2$x)] <- 
  with(dat2, 
       trunc(1 + runif(length(x), 0, stratn-stratm))[is.na(x)])

# oder in einem Befehl, ohne stratn und stratm erzeugen zu  müssen.
dat2$sq.st[is.na(dat2$x)] <- 
  with(dat2, 
       trunc(1 + runif(length(x), 0, 
                 rep(table(stratum, is.na(x))[,1], table(stratum))
                 ))[is.na(x)])
with(dat2, cbind(stratn-stratm, x, seqnstr, sq.st)[is.na(x),])

# Wahrscheinlich war das gemeint: Für fehlende x wird ein Zufällig ausgewählter Wert der vorhandenen x im entsprechenden Stratum eingesetzt.
dat2$xnew <- dat2$x
dat2$xnew[is.na(dat2$x)] <- with(dat2, x[seqnstr][is.na(x)])

# Zurück in die urspründliche Reihenfolge
dat2[order(as.numeric(row.names(dat2))),]


########
# 6.4.7.1 - S. 156
########

dat <- data.frame(NUM1 = c(1,1,0,0,1,0,0,1,0), NUM2 = c(1,1,0,0,1,0,1,1,0),
                  NUM3 = c(1,1,0,1,1,1,0,1,0), NUM4 = c(1,0,1,0,NA,NA,0,1,0))
library(epicalc)
alpha(c(NUM1,NUM2, NUM3, NUM4), dataFrame = dat)
VORHER <- dat$NUM4
dat$NUM4[is.na(dat$NUM4)] <- round(apply(dat[is.na(dat$NUM4),1:3], 1, mean), 0)
SKALA1 <- apply(dat, 1, sum)
SKALA2 <- apply(cbind(dat[,1:3], VORHER), 1, function(x) sum(x, na.rm = TRUE))
cbind(SKALA1, SKALA2)


###########
# 6.4.7.2 - S.158
###########

dat <- data.frame(ITEM1 = c(1,1,1,4,4,4,1), ITEM2 = c(2,2,2,1,1,1,2), 
                  ITEM3 = c(3,3,3,2,2,2,3), ITEM4 = c(5,5,5,NA,3,3,NA))
library(mice)
imp <- mice(dat)        # Grundsätzlich funktioniert das. Mit dem Beispieldatensatz aber leider nicht. Multikolinearität?
dat.neu <- complete(imp)