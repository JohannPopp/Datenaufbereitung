# Schendera 2007 Datenqualität

##############################

# Kapitel 5: Doppelte Werte und mehrfache Datenzeilen

##############################

#######
# 5.2.1 - S. 96
#######

dat <- data.frame(ID = c(1, 2, 3, 4, 2, 3, 5),
                  GRUPPE = c("a", "a", "b", "b", "a", "b", "a"),
                  ALTER = c(8, 17, 23, 75, 17, 23, 65))
table(dat$ID)
barplot(table(dat$ID))


#########
# 5.2.2 - S. 97
#########

dat <- data.frame(ID = c(1, 2, 3, 4, 1, 2, 3, 3, 4),
                  GRUPPE = c(rep("a", 4), rep("b", 5)),
                  ALTER = c(8, 17, 23, 74, 75, 17, 23, 65, 46))
table(dat$ID, dat$GRUPPE)
barplot(table(dat$ID, dat$GRUPPE), beside = TRUE)

########
# 5.3 - S. 98 ff.
########

dat <- data.frame(ID   = c(1, 2, 3, 4, 4, 5),
                  VAR1 = c(2, 3, 4, 5, 6, 6),
                  VAR2 = c(3, 4, 5, 6, 7, 7),
                  VAR3 = c(4, 5, 6, 7, 8, 8),
                  VAR4 = c(5, 6, 7, 8, 9, 9))
# den ersten doppelten behalten
dat[duplicated(dat$ID) == FALSE,]
dat[!duplicated(dat$ID),]

# den letzten doppelten behalten
dat[!duplicated(dat$ID, fromLast = TRUE),]

# alle nicht einmaligen ID's verwerfen
dat[dat$ID != names(table(dat$ID))[table(dat$ID) > 1],]


##########
# 5.4 - S. 102 f.
##########

dat <- data.frame(ID   = c(1, 2, 3, 4, 4, 5),
                  VAR1 = c(2, 3, 4, 5, 6, 6),
                  VAR2 = c(3, 4, 5, 6, 7, 7),
                  VAR3 = c(4, 5, 6, 7, 8, 8),
                  VAR4 = c(5, 6, 7, 8, 9, 9))
dat[!duplicated(dat[1:4]),]
# oder
dat[!duplicated(cbind(dat$ID, dat$VAR1, dat$VAR2, dat$VAR3)),]



dat[dat$ID == names(table(dat$ID))[table(dat$ID) > 1],]


##########
# 5.5 - S. 104 ff.
##########

dat <- data.frame(ID       = c(1, 2, 3, 4, 2, 3, 5, 2, 3, 5),
                  GRUPPE   = c("a","a","b","b","a","b","a","a","b","a"),
                  ALTER    = c(8, 17, 23, 75, 17, 23, 65, 17, 23, 65),
                  VARNUM1  = c(1, 0, 1, 0, 0, 1, 1, 0, 1, 1),
                  VARNUM2  = c(0, 1, 1, 1, 1, 1, 0, 1, 1, 0),
                  VARNUM3  = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                  VARCHAR1 = c("b","a","a","b","a","a","c","a","a","c"),
                  VARCHAR2 = c("c","b","b","a","b","b","b","b","b","b"),
                  VARCHAR3 = c("a","c","c","c","c","c","a","c","c","a"))
dat[!duplicated(dat),]
# oder noch einfacher
unique(dat)

(DOPPELT <- table(paste(data.frame(t(dat))), dat$ID))

dat$HÄUFIGKEIT <- table(paste(data.frame(t(dat))))[paste(data.frame(t(dat)))]
           
table(dat$HÄUFIGKEIT == 1)
table(duplicated(dat))
table(apply(DOPPELT, 1, sum))
# oder
table(DOPPELT, exclude = 0)

table(dat$HÄUFIGKEIT)


##########
# 5.6 S. 111 ff.
##########

dat <- data.frame(ID       = c(1, 2, 3, 4, 2, 3, 5),
                  GRUPPE   = c("a","a","b","b","a","b","a"),
                  ALTER    = c(8, 17, 23, 75, 17, 23, 65),
                  VARNUM1  = c(1, 0, 1, 0, 0, 1, 1),
                  VARNUM2  = c(0, 1, 1, 1, 1, 1, 0),
                  VARNUM3  = c(1, 0, 0, 0, 0, 0, 0),
                  VARCHAR1 = c("b","a","a","b","a","a","c"),
                  VARCHAR2 = c("c","b","b","a","b","b","b"),
                  VARCHAR3 = c("a","c","c","c","c","c","a"))

table(duplicated(dat))
dat[!duplicated(dat),]
dat[duplicated(dat),]

#########
# 5.7 - S. 113 ff.
##########

dat <- data.frame(PATIENT = c(1,1,1,1,2,2,2,3,3,3), 
                  RECORD  = c("A","B","B","C","A","B","C","A","B","C"),
                  DATA    = c(127,58,58,59,98,67,75,58,59,43))

data.frame(ID    = dat$PATIENT,
           IQ    = dat$DATA[dat$RECORD == "A"],
           ALTER = dat$DATA[dat$RECORD == "B"],
           SCL90 = dat$DATA[dat$RECORD == "C"])

data.frame(ID    = unique(dat$PATIENT),
           IQ    = unique(dat)$DATA[unique(dat)$RECORD == "A"],
           ALTER = unique(dat)$DATA[unique(dat)$RECORD == "B"],
           SCL90 = unique(dat)$DATA[unique(dat)$RECORD == "C"])


#########
# 5.8 - S. 115 ff.
#########

library(RODBC)
odbcConnect(file.choose())
