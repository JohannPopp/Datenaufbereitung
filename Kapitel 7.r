# Schendera 2007 Datenqualit‰t

##############################

# Kapitel 7: Ausreiﬂer

##############################

#######
# 7.2.1 - S. 171
#######

dat <- data.frame(matrix(c(1,0,5,3,4,5,6,7,3,4,5,6,7,3,4,5,6,7,3,4,5,
                           2,5,6,7,3,4,5,6,7,3,4,5,7,3,4,5,6,8,8,9,9,
                           3,5,3,1,6,4,2,8,0,4,5,3,2,4,3,5,6,7,8,9,1,
                           4,1,2,3,4,5,6,7,8,9,0,4,3,5,6,7,8,9,0,1,4),
                         nrow = 4, byrow = TRUE))
colnames(dat) <- c("ID", paste("var", 1:20, sep = ""))

AUSREIS1 <- apply(dat[,-1], 1, 
                  function(x) length(x[x==0 | x==1 | x==8 | x==9]))
AUSREIS2 <- apply(dat[,-1], 1, function(x) length(x[x<2 | x>7]))
cbind(dat$ID, AUSREIS1, AUSREIS2)


##########
# 7.2.4 - S. 176  ff.
############

dat <- data.frame(SEE = c("Bodensee", "M¸ritz", "Chimsee", "Schweriner See",
                          "Starnberger See", "Ammersee", "Plauer See", 
                          "Kummerower See", "Steinhuder Meer", 
                          "Groﬂer Plˆner See", "Schaalsee", "Selenter See",
                          "Kˆlpinsee"),
                  QKM = c(571.5, 109.2, 79.9, 61.5, 56.4, 46.6, 38.4, 32.5, 
                          29.1, 30, 22.8, 22.4, 20.3)) 

boxplot(dat$QKM)
# Mit Beschriftung
text(1.02, 
     boxplot(dat$QKM)$out, 
             labels = dat$SEE[dat$QKM == 
                                 boxplot(dat$QKM, 
                                         ylab = expression(Km^2), 
                                         xlab = "Nat¸rliche Seen")$out], 
     adj = c(0, 0.5))
# Einfache Alternative
plot(dat$SEE, dat$QKM, las = 2) 
plot(dat$QKM)

#Histogramm
hist(dat$QKM) 
# mit Normalverteilungskurve
hist(dat$QKM, freq = FALSE)
lines(seq(min(dat$QKM), max(dat$QKM), length.out = 1000), 
      dnorm(seq(min(dat$QKM), max(dat$QKM), length.out = 1000), 
             mean(dat$QKM), sd(dat$QKM))       
        * length(dat$QKM) * max(diff(hist(dat$QKM)$breaks)))

# Bevˆlkerungspyramide
Erziehungsberatung <- abs(rnorm(200,20,10)^3)/30
Kath.Tr‰ger <- sample(c("nein", "nein", "nein","ja"),200,replace = TRUE)

library(Hmisc)
out <- histbackback(split(Erziehungsberatung, Kath.Tr‰ger), las = 3)

barplot(-out$left, col = "red", horiz = TRUE, add = TRUE, 
        space = 0, axes = FALSE)
barplot(out$right, col = "blue", horiz = TRUE, add = TRUE, 
        space = 0, axes = FALSE)


# Fehlerbalken
errbar(1, mean(dat$QKM), 
       yplus = mean(dat$QKM) + sd(dat$QKM), 
       yminus = mean(dat$QKM) - sd(dat$QKM),
       xlab = "Nat¸rliche Seen",
       ylab = expression(Km^2))

# Eindimensionales Streudiagramm
plot(dat$QKM, rep(1, length(dat$QKM)))

plot(rep(1, length(dat$QKM)), dat$QKM)
text(1.02, dat$QKM, labels = dat$SEE, adj = c(0,0.5))

# St‰ngel-Blatt-Diagramm
stem(dat$QKM)
stem(rnorm(100, 20, 5))


#############
# 7.3.1 - S. 183 ff.
#############

model <- lm(circumference ~ age, data = Orange)
par(mfrow = c(2,2))
plot(model)

# Residuen
resid(model)
rstandard(model)
rstudent(model)

# Hebelwerte 
## Leverage wie bei Cohen (2003), S. 394
lev <- function(x){
  1/length(x[!is.na(x)]) + 
  (x - mean(x, na.rm = TRUE))^2 / 
  (sum((x - mean(x, na.rm = TRUE))^2, na.rm = TRUE))}
lev(model$model$age)

## Hat values
hatvalues(model)

# Einflussstatistik
influence.measures(model)


###########
# 7.3.3.1 - S. 189
###########

dat <- data.frame(WEEK = rep(1:280, each = 7),
                  crestpr = seq(2.1, 1.8, length.out = 280*7) + 
                            rnorm(280*7, 0, 0.1))
dat$crestpr[1850] <- 25
plot(tapply(dat$crestpr, dat$WEEK, mean), type = "l")


#######
# 7.3.3.2 - S. 191
########

library(qcc)
data(pistonrings)
diameter <- qcc.groups(pistonrings$diameter, pistonrings$sample)

qcc(diameter[1:25,], type="xbar")
qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,])

#########
# 7.4.3 - S. 193 ff.
#########

dat <- data.frame(Thrombo = trunc(rnorm(100, 170,60)),
                  Leuko = round((rgamma(100, 2) + 0.5) * 3, 2),
                  pO2 = trunc(rnorm(100, 86, 5)),
                  Sex = sample(c("m‰nnlich", "weiblich"), 100, replace = TRUE))
summary(dat)
plot(dat$Leuko, dat$Thrombo, main = "Bivariate Exploration")

# 3D
library(lattice)
cloud(Thrombo ~ pO2 * Leuko, data = dat)

library(car)
scatter3d(Thrombo ~ pO2 * Leuko, data = dat) # frei drehbar

library(rggobi)

# 2D mit dichotomer Variable
plot(dat$Leuko, dat$Sex)