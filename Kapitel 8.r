# Schendera 2007 Datenqualität

##############################

# Kapitel 8: Plausibilität

##############################

########
# 8.2.2.1 - S. 214 f.
########

dat <- data.frame(LK.unt = rbeta(500, 1, 5)*60,
                  LK.pos = rnorm(500, 25, 7),
                  Menopause = sample(c("prä-menopausal",
                                       "post-menopausal",
                                       "unbekannt"),
                                     500, replace = TRUE))
dat$LK.unt[239] <- 55
dat$LK.pos[c(331, 492, 90, 282)] <- c(127, 50, 49, 47)
levels(dat$Menopause) <- levels(dat$Menopause)[c(2,1,3)]
dat$Menopause[c(331, 492)]  <- "prä-menopausal"
dat$Menopause[c(90, 282)] <- "post-menopausal"


plot(dat$LK.pos, dat$LK.unt, col = "grey", pch = 20,
     ylab = "Anzahl untersuchter LK", xlab = "Anzahl positiver LK")
text(dat$LK.pos, dat$LK.unt, pos = 4, cex = 0.7, xpd = TRUE)



boxplot(dat$LK.pos ~ dat$Menopause, ylab = "Anzahl positiver Lymphknoten")
text(dat$Menopause, dat$LK.pos, cex = 0.7, pos = 4)

# Etwas komplizierter, wenn man nur die Extremwerte beschriften möchte
b <- boxplot(dat$LK.pos ~ dat$Menopause, ylab = "Anzahl positiver Lyphknoten")
text(b$group, b$out, 
     labels = sapply(b$out, function(x) which(x == dat$LK.pos)),
     pos = 4, cex = 0.7)                            