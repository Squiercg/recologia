###http://recologia.com.br/2018/05/rarefacao-e-o-numero-total-de-especies/
##
library(vegan)
data(BCI)
bci <- BCI[seq(5, 50, by = 5), ]

##
N <- colSums(bci)
subs3 <- c(seq(500, 4500, by = 500), sum(N))
rar3 <- rarefy(N, sample = subs3, se = T, MARG = 2)

##
plot(subs3, rar3[1, ], ylab = "Riqueza de espécies",axes = FALSE,
     xlab = "Número de indivíduos",type = "n",
     ylim = c(0, 250), xlim = c(500,7000))
axis(1, at = 1:5 * 1000)
axis(2)
text(2500, 200, "Rarefação baseada em indivídus (10 plots)")

lines(subs3, rar3[1, ], type = "b")
lines(subs3, rar3[1, ] + 2 * rar3[2, ], lty = 3)
lines(subs3, rar3[1, ] - 2 * rar3[2, ], lty = 3)


##
ace <- estimateR(N)
segments(6000, ace["S.ACE"] - 2 * ace["se.ACE"],6000, ace["S.ACE"] + 2 * ace["se.ACE"], lwd = 3)
text(6000, 150, "Estimativa ACE", srt = 90, adj = c(1,0.5))

##
chaoF <- specpool(bci)
segments(6300, chaoF[1, "chao"] - 2 * chaoF[1,"chao.se"],
         6300, chaoF[1, "chao"] + 2 * chaoF[1,"chao.se"],
         lwd = 3, col = "grey")
text(6300, 150, "Estimativa Chao2", srt = 90, adj = c(1,0.5))

##
points(6700, dim(BCI)[2], pch = 19, cex = 1.5)
text(6700, 150, "Riqueza observada em 50 hectares",srt = 90, adj = c(1, 0.5))
