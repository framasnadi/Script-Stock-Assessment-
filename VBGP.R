
## VBGP solea Solemon 2014-2017

setwd("C:/Users/francesco/Desktop/Stock Assessment/SS3/Corso Ponza 2019/SOLE/dati")
library(readxl)
dati_Solea_xVBGP <- read_excel("C:/Users/francesco/Desktop/Stock Assessment/SS3/Corso Ponza 2019/SOLE/dati/dati_Solea_xVBGP.xlsx")
View(dati_Solea_xVBGP)
dati<-dati_Solea_xVBGP

dati$age<-dati$Age + 0.5
dati$agee<-dati$Age + 0.85
par(mfrow=c(1,1))

#install.packages("FSA")
library(FSA)
##?vbFuns
vb <- vbFuns("typical")
plot(TL ~ agee, data=subset(dati,Sex=="m"),ylim=c(0,50),xlim=c(0,15),col=4)
points(TL~ age, data=subset(dati,Sex=="f"),col=2)
#abline(h=41,lty=2)

#fm1 <- nls(leng ~ Linf*(1-exp(-K*(age-t0))), start=list(Linf=37,K=0.2,t0=-1),
 #          data=subset(nemo,sex=="M"),lower=c(33,0.05,-3),
  #         upper=c(41,0.35,0),algorithm="port")

#fm2 <- nls(leng ~ Linf*(1-exp(-K*(age-t0))), start=list(Linf=37,K=0.2,t0=-1),
   #        data=subset(nemo,sex=="F"),lower=c(33,0.05,-3),
    #       upper=c(41,0.35,0),algorithm="port")

fm3<-nls(TL ~ Linf*(1-exp(-K*(age-t0))), start=list(Linf=31,K=0.3,t0=-0.6),
       data=dati,lower=c(26,0.05,-3),
       upper=c(41,1,0),algorithm="port")

lines(0:15,vb(0:15,as.list(coef(fm3))),lwd=2,col=2)
#lines(0:20,vb(0:20,as.list(coef(fm1))),lwd=2,col=2)
#lines(0:20,vb(0:20,as.list(coef(fm2))),lwd=2,col=4)
legend("bottom",c("Age+0.5","Age+0.85"),col=c(2,4),lty=c(1,1))

summary(fm3)

# Alternatively, use you may use vbStarts(), but in this example
# it does not work well, as it requres more settings
svTypicalM <- vbStarts(leng~age,data=subset(nemo,sex=="M"))
svTypicalF <- vbStarts(leng~age,data=subset(nemo,sex=="F"))
lines(0:20,vb(0:20,svTypicalM),lwd=2,col=3) 
lines(0:20,vb(0:20,svTypicalF),lwd=2,col=3) 