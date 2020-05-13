###################################
#   Paper VBGP solea Solemon      #
#######################################################
# Developed by Francesco Masnadi (CNR-IRBIM, Ancona)  #
#######################################################

rm(list=ls())  #clean the console

### The working directory must be the folder were the code and input data are stored. Please insert here the working directory, or press ctr+shift+h and navigate to the right folder
setwd("C:/Users/f.masnadi/Desktop/Stock Assessment/SS3/Corso Ponza 2019/SOLE/dati/VBGP")

# Age shift in data assuming all births on 1st January
age_shift <- 0.5  


# Install and load missing packages
list.of.packages <- c("FSA", "readxl","dplyr","stargazer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(FSA);library(readxl);library(dplyr);library(stargazer)
# create plot folder (automatically)
dir.create(file.path(".", paste("Plots & Tables_",age_shift, sep = ""  )))
dir.create(file.path(".", paste("Plots & Tables_",age_shift, "/Mat_Ogive_Length" ,sep = ""  )))
dir.create(file.path(".", paste("Plots & Tables_",age_shift, "/Mat_Ogive_Age" ,sep = ""  )))
dir.create(file.path(".", paste("Plots & Tables_",age_shift, "/LW_Relation" ,sep = ""  )))
plotdir <- (paste0(".", "/" , "Plots & Tables_",age_shift,"/"))
plotdirOg <- (paste0(".", "/" , "Plots & Tables_",age_shift,"/","Mat_Ogive_Length/"))
plotdirOgA <- (paste0(".", "/" , "Plots & Tables_",age_shift,"/","Mat_Ogive_Age/"))
plotdirLW <- (paste0(".", "/" , "Plots & Tables_",age_shift,"/","LW_Relation/"))
# Import data
dati <- read_excel("./datiSolea_14_18.xlsx") %>% dplyr::filter(Whole != 99 & Age_sect != 99);summary(dati)
#View(dati)
dati$TL <- dati$`TL(cm)`
dati$age<-dati$Age_sect + age_shift      # aggiungo 0.85 perchè solemon è fatto a fine anno
dati$age_w<-dati$Whole + age_shift       # aggiungo 0.85 perchè solemon è fatto a fine anno
dati$sez<-dati$sezioni + age_shift       # aggiungo 0.85 perchè solemon è fatto a fine anno
#dati %>% dplyr::filter(age  == 14.85 )

########################################
##    VBGP by Sex and Year and aggregate
########################################
vb <- vbFuns("typical")
for(i in unique(dati$anno)) {
mm<-nls(TL ~ Linf*(1-exp(-K*(age-t0))), start=list(Linf=35,K=0.5,t0=-0.46),
         data=(dati %>% dplyr::filter(anno==i) %>% dplyr::filter(Sex=="m")),lower=c(25,0.1,-3),
         upper=c(43,1,1),algorithm="port")
 fm <- nls(TL ~ Linf*(1-exp(-K*(age-t0))), start=list(Linf=35,K=0.5,t0=-0.46),
           data=(dati %>% dplyr::filter(anno==i) %>% dplyr::filter(Sex=="f")),lower=c(25,0.1,-3),
           upper=c(43,1,1),algorithm="port")
 wmm<-nls(TL ~ Linf*(1-exp(-K*(age_w-t0))), start=list(Linf=35,K=0.5,t0=-0.46),
         data=(dati %>% dplyr::filter(anno==i) %>% dplyr::filter(Sex=="m")),lower=c(25,0.1,-3),
         upper=c(43,1,1),algorithm="port")
 wfm <- nls(TL ~ Linf*(1-exp(-K*(age_w-t0))), start=list(Linf=35,K=0.5,t0=-0.46),
           data=(dati %>% dplyr::filter(anno==i) %>% dplyr::filter(Sex=="f")),lower=c(25,0.1,-3),
           upper=c(43,1,1),algorithm="port")
 w <- nls(TL ~ Linf*(1-exp(-K*(age_w-t0))), start=list(Linf=35,K=0.5,t0=-0.46),
            data=(dati %>% dplyr::filter(anno==i)),lower=c(25,0.1,-3),
            upper=c(43,1,1),algorithm="port")
 s <- nls(TL ~ Linf*(1-exp(-K*(age-t0))), start=list(Linf=35,K=0.5,t0=-0.46),
           data=(dati %>% dplyr::filter(anno==i)),lower=c(25,0.1,-3),
           upper=c(43,1,1),algorithm="port")
print(mm)
print(fm)
print(wmm)
print(wfm)
print(w)
print(s)
Male_sect <-as.list(coef(mm))
Female_sect <-as.list(coef(fm))
Male_Whole<-as.list(coef(wmm))
Female_Whole <-as.list(coef(wfm))
Combined_Whole <-as.list(coef(w))
Combined_sect <-as.list(coef(s))
x<-cbind(Male_Whole,Female_Whole,Male_sect,Female_sect, Combined_Whole, Combined_sect)
write.csv2(x, paste0(plotdir,"VBGP_",i,".csv"))
par(mfrow=c(1,1))
tiff(paste0(plotdir,"VB curve",i,".tif"),width = 170, height = 85, units = "mm", res = 400, pointsize = 6)
par(mfrow=c(1,2))
# plot whole
plot(TL ~ age_w, data=(dati %>% dplyr::filter(anno==i)%>% dplyr::filter(Sex=="m")),ylim=c(0,max(dati$TL+10)),xlim=c(0,max(dati$Age_sect+1)),col=4, xlab = "Age", pch = 4)
points(TL~ age_w, data=(dati %>% dplyr::filter(anno==i)%>% dplyr::filter(Sex=="f")),col=2)
lines(0:max(dati$Age_sect+1),vb(0:max(dati$Age_sect+1),as.list(coef(wmm))),lwd=1.5,col=4,lty=2)
lines(0:max(dati$Age_sect+1),vb(0:max(dati$Age_sect+1),as.list(coef(wfm))),lwd=1.5,col=2,lty=2)
lines(0:max(dati$Age_sect+1),vb(0:max(dati$Age_sect+1),as.list(coef(w))),lwd=1.5,col=1,lty=2)
title(main= c(paste(i," VB Curve Whole")))
legend("bottom",c("Male", "Female", "Combined"),col=c(4,2,1),lty=c(2,2,2),lwd=c(1.5,1.5,1.5))
coeVBwm <- coef(wmm)
coeVBwf <- coef(wfm)
coeVBwag <- coef(w)
text(labels=paste("Male"),min(dati$Whole+1),max(dati$TL)+8,cex=1.1)
text(min(dati$Whole+3.7),max(dati$TL)+8,labels=paste("Linf = ",round(coeVBwm[1],digits=2),  sep=""),cex=1)
text(min(dati$Whole+6.5),max(dati$TL)+8,labels=paste("K = ",round(coeVBwm[2],digits=2),  sep=""),cex=1)
text(min(dati$Whole+9),max(dati$TL)+8,labels=paste("t0 = ",round(coeVBwm[3],digits=2),  sep=""),cex=1)
text(labels=paste("Female"),min(dati$Whole+1),max(dati$TL)+5,cex=1.1)
text(min(dati$Whole+3.7),max(dati$TL)+5,labels=paste("Linf = ",round(coeVBwf[1],digits=2),  sep=""),cex=1)
text(min(dati$Whole+6.5),max(dati$TL)+5,labels=paste("K = ",round(coeVBwf[2],digits=2),  sep=""),cex=1)
text(min(dati$Whole+9),max(dati$TL)+5,labels=paste("t0 = ",round(coeVBwf[3],digits=2),  sep=""),cex=1)
text(labels=paste("Comb."),min(dati$Whole+1),max(dati$TL)+2,cex=1.1)
text(min(dati$Whole+3.7),max(dati$TL)+2,labels=paste("Linf = ",round(coeVBwag[1],digits=2),  sep=""),cex=1)
text(min(dati$Whole+6.5),max(dati$TL)+2,labels=paste("K = ",round(coeVBwag[2],digits=2),  sep=""),cex=1)
text(min(dati$Whole+9),max(dati$TL)+2,labels=paste("t0 = ",round(coeVBwag[3],digits=2),  sep=""),cex=1)
# plot sect
plot(TL ~ age, data=(dati %>% dplyr::filter(anno==i)%>% dplyr::filter(Sex=="m")),ylim=c(0,max(dati$TL+10)),xlim=c(0,max(dati$Age_sect+1)),col=4, xlab = "Age" ,pch = 4)
points(TL~ age, data=(dati %>% dplyr::filter(anno==i)%>% dplyr::filter(Sex=="f")),col=2)
points(TL~ sez, data=(dati %>% dplyr::filter(anno==i)%>% dplyr::filter(Sex=="f")),col=2,lwd=3)
points(TL~ sez, data=(dati %>% dplyr::filter(anno==i)%>% dplyr::filter(Sex=="m")),col=4, lwd=3, pch = 4)
lines(0:max(dati$Age_sect+1),vb(0:max(dati$Age_sect+1),as.list(coef(mm))),lwd=1.5,col=4,lty=2)
lines(0:max(dati$Age_sect+1),vb(0:max(dati$Age_sect+1),as.list(coef(fm))),lwd=1.5,col=2,lty=2)
lines(0:max(dati$Age_sect+1),vb(0:max(dati$Age_sect+1),as.list(coef(s))),lwd=1.5,col=1,lty=2)
title(main= c(paste(i," VB Curve Section")))
legend("bottom",c("Male", "Female", "Combined"),col=c(4,2,1),lty=c(2,2,2),lwd=c(1.5,1.5,1.5))
coeVBm <- coef(mm)
coeVBf <- coef(fm)
coeVBag <- coef(s)
text(labels=paste("Male"),min(dati$Age_sect+1),max(dati$TL)+8,cex=1.1)
text(min(dati$Age_sect+3.7),max(dati$TL)+8,labels=paste("Linf = ",round(coeVBm[1],digits=2),  sep=""),cex=1)
text(min(dati$Age_sect+6.5),max(dati$TL)+8,labels=paste("K = ",round(coeVBm[2],digits=2),  sep=""),cex=1)
text(min(dati$Age_sect+9),max(dati$TL)+8,labels=paste("t0 = ",round(coeVBm[3],digits=2),  sep=""),cex=1)
text(labels=paste("Female"),min(dati$Age_sect+1),max(dati$TL)+5,cex=1.1)
text(min(dati$Age_sect+3.7),max(dati$TL)+5,labels=paste("Linf = ",round(coeVBf[1],digits=2),  sep=""),cex=1)
text(min(dati$Age_sect+6.5),max(dati$TL)+5,labels=paste("K = ",round(coeVBf[2],digits=2),  sep=""),cex=1)
text(min(dati$Age_sect+9),max(dati$TL)+5,labels=paste("t0 = ",round(coeVBf[3],digits=2),  sep=""),cex=1)
text(labels=paste("Comb."),min(dati$Age_sect+1),max(dati$TL)+2,cex=1.1)
text(min(dati$Age_sect+3.7),max(dati$TL)+2,labels=paste("Linf = ",round(coeVBag[1],digits=2),  sep=""),cex=1)
text(min(dati$Age_sect+6.5),max(dati$TL)+2,labels=paste("K = ",round(coeVBag[2],digits=2),  sep=""),cex=1)
text(min(dati$Age_sect+9),max(dati$TL)+2,labels=paste("t0 = ",round(coeVBag[3],digits=2),  sep=""),cex=1)
dev.off()
}

# all years aggregated
mma<-nls(TL ~ Linf*(1-exp(-K*(age-t0))), start=list(Linf=35,K=0.5,t0=-0.46),
        data=(dati  %>% dplyr::filter(Sex=="m")),lower=c(25,0.1,-3),
        upper=c(43,1,1),algorithm="port")
fma <- nls(TL ~ Linf*(1-exp(-K*(age-t0))), start=list(Linf=35,K=0.5,t0=-0.46),
          data=(dati  %>% dplyr::filter(Sex=="f")),lower=c(25,0.1,-3),
          upper=c(43,1,1),algorithm="port")
wmma<-nls(TL ~ Linf*(1-exp(-K*(age_w-t0))), start=list(Linf=35,K=0.5,t0=-0.46),
         data=(dati  %>% dplyr::filter(Sex=="m")),lower=c(25,0.1,-3),
         upper=c(43,1,1),algorithm="port")
wfma <- nls(TL ~ Linf*(1-exp(-K*(age_w-t0))), start=list(Linf=35,K=0.5,t0=-0.46),
           data=(dati  %>% dplyr::filter(Sex=="f")),lower=c(25,0.1,-3),
           upper=c(43,1,1),algorithm="port")
wA <- nls(TL ~ Linf*(1-exp(-K*(age_w-t0))), start=list(Linf=35,K=0.5,t0=-0.46),
            data=dati,lower=c(25,0.1,-3),
            upper=c(43,1,1),algorithm="port")
sA <- nls(TL ~ Linf*(1-exp(-K*(age-t0))), start=list(Linf=35,K=0.5,t0=-0.46),
           data=dati ,lower=c(25,0.1,-3),
           upper=c(43,1,1),algorithm="port")
print(mma)
print(fma)
print(wmma)
print(wfma)
print(wA)
print(sA)
Male_sect <-as.list(coef(mma))
Female_sect <-as.list(coef(fma))
Male_Whole<-as.list(coef(wmma))
Female_Whole <-as.list(coef(wfma))
Combined_Whole<-as.list(coef(wA))
Combined_sect <-as.list(coef(sA))
xa<-cbind(Male_Whole,Female_Whole,Male_sect,Female_sect, Combined_Whole, Combined_sect)
write.csv2(xa, paste0(plotdir,"VBGP_aggragate.csv"))
par(mfrow=c(1,1))
tiff(paste0(plotdir,"VB curve aggragate data.tif"),width = 170, height = 85, units = "mm", res = 400, pointsize = 6)
par(mfrow=c(1,2))
# plot whole
plot(TL ~ age_w, data=(dati %>% dplyr::filter(Sex=="m")),ylim=c(0,max(dati$TL+10)),xlim=c(0,max(dati$Age_sect+1)),col=4, xlab = "Age",pch = 4)
points(TL~ age_w, data=(dati %>% dplyr::filter(Sex=="f")),col=2)
lines(0:max(dati$Age_sect+1),vb(0:max(dati$Age_sect+1),as.list(coef(wmma))),lwd=1.5,col=4,lty=2)
lines(0:max(dati$Age_sect+1),vb(0:max(dati$Age_sect+1),as.list(coef(wfma))),lwd=1.5,col=2,lty=2)
lines(0:max(dati$Age_sect+1),vb(0:max(dati$Age_sect+1),as.list(coef(wA))),lwd=1.5,col=1,lty=2)
title(main= c(paste("VB Curve Whole aggragate data")))
legend("bottom",c("Male", "Female","Combined"),col=c(4,2,1),lty=c(2,2,2),lwd=c(1.5,1.5,1.5))
coeVBwm <- coef(wmma)
coeVBwf <- coef(wfma)
coeVBwag <- coef(wA)
text(labels=paste("Male"),min(dati$Whole+1),max(dati$TL)+8,cex=1.1)
text(min(dati$Whole+3.7),max(dati$TL)+8,labels=paste("Linf = ",round(coeVBwm[1],digits=2),  sep=""),cex=1)
text(min(dati$Whole+6.5),max(dati$TL)+8,labels=paste("K = ",round(coeVBwm[2],digits=2),  sep=""),cex=1)
text(min(dati$Whole+9),max(dati$TL)+8,labels=paste("t0 = ",round(coeVBwm[3],digits=2),  sep=""),cex=1)
text(labels=paste("Female"),min(dati$Whole+1),max(dati$TL)+5,cex=1.1)
text(min(dati$Whole+3.7),max(dati$TL)+5,labels=paste("Linf = ",round(coeVBwf[1],digits=2),  sep=""),cex=1)
text(min(dati$Whole+6.5),max(dati$TL)+5,labels=paste("K = ",round(coeVBwf[2],digits=2),  sep=""),cex=1)
text(min(dati$Whole+9),max(dati$TL)+5,labels=paste("t0 = ",round(coeVBwf[3],digits=2),  sep=""),cex=1)
text(labels=paste("Comb."),min(dati$Whole+1),max(dati$TL)+2,cex=1.1)
text(min(dati$Whole+3.7),max(dati$TL)+2,labels=paste("Linf = ",round(coeVBwag[1],digits=2),  sep=""),cex=1)
text(min(dati$Whole+6.5),max(dati$TL)+2,labels=paste("K = ",round(coeVBwag[2],digits=2),  sep=""),cex=1)
text(min(dati$Whole+9),max(dati$TL)+2,labels=paste("t0 = ",round(coeVBwag[3],digits=2),  sep=""),cex=1)
# plot sect
plot(TL ~ age, data=(dati %>% dplyr::filter(Sex=="m")),ylim=c(0,max(dati$TL+10)),xlim=c(0,max(dati$Age_sect+1)),col=4, xlab = "Age", pch = 4)
points(TL~ age, data=(dati %>% dplyr::filter(Sex=="f")),col=2)
points(TL~ sez, data=(dati %>% dplyr::filter(Sex=="f")%>% dplyr::filter(Whole != Age_sect)),col=2, lwd=3)
points(TL~ sez, data=(dati %>% dplyr::filter(Sex=="m")%>% dplyr::filter(Whole != Age_sect)),col=4,lwd=3, pch = 4)
lines(0:max(dati$Age_sect+1),vb(0:max(dati$Age_sect+1),as.list(coef(mma))),lwd=1.5,col=4,lty=2)
lines(0:max(dati$Age_sect+1),vb(0:max(dati$Age_sect+1),as.list(coef(fma))),lwd=1.5,col=2,lty=2)
lines(0:max(dati$Age_sect+1),vb(0:max(dati$Age_sect+1),as.list(coef(sA))),lwd=1.5,col=1,lty=2)
title(main= c(paste("VB Curve Section aggragate data")))
legend("bottom",c("Male", "Female", "Combined"),col=c(4,2,1),lty=c(2,2,2),lwd=c(1.5,1.5,1.5))
coeVBm <- coef(mma)
coeVBf <- coef(fma)
coeVBag <- coef(sA)
text(labels=paste("Male"),min(dati$Age_sect+1),max(dati$TL)+8,cex=1.1)
text(min(dati$Age_sect+3.7),max(dati$TL)+8,labels=paste("Linf = ",round(coeVBm[1],digits=2),  sep=""),cex=1)
text(min(dati$Age_sect+6.5),max(dati$TL)+8,labels=paste("K = ",round(coeVBm[2],digits=2),  sep=""),cex=1)
text(min(dati$Age_sect+9),max(dati$TL)+8,labels=paste("t0 = ",round(coeVBm[3],digits=2),  sep=""),cex=1)
text(labels=paste("Female"),min(dati$Age_sect+1),max(dati$TL)+5,cex=1.1)
text(min(dati$Age_sect+3.7),max(dati$TL)+5,labels=paste("Linf = ",round(coeVBf[1],digits=2),  sep=""),cex=1)
text(min(dati$Age_sect+6.5),max(dati$TL)+5,labels=paste("K = ",round(coeVBf[2],digits=2),  sep=""),cex=1)
text(min(dati$Age_sect+9),max(dati$TL)+5,labels=paste("t0 = ",round(coeVBf[3],digits=2),  sep=""),cex=1)
text(labels=paste("Comb."),min(dati$Age_sect+1),max(dati$TL)+2,cex=1.1)
text(min(dati$Age_sect+3.7),max(dati$TL)+2,labels=paste("Linf = ",round(coeVBag[1],digits=2),  sep=""),cex=1)
text(min(dati$Age_sect+6.5),max(dati$TL)+2,labels=paste("K = ",round(coeVBag[2],digits=2),  sep=""),cex=1)
text(min(dati$Age_sect+9),max(dati$TL)+2,labels=paste("t0 = ",round(coeVBag[3],digits=2),  sep=""),cex=1)
dev.off()




########################################
##    Length-weight relationship by Sex and Year
########################################
dati <- dati %>% mutate(Sex =  replace(Sex, Sex == "f", "F"));dati <- dati %>% mutate(Sex =  replace(Sex, Sex == "m", "M"))
dati <- dati %>% filter(Sex != "NA")

for(i in unique(dati$Sex)) { for (bb in unique(dati$anno)) {
  datLW <- dati %>% dplyr::filter(Sex==i)%>% dplyr::filter(anno==bb)       
mod <- lm(log(Weight) ~ log(`TL(cm)`), data = datLW)
#print(summary(mod))
R<-summary(mod)$adj.r.squared;pv<-format.pval(pf(summary(mod)$fstatistic[1L], summary(mod)$fstatistic[2L], summary(mod)$fstatistic[3L], lower.tail = F))
co <- coef(mod)
a <- exp(co[1])
b <- co[2]
# Test for b.  H0 : ?? = 3 ??? H0 : "Isometric growth" ; HA : ?? != 3 ??? HA : "Allometric growth"
print(hoCoef(mod,2,3));print(confint(mod))
write.csv(hoCoef(mod,2,3), paste0(plotdirLW,bb," Test_b ",i, ".csv"))
# plots
tiff(paste0(plotdirLW,bb," L-W rel ",i, ".tif"),width = 85, height = 85, units = "mm", res = 200, pointsize = 6)
par(mfrow = c(1,1))
#plot(log(Weight) ~ log(`TL(cm)`), data = datLW, ylab = "log(weight)",  xlab="log(length)")
#abline(co, col = 2, lwd = 2)
plot(x = datLW$`TL(cm)`, y = datLW$Weight, ylab = "Weight (gr)", xlab = "Length (cm)")
lines(c(0:max(datLW$`TL(cm)`+10)), exp(co[1])*c(0:max(datLW$`TL(cm)`+10))^co[2], col=2, lwd=2)
text(min(datLW$`TL(cm)`+3),max(datLW$Weight)-20,pos = 1,adj = c(0.5, NA),labels=paste("a = ",round(a,digits=4),   sep=""),cex=1.2)
text(min(datLW$`TL(cm)`+8.5) ,max(datLW$Weight)-20,pos = 1,adj = c(0.5, NA),labels=paste("b = ",round(b,digits=4),   sep=""),cex=1.2)
text(min(datLW$`TL(cm)`+3) ,max(datLW$Weight)-50,pos = 1,adj = c(0.5, NA),labels=paste("R² = ",round(R,digits=4),   sep=""),cex=1.2)
text(min(datLW$`TL(cm)`+4.5) ,max(datLW$Weight)-80,pos = 1,adj = c(0.5, NA),labels=paste("p-value = ",pv,   sep=""),cex=1.2)
title(main= c(paste(bb," L-W rel ",i)))
dev.off()
 }
}
# test differenze slopes tra anni
dati$logW <-log(dati$Weight)
dati$logL <-  log(dati$`TL(cm)`)
dati$sex <-as.factor(dati$Sex)
dati$year <-as.factor(dati$anno)

lmyear <- lm(logW ~ logL*year, data = dati)
print(summary(lmyear))
sumy<-stargazer(lmyear, type = "text") # includes Dec
write.csv2(sumy, paste0(plotdirLW,"Test_ComparingLW by years.csv"))
tiff(paste0(plotdirLW,"Test_ComparingLW by years.tif"),width = 85, height = 85, units = "mm", res = 300, pointsize = 6)
fitPlot(lmyear,xlab="log Length (cm)",ylab="log Weight (gr)",legend="topleft",main="")
dev.off()
# test differenze slopes tra sex
lmsex <- lm(logW ~ logL*sex, data = dati)
summary(lmsex);sums<-stargazer(lmsex, type = "text")
write.csv2(sums, paste0(plotdirLW,"Test_ComparingLW by sex.csv"))
tiff(paste0(plotdirLW,"Test_ComparingLW by sex.tif"),width = 85, height = 85, units = "mm", res = 300, pointsize = 6)
fitPlot(lmsex,xlab="log Length (cm)",ylab="log Weight (gr)",legend="topleft",main="")
dev.off()
# test differenze slopes tra anni e sex
lmyrsex <- lm(logW ~ logL*year*sex, data = dati)
summary(lmyrsex);sumys<-stargazer(lmyrsex, type = "text")
write.csv2(sumys, paste0(plotdirLW,"Test_ComparingLW by year & sex.csv"))
tiff(paste0(plotdirLW,"Test_ComparingLW by year & sex.tif"),width = 85, height = 85, units = "mm", res = 500, pointsize = 6)
fitPlot(lmyrsex,xlab="log Length (cm)",ylab="log Weight (gr)",legend="topleft",main="")
dev.off()

####################################
##   Maturity ogive by length by Sex
####################################
dati$TL <- round(dati$`TL(cm)`)

for(i in unique(dati$Sex)) { for (bb in unique(dati$anno))  {       
        datf <- dati %>% dplyr::filter(Sex==i)%>% dplyr::filter(anno==bb)
        datf$mat<-factor(datf$Mat)
        # (immature/mature) vector##
        datf$matbin <- ifelse(datf$Mat == 1,0,1)
        datmatf <- subset(datf, select=c(TL, matbin))
        datmatf <- as.data.frame(ftable(matbin ~ TL, data=datmatf))
        cummatf <- aggregate(Freq~ TL,datmatf,sum)
        #Change the name of the "Freq" variable
        colnames(cummatf)[2]<-"Tot"
        # Build a new data.frame merging the number of mature fish ("Freq") and the total number of bservations ("Tot")
        propmatf <- merge(cummatf, subset(datmatf, matbin=="1"))
        propmatf$TL <- as.numeric(as.character(propmatf$TL))
        #Calculate the proportion of mature fish
        propmatf$p <- propmatf$Freq/propmatf$Tot
        head(propmatf)
        ########### Maturity ogives using a binomial GLM ###########
        M <- glm(p ~ TL, data=propmatf,weights=Tot, family=binomial)
        summary(M);coef(M)
        # Plot estimated and observed data 
        predf <- predict(M,type="response", newdata=data.frame(TL=0:max(dati$TL)))
        par(mfrow=c(1,1))
        
        tiff(paste0(plotdirOg,bb," Maturity ogive Length ",i,".tif"),width = 170, height = 85, units = "mm", res = 200, pointsize = 6)
        plot(0:max(dati$TL),predf,lwd=2,col=4,type="l", xlab = "TL", ylab = "% of mature")
        points(propmatf$TL,  propmatf$p)
        coe <- coef(M)
        L50<- -coe[1]/coe[2]
        segments(0,0.5,L50,0.5, col=2)
        segments(L50,0,L50,0.5,col=2)
        text(L50 + 7,0.5,labels=paste("L50 = ",round(L50,digits=2),   sep=""),cex=1.2)
         title(main= c(paste(bb," Maturity ogive Length ",i)))
        dev.off()
 }
}


####################################
##      Maturity ogive by Age by Sex
####################################
for(i in unique(dati$Sex)) { for (bb in unique(dati$anno)) {       
        datf <- dati %>% dplyr::filter(Sex==i)%>% dplyr::filter(anno==bb)
        datf$mat<-factor(datf$Mat)
        # (immature/mature) vector##
        datf$matbin <- ifelse(datf$Mat == 1,0,1)
        datmatf <- subset(datf, select=c(age, matbin))
        datmatf <- as.data.frame(ftable(matbin ~ age, data=datmatf))
        cummatf <- aggregate(Freq~ age,datmatf,sum)
        #Change the name of the "Freq" variable
        colnames(cummatf)[2]<-"Tot"
        # Build a new data.frame merging the number of mature fish ("Freq") and the total number of bservations ("Tot")
        propmatf <- merge(cummatf, subset(datmatf, matbin=="1"))
        propmatf$age <- as.numeric(as.character(propmatf$age))
        #Calculate the proportion of mature fish
        propmatf$p <- propmatf$Freq/propmatf$Tot
        head(propmatf)
        ########### Maturity ogives using a binomial GLM ###########
        M <- glm(p ~ age, data=propmatf,weights=Tot, family=binomial)
        summary(M);coef(M)
        # Plot estimated and observed data 
        predf <- predict(M,type="response", newdata=data.frame(age=0:max(dati$age)))
        par(mfrow=c(1,1))
        
        tiff(paste0(plotdirOgA,bb," Maturity ogive Age ",i,".tif"),width = 170, height = 85, units = "mm", res = 200, pointsize = 6)
        plot(0:max(dati$age),predf,lwd=2,col=4,type="l", xlab = "TL", ylab = "% of mature")
        points(propmatf$age,  propmatf$p)
        coe <- coef(M)
        L50<- -coe[1]/coe[2]
        segments(0,0.5,L50,0.5, col=2)
        segments(L50,0,L50,0.5,col=2)
        text(L50 + 7,0.5,labels=paste("T50 = ",round(L50,digits=2),   sep=""),cex=1.2)
        title(main= c(paste(bb," Maturity ogive Age ",i)))
        dev.off()
 }
}

