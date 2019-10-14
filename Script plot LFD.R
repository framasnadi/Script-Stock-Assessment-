####################
#   Grafici LFD    #
####################
library(readr)
library(dplyr)
library(ggplot2)
setwd("C:/Users/f.masnadi/Desktop/Stock Assessment/SS3/Corso Ponza 2019/SOLE/dati/File_catture/file estratti da R")

############################################
# grafici per dati annuali di Landing
plot_LFD_year <- read_csv("plot_LFD_year.csv")
#View(plot_LFD_year)
data<-plot_LFD_year 
head(data)
unique(data$Attrezzo)
# grafico accorpato LFD per fleet per anno (sovrapposte)
# ricordarsi di cambiare il ggtitle con la specie giusta!
ggplot(data, aes(classe_lun,Numero_espanso,color=factor(Anno)))+geom_line(size=1.1)+ggtitle("SOL LFD")+xlab("CL (cm)") +ylab("thousands")+ scale_colour_discrete(name  ="Years")+ facet_wrap(as.factor(data$Attrezzo),scales = "free")
# grafico LFD per anno per attrezzo
ggplot(data, aes(classe_lun,Numero_espanso,color=factor(Attrezzo))) +ggtitle("SOL LFD") + facet_wrap(data$Anno)+geom_line(size=1.1)+xlab("CL (cm)")+ylab("thousands")+ scale_colour_discrete(name  ="Attrezzo")


############################################
# grafici per dati semestrali di Landing
plot_LFD_quarter <- read_csv("plot_LFD_quarter.csv")
data2<-plot_LFD_quarter 
head(data2)
unique(data2$Attrezzo)
unique(data2$Trimestre)
# LFD per quarter per Anno
# ricordarsi di cambiare il ggtitle con la specie giusta!
# GNS
ggplot(data2[data2$Attrezzo=="Reti da Posta",], aes(classe_lun,Numero_espanso,color=factor(Trimestre)))+geom_line(size=1.1)+ggtitle("SOL GNS")+xlab("CL (cm)") +ylab("thousands")+ scale_colour_discrete(name  ="Quarter")+ facet_wrap(as.factor(data2[data2$Attrezzo=="Reti da Posta",]$Anno),scales = "free")
# OTB
ggplot(data2[data2$Attrezzo=="Strascico",], aes(classe_lun,Numero_espanso,color=factor(Trimestre)))+geom_line(size=1.1)+ggtitle("SOL OTB")+xlab("CL (cm)") +ylab("thousands")+ scale_colour_discrete(name  ="Quarter")+ facet_wrap(as.factor(data2[data2$Attrezzo=="Strascico",]$Anno),scales = "free")
# TBB
ggplot(data2[data2$Attrezzo== "Rapido" ,], aes(classe_lun,Numero_espanso,color=factor(Trimestre)))+geom_line(size=1.1)+ggtitle("SOL TBB")+xlab("CL (cm)") +ylab("thousands")+ scale_colour_discrete(name  ="Quarter")+ facet_wrap(as.factor(data2[data2$Attrezzo=="Rapido",]$Anno),scales = "free")
# GTR
ggplot(data2[data2$Attrezzo=="Tremaglio",], aes(classe_lun,Numero_espanso,color=factor(Trimestre)))+geom_line(size=1.1)+ggtitle("SOL GTR")+xlab("CL (cm)") +ylab("thousands")+ scale_colour_discrete(name  ="Quarter")+ facet_wrap(as.factor(data2[data2$Attrezzo=="Tremaglio",]$Anno),scales = "free")
# FPO
ggplot(data2[data2$Attrezzo=="Nasse",], aes(classe_lun,Numero_espanso,color=factor(Trimestre)))+geom_line(size=1.1)+ggtitle("SOL FPO")+xlab("CL (cm)") +ylab("thousands")+ scale_colour_discrete(name  ="Quarter")+ facet_wrap(as.factor(data2[data2$Attrezzo=="Nasse",]$Anno),scales = "free")
# PTF
ggplot(data2[data2$Attrezzo=="Volante",], aes(classe_lun,Numero_espanso,color=factor(Trimestre)))+geom_line(size=1.1)+ggtitle("SOL PTF")+xlab("CL (cm)") +ylab("thousands")+ scale_colour_discrete(name  ="Quarter")+ facet_wrap(as.factor(data2[data2$Attrezzo=="Volante",]$Anno),scales = "free")
# OTHER
ggplot(data2[data2$Attrezzo=="Other",], aes(classe_lun,Numero_espanso,color=factor(Trimestre)))+geom_line(size=1.1)+ggtitle("SOL Other")+xlab("CL (cm)") +ylab("thousands")+ scale_colour_discrete(name  ="Quarter")+ facet_wrap(as.factor(data2[data2$Attrezzo=="Other",]$Anno),scales = "free")
# LFD per quarter per Attrezzo (indipendentemente dagli anni)
data2$Attrezzo<-ifelse(grepl("Reti da Posta", data2$Attrezzo), "Reti_da_Posta", data2$Attrezzo)
ggplot(data2, aes(classe_lun,Numero_espanso,color=factor(Attrezzo))) +ggtitle("SOL by Quarter") +
  facet_wrap(data2$Trimestre)+geom_line(size=1.1)+xlab("CL (cm)")+ylab("thousands")+ scale_colour_discrete(name  ="Attrezzo")

############################################
# grafici per dati annuali di Discard
plot_LFD_discard_year <- read_csv("plot_LFD_discard_year.csv")
#View(plot_LFD_year)
data3<-plot_LFD_discard_year 
head(data3)
unique(data3$Attrezzo)
# grafico accorpato LFD per fleet per anno (sovrapposte)
# ricordarsi di cambiare il ggtitle con la specie giusta!
ggplot(data3, aes(classe_lun,Numero_espanso,color=factor(Anno)))+geom_line(size=1.1)+ggtitle("SOL Discard LFD")+xlab("CL (cm)") +ylab("thousands")+ scale_colour_discrete(name  ="Years")+ facet_wrap(as.factor(data3$Attrezzo),scales = "free")
# grafico LFD per anno per attrezzo
ggplot(data3, aes(classe_lun,Numero_espanso,color=factor(Attrezzo))) +ggtitle("SOL Discard LFD") + facet_wrap(data3$Anno)+geom_line(size=1.1)+xlab("CL (cm)")+ylab("thousands")+ scale_colour_discrete(name  ="Attrezzo")


##################################################
# ciclo grafici LFD per porto
data<- read_csv("LFD_Campbiol.csv")
head(data)
setwd("C:/Users/f.masnadi/Desktop/Stock Assessment/SS3/Corso Ponza 2019/SOLE/dati/File_catture/file estratti da R/grafici porto")
plot_list = list()
for ( prt in unique(data$LO01_PORTO) )  {
  dataset<-data%>%dplyr::filter(LO01_PORTO==prt)
  dataset2<-dataset%>%dplyr::group_by(Anno,TIPO_OSSERVAZIONE,classe_lun)%>%summarize(n=sum(n_land))
  X11() 
  plot.new()
  par(mfrow=(c(1,1)))
  # ricordarsi di cambiare il ggtitle con la specie giusta!
  p = ggplot(dataset2, mapping=aes(x=classe_lun, y= n, fill=TIPO_OSSERVAZIONE), color=~TIPO_OSSERVAZIONE)+geom_col(position = "dodge")+facet_wrap(~Anno)+ggtitle(paste("LFD Solea_", prt ))
  plot_list[[prt]] = p
  png(paste("plot_", prt , ".png", sep = ""), width=1200, height=1200, res=120) 
  print(plot_list[[prt]])
  dev.off() # finish export
}   


# ciclo grafici LFD per porto
data<- read_csv("LFD_Campbiol.csv")
head(data)
setwd("C:/Users/f.masnadi/Desktop/Stock Assessment/SS3/Corso Ponza 2019/SOLE/dati/File_catture/file estratti da R/grafici porto")
plot_list = list()
for ( prt in unique(data$LO01_PORTO) )  {
  dataset<-data%>%dplyr::filter(LO01_PORTO==prt)
  dataset2<-dataset%>%dplyr::group_by(Anno,TIPO_OSSERVAZIONE,classe_lun)%>%summarize(n=sum(n_land))
  X11() 
  plot.new()
  par(mfrow=(c(1,1)))
  # ricordarsi di cambiare il ggtitle con la specie giusta!
  p = ggplot()+facet_wrap(~Anno)+geom_col(data = dataset2 %>% group_by(Anno, TIPO_OSSERVAZIONE, classe_lun)%>% tally(), aes(x=classe_lun,y=n,fill=factor(TIPO_OSSERVAZIONE)), position = position_dodge(width = 0.5))+ geom_line(data = dataset2 %>% group_by(Anno,classe_lun)%>% tally(), aes(x=classe_lun, y= n))
  plot_list[[prt]] = p
  png(paste("plot_", prt , ".png", sep = ""), width=1200, height=1200, res=120) 
  print(plot_list[[prt]])
  dev.off() # finish export
} 


###########################################
# tabelle con statistiche
setwd("C:/Users/f.masnadi/Desktop/Stock Assessment/SS3/Corso Ponza 2019/SOLE/dati/File_catture/file estratti da R")
data<- read_csv("LFD_Campbiol.csv")
ll <- function(standev) pchisq(standev, df=n()-1, lower.tail=FALSE)-0.025
uu <- function(standev) pchisq(standev, df=n()-1, lower.tail=FALSE)-0.975
# tabella com medie e dev. stand. per PORTO e ATTREZZO (originale Stefano)
pa <- data %>% group_by(LO01_PORTO, Attrezzo) %>% summarize(media = mean(classe_lun ),  ic_media_low = media - 1.96*sd(classe_lun)/sqrt(n()), ic_media_up = media + 1.96*sd(classe_lun)/sqrt(n()), standev = sd(classe_lun), ic_dev_low = sqrt((n()-1)*(standev^2)/uniroot(ll, lower=-0.1, upper=100000000)$root), ic_dev_up = sqrt((n()-1)*(standev^2)/uniroot(uu, lower=-0.1, upper=100000000)$root))
View(pa)
write.csv(pa, "stat_PortoXattrezzo.csv")
# tabella com medie e dev. stand. per ANNO, PORTO e ATTREZZO (originale Stefano)
pa2 <- data %>% group_by(Anno, LO01_PORTO, Attrezzo) %>% summarize(media = mean(classe_lun ), specimen_sampled = length(classe_lun) , number_of_samples= length(unique(LO01_ID)),  ic_media_low = media - 1.96*sd(classe_lun)/sqrt(n()), ic_media_up = media + 1.96*sd(classe_lun)/sqrt(n()), standev = sd(classe_lun), ic_dev_low = sqrt((n()-1)*(standev^2)/uniroot(ll, lower=-0.1, upper=100000000)$root), ic_dev_up = sqrt((n()-1)*(standev^2)/uniroot(uu, lower=-0.1, upper=100000000)$root))
View(pa2)
write.csv(pa2, "stat_PortoXattrezzoXanno.csv")

# Plot di medie LFD in funzione dell'ANNO e del PORTO e del tipo di osservazione
df <- data %>% group_by(LO01_PORTO, Anno, TIPO_OSSERVAZIONE ) %>% summarize(media = mean(classe_lun))
ggplot(df, aes(x = Anno, media, color=TIPO_OSSERVAZIONE)) + geom_point(size = 2.5) + theme_bw() + ylab("Media taglia (mm)") + xlab("Year") + theme(axis.title.x = element_text(size=10), axis.text.x  = element_text(size=10, colour = "black")) + theme(axis.title.y = element_text(size=10), axis.text.y  = element_text(size=10, colour = "black")) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + facet_wrap(~LO01_PORTO, ncol=3) + geom_smooth(method="lm", se=F) + theme(strip.text.x = element_text(size = 6, margin = margin(0, 0, 0, 0, "cm")))
# Plot di medie TAGLIA in funzione dell'ANNO e dell'ATTREZZO e del tipo di osservazione
#df <- data %>% group_by(Attrezzo, Anno, TIPO_OSSERVAZIONE) %>% summarize(media = mean(classe_lun))
#ggplot(df, aes(x = Anno, media, color=TIPO_OSSERVAZIONE)) + geom_point(size = 2.5) + theme_bw() + ylab("Media taglia (mm)") + xlab("Year") + theme(axis.title.x = element_text(size=10), axis.text.x  = element_text(size=10, colour = "black")) + theme(axis.title.y = element_text(size=10), axis.text.y  = element_text(size=10, colour = "black")) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + facet_wrap(~Attrezzo, ncol=3) + geom_smooth(method="lm", se=F)


# ANOVA con post-hoc ------------------------------------------------------
library(FSA)
library(car)
library(userfriendlyscience)
# Si fa prima il test di Levene per l'omogeneità delle varianze
# Se uguali, test parametrico normale
# Se diverse, post-hoc con games-howell
# FATTORE: PORTO
Summarize(classe_lun ~ Anno, data = data) 
leveneTest(classe_lun ~ Anno, data = data)
one.way <- oneway(factor(data$Anno), y = data$classe_lun, posthoc = 'games-howell') # varianze diverse
one.way
tky = as.data.frame(one.way$`intermediate`$posthoc)
tky$pair = rownames(tky)
label <- as.character(formatC(tky[,6], digits = 3, width = 4, format = "f", flag = "0"))
# Plot pairwise TukeyHSD comparisons and shape by significance level
# il valore di cut <0 è per evitare che degli 0 vengano tradotti in NA
ggplot(tky, aes(shape = cut(`p`, c(-0.1, 0.05, 1), label = c("p<0.05", "Non-Sig")))) +
geom_hline(yintercept = 0, lty= "11", colour = "grey30") + geom_errorbar(aes(pair, ymin = ci.lo, ymax = ci.hi), width = 0.2) + geom_point(aes(pair, diff), size = 4) + labs(shape = "") + theme_bw() + scale_shape_manual(values=c(1, 19)) + annotate("text", x = tky$pair, y = tky$diff, label = label, hjust = 1.4, vjust = 0.5, size = 4)  + theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.1, hjust = 1)) + scale_y_continuous(limits=c(-10, 10)) + scale_x_discrete("pair")

