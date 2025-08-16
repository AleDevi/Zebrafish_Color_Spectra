library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(zoo)


#a bit of bad code to merge, smooth, analyse colour specttra


#from Wouter
#files <- list.files("D:/DroppBoxx/Dropbox/Exp. Priming Brain Lines/Sperm Lines Velocities/Pre/", ".xls", full.names = TRUE)
#frames <- lapply(files, read_xls, sheet = 1, skip = 9)
#names(frames) <- substr(basename(files), 1, nchar(basename(files)) - 4)
#dat_pre <- bind_rows(frames, .id = "male_id")

#setwd("yourfolder") ##temporary directory to save data



###Getting the names of the datasets in folder (my dataset)
filenames<-list.files("yourfolder",full.names=T)

#Create a list used for its names. The names of this list will be used later
listnames<-sapply(filenames[1:(length(filenames)-1)], 
                  read.csv,
                  sep="\t", 
                  nrows = 1, 
                  header =T,
                  simplify = FALSE) ####

#Change the names of the list removing the path
names(listnames)<-sub("yourfolder", replacement="",x=names(listnames))

#for some reason the dataframe in the names list has always one extra column
#the function below removes it
remove_col <- function(mydata) {
        mydata <- subset(mydata, select=-c(ncol(mydata)))
        return(mydata)   # return the modified list/data frame
}

#Apply the function to the objects in the list
listnames2<-sapply(listnames, remove_col)

#test if it worked
ncol(listnames[[1]])
ncol(listnames2[[1]])
#yes

####Working now on the datasets
#getting all the datasets
lista<-sapply(filenames[1:(length(filenames)-1)], 
              read.csv,sep="\t",
              header =F, 
              skip=9,
              simplify = FALSE) ####Per fare un file con una serie di "oggetti" 

names(lista)<-sub("yourfolder", replacement="",x=names(lista))
names(lista)<-sub(".tsv", replacement="",x=names(lista))

##remove rows out of NM interest to the datasets in the list. NB: these numbers works with 
##that specific spectrometer and probably need to be changed if another instrument is used 
lista2<-lapply(lista, function(x) 
        x[c(-278:-1,-1458:-nrow(x)),]) #these rows numbers keep everything
                                       #between 280 and 700

##also remember to read the WL
Wavelength<-read.table("yourfolder/Wavelength.txt", header=T)
Wavelength<-as.numeric(Wavelength[c(-278:-1,-1458:-2056),])

length(Wavelength)

##a function to :
# change list names removing ".tsv"
#change names to all colums of all datasets in the list
#and keep only those with "transmission" in it

#newnames<-function (x,y){for(i in c(1:length(x))){colnames(x[[i]])<-colnames(y[[i]])
#x}
#        x
#}

newnames<-function (x,y){for(i in c(1:length(x))){
        colnames(x[[i]])<-colnames(y[[i]]) #change names of colums in the datasets of the list
x[[i]]<-select(x[[i]],contains("Transmission"))} #keep only colums with "Transmission"
        names(x)<-sub(".tsv", replacement="",x=names(x)) #change nemes of datasets in the list
        x
}


#Applying the function to the list 
lista3<-newnames(lista2,listnames2)

#Check if some files have more measurements
measurements<-data.frame((sapply(lista3,length)))
colnames(measurements)<-"Counts"
morecounts<-filter(measurements,Counts!=9)
measurements
morecounts
rownames(morecounts)

##########FINDING THE PROBLEMS WITH SOME DATSETS (Solved)
#test1<-cbind(Wavelength,lista3$M18B.tsv[,2:10])      
#test2<-gather(test1,key="Color",value="Transmission",names(test1)[2:ncol(test1)])
#test2 %>%
#        mutate(across(Color, factor, levels=c(unique(test2$Color)))) %>%##this to order the facets as they appear in the dataset
#        ggplot(aes(x=Wavelength)) + 
#        geom_point(aes(y=Transmission,col=Color))+ 
#        facet_wrap(Color~.)

#test4<-cbind(Wavelength,lista3$M1B.tsv)      
#test5<-gather(test4,key="Color",value="Transmission",names(test4)[2:ncol(test4)])
#test5 %>%
#        mutate(across(Color, factor, levels=c(unique(test5$Color)))) %>%##this to order the facets as they appear in the dataset
#        ggplot(aes(x=Wavelength)) + ylim(0, 90)+
#        geom_point(aes(y=Transmission,col=Color))+facet_wrap(Color~.)

#test6<-cbind(Wavelength,lista3$F11.tsv)      
#test7<-gather(test6,key="Color",value="Transmission",names(test6)[2:ncol(test6)])
#test7 %>%
#        mutate(across(Color, factor, levels=c(unique(test7$Color)))) %>%##this to order the facets as they appear in the dataset
#        ggplot(aes(x=Wavelength)) + ylim(0, 50)+
#        geom_point(aes(y=Transmission,col=Color))+facet_wrap(Color~.)

#plot(cbind(lista3$M1B.tsv[2],lista3$F2.tsv[1]))


####################################
###Smoothing the spectra and removing the extra NM (281,299)
##########################################################################

#See hoh it look like
plot(Wavelength,lista3[[5]]$Transmission)
#see how it looks after smoothing
plot(Wavelength,rollmean((lista3[[6]]$Transmission.1),50,align="center",fill="extend"))

#This function smooths all the spectra of a list and merge them in a single file
#this is not really useful at the moment. It is better to work a bit more on the lists
smootspectraF<-function(x,y){y<-y#x is the list of dataframes, y is the wavelength file
for(i in c(1:length(x))){
        for(j in c(1:ncol(x[[i]]))){
                y<-cbind(y,(rollmean(x[[i]][,j],50,align="center",fill="extend")))}
        }
y}

#dataset<-smootspectraF(lista3,Wavelength)

#This function to smooth all the spectra of a list and merge them in a list
smootspectra<-function(x,w,f=50){y<-vector("list",length(x))#x is the list of dataframes, y is the wavelength file
names(y)=names(x)
for(i in c(1:length(x))){
        z<-w
        for(j in c(1:ncol(x[[i]]))){
                z<-as.data.frame(cbind(z,rollmean(x[[i]][,j],f,align="center",fill="extend")))
                         }
        z<-cbind(names(x[i]),z)
                 names(z)<-c("ID","NM","LD1","LD2","LD3","LU1","LU2","LU3","T1","T2","T3")
       y[[i]]<-z
       }
y}

AllSpAverage<-smootspectra(lista3,Wavelength)
AllSpAverage<-lapply(AllSpAverage,filter,NM>=300,NM<=700) #remove useless part of the spectra

AllSpAverage$F17 %>% ggplot(aes(x=NM))+geom_point(aes(y=LU1),color="red")+
        geom_point(aes(y=LU2),color="yellow")+
        geom_point(aes(y=LU3),color="green")

##Function to calculate an average value for the 3 categories.
# It may be a a good move to pivot everything to have a 
SpAverage<-bind_rows(AllSpAverage, .id = "ID")

Spectra_average<-SpAverage %>%pivot_longer(!ID&!NM, names_to = "BodyPart", values_to = "Reflectance")
Spectra_average$Body<-substr(Spectra_average$BodyPart, 1,nchar(Spectra_average$BodyPart)-1)
Spectra_average$NM1<-Spectra_average$NM %>% round(,0)
Spectra_average2<-Spectra_average %>%filter %>% group_by(ID,Body,NM1) %>%  summarise(Reflectance=(mean(Reflectance))) 
Spectra1<-Spectra_average2[,c(1:4)]
               
##Below more grouping. It is easier to do it after the line above
Spectra_average2$NM5<-rep(rep(c(rep(seq(300,695,5),each=5),700),times=3),times=72)
Spectra_average2$NM10<-rep(rep(c(rep(seq(300,690,10),each=10),700),times=3),times=72)
Spectra5<-Spectra_average2 %>% group_by(ID,Body,NM5) %>%  summarise(Reflectance=(mean(Reflectance))) 
Spectra10<-Spectra_average2 %>% group_by(ID,Body,NM10) %>%  summarise(Reflectance=(mean(Reflectance))) 
Spectra10$sex<-substr(Spectra10$ID,1,1)

library(lme4)
library(lmerTest)
m1<-lmer(Reflectance~NM10+sex+(1|ID),filter(Spectra10,Body=="LU"))
ranova(m1)
summary(m1)

##Have a look at the average spectra
Spectra1 %>% group_by(Body,NM1) %>% 
        summarise(Reflectance=(mean(Reflectance)))%>% 
        ggplot(aes(x=NM1))+geom_point((aes(y=Reflectance,col=Body)))
Spectra5 %>% ggplot(aes(x=as.factor(NM5)))+geom_boxplot(aes(y=Reflectance))+facet_wrap(~Body)
Spectra10 %>% ggplot(aes(x=as.factor(NM10)))+
        geom_point(aes(y=Reflectance,col=Body),alpha=0.2)+
        geom_boxplot(aes(y=Reflectance))+
        scale_x_discrete("NM",breaks=(seq(300,700,by=25)))+
        facet_wrap(~Body)


?scale_x_discrete()

##Let's find if the lines have different HUEs (dominant colors)
hueref<-Spectra1 %>%
        group_by(ID, Body) %>%
        summarise(Reflectance = max(Reflectance, na.rm=TRUE)) 
hueNM<-left_join(hueref, Spectra1, by="Reflectance")
hueNM$Sex<-substr(hueNM$ID.x,1,1)
hue<-hueNM %>% group_by(Body.x,Sex) %>% summarise(meanhue=mean(NM1))

hueNM %>% filter(Body.x!="T")  %>% ggplot(aes())+
        geom_boxplot(aes(y=NM1,col=Body.x))+coord_flip()

sexbody<-as.factor(paste(hueNM$Body.x[hueNM$Body.x!="T"],hueNM$Sex[hueNM$Body.x!="T"]))
hueNM %>% filter(Body.x!="T")  %>% ggplot(aes())+
        geom_boxplot(aes(y=NM1,col=sexbody))+coord_flip()+
theme(axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank())+ggtitle("Average HUE (color dominance)")

#it seems so. let's see if males and females differ and if the lines are different
lm1<-lm(NM1~Body.x,filter(hueNM,Body.y!="T"))

anova(lm1)

#no differences between sexes in Tail Colouration
lm2<-lm(NM2~Sex,filter(hueNM,Body.y=="T"))
anova(lm2)


#############
##Let see what the PCA say
LINEDOWN<-Spectra1 %>% filter(Body=="LD")
LINEUP<-Spectra1 %>% filter(Body=="LU")
TAIL<-Spectra1 %>% filter(Body=="T")

LINEDOWNL<-LINEDOWN%>%group_by(ID) %>% pivot_wider(ID,names_from=NM1,values_from =Reflectance)
LINEUPL<-LINEUP%>%group_by(ID) %>% pivot_wider(ID,names_from=NM1,values_from =Reflectance)
TAILL<-TAIL%>%group_by(ID) %>% pivot_wider(ID,names_from=NM1,values_from =Reflectance)

group<-LINEDOWNL$ID
LDBR<-rowSums(LINEDOWNL[,c(2:ncol(LINEDOWNL))])/400
LUBR<-rowSums(LINEUPL[,c(2:ncol(LINEDOWNL))])/400
TBR<-rowSums(TAILL[,c(2:ncol(TAILL))])/400


library("FactoMineR")
library("factoextra")

LD_PCA<-prcomp(LINEDOWNL[,c(2:ncol(LINEDOWNL))],center = TRUE, scale. = TRUE)
fviz_eig(LD_PCA)
summary(LD_PCA)
get_eigenvalue(LD_PCA)
ggplot()+geom_point(aes(y=LD_PCA$rotation[,1],x=seq(300,700,1)),col="blue")+
        geom_point(aes(y=LD_PCA$rotation[,2],x=seq(300,700,1)),col="red")#+
        geom_point(aes(y=LD_PCA$rotation[,3],x=seq(300,700,1)),col="green")#+
        #coord_cartesian(ylim=c(0,15))

LU_PCA<-prcomp(LINEUPL[,c(2:ncol(LINEUPL))],center = TRUE, scale. = TRUE)
fviz_eig(LU_PCA)
summary(LU_PCA)
get_eigenvalue(LU_PCA)
ggplot()+geom_point(aes(y=LU_PCA$rotation[,1],x=seq(300,700,5)),col="blue")+
        geom_point(aes(y=LU_PCA$rotation[,2],x=seq(300,700,5)),col="red")#+
        geom_point(aes(y=LU_PCA$rotation[,3],x=seq(300,700,5)),col="green")#+
        #coord_cartesian(ylim=c(0,10))

T_PCA<-prcomp(TAILL[,c(2:ncol(TAILL))],center = T, scale. = TRUE)
summary(T_PCA)
plot(T_PCA)
fviz_eig(T_PCA)
get_eigenvalue(T_PCA)
ggplot()+geom_point(aes(y=T_PCA$rotation[,1],x=seq(300,700,1)),col="blue")+
        geom_point(aes(y=T_PCA$rotation[,2],x=seq(300,700,1)),col="red")#+
        #geom_point(aes(y=T_PCA$rotation[,3],x=seq(300,700,5)),col="green")#+
        #?coord_cartesian(ylim=c(-0.25,0.25))
?prcomp()

PC.Values<-as.data.frame(cbind(LD_PCA$x[,c(1:2)],LU_PCA$x[,c(1:2)],T_PCA$x[,c(1:2)]))
names(PC.Values)<-c("LD_PC1","LD_PC2","LU_PC1","LU_PC2","T_PC1","T_PC2")
PC.Values$ID<-TAILL[,1]
PC.Values<-cbind(PC.Values$ID,PC.Values[,c(1:6)])
names(PC.Values[,1])<-"ID"
PC.Values$Sex<-substr(PC.Values$ID,1,1)
PC.Values<-cbind(PC.Values,(LDBR),(LUBR),(TBR))
names(PC.Values)<-c("ID","LD_PC1","LD_PC2","LU_PC1","LU_PC2","T_PC1","T_PC2","Sex","LD_BR","LU_BR","T_BR")

plot(PC.Values$LD_PC1,PC.Values$LD_BR)
plot(PC.Values$LU_PC1,PC.Values$LU_BR)
plot(PC.Values$T_PC1,PC.Values$T_BR)

library(corrplot)
cordataset<-as.data.frame(PC.Values[,c(2:7,9:11)])
testRes = cor.mtest(cordataset, conf.level = 0.95)
corrplot(cor(cordataset),p.mat = testRes$p, sig.level = 0.01, addrect = 2)


write.csv(PC.Values,"C:/Users/Color&Sound/Experiments/Padova/Zebrafish Odour Choice/ZebrafishSpectra/PCAVALUES1NM.csv")
m1<-lm(T_PC2~Sex,PC.Values)
anova(m1)

PC.ValuesL<-PC.Values %>%pivot_longer(!ID&!Sex, names_to = "BodyPart", values_to = "Reflectance")
PC.ValuesL<-PC.Values %>%pivot_longer(!ID&!Sex, names_to = "BodyPart",values_to = "PCSCORE")
PC.ValuesL
PC.ValuesL %>% filter(BodyPart!=c("LD_BR","LU_BR","T_BR")) %>% ggplot(aes(x=BodyPart ))+geom_boxplot(aes(col=Sex,y=PCSCORE))
PC.ValuesL %>% ggplot(aes(x=BodyPart ))+geom_boxplot(aes(col=Sex,y=PCSCORE))

