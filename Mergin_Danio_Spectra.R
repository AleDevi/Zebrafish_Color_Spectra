library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(zoo)
library(corrplot)

#from Wouter
#files <- list.files("D:/DroppBoxx/Dropbox/Exp. Priming Brain Lines/Sperm Lines Velocities/Pre/", ".xls", full.names = TRUE)
#frames <- lapply(files, read_xls, sheet = 1, skip = 9)
#names(frames) <- substr(basename(files), 1, nchar(basename(files)) - 4)
#dat_pre <- bind_rows(frames, .id = "male_id")

#setwd("C:/Users/Color&Sound/OneDrive/Desktop/ZebrafishSpectra/Experimental") ##temporary directory to save data



###Getting the names of the datasets in folder (my dataset)
filenames<-list.files("C:/Users/Color&Sound/Experiments/Padova/Zebrafish Odour Choice/ZebrafishSpectra/Experimental",full.names=T)

#Create a list used for its names. The names of this list will be used later
listnames<-sapply(filenames[1:(length(filenames)-1)], 
                  read.csv,
                  sep="\t", 
                  nrows = 1, 
                  header =T,
                  simplify = FALSE) ####

#Change the names of the list removing the path
names(listnames)<-sub("C:/Users/Color&Sound/Experiments/Padova/Zebrafish Odour Choice/ZebrafishSpectra/Experimental/", replacement="",x=names(listnames))

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

names(lista)<-sub("C:/Users/Color&Sound/Experiments/Padova/Zebrafish Odour Choice/ZebrafishSpectra/Experimental/", replacement="",x=names(lista))
names(lista)<-sub(".tsv", replacement="",x=names(lista))

##remove rows out of NM interest to the datasets in the list. NB: these numbers works with 
##that specific spectrometer and probably need to be changed if another instrument is used 
lista2<-lapply(lista, function(x) 
        x[c(-278:-1,-1458:-nrow(x)),]) #these rows numbers keep everything
                                       #between 280 and 700

##also remember to read the WL
Wavelength<-read.table("C:/Users/Color&Sound/Experiments/Padova/Zebrafish Odour Choice/ZebrafishSpectra/Experimental/Wavelength.txt", header=T)
Wavelength<-as.numeric(Wavelength[c(-278:-1,-1458:-2056),])

length(Wavelength)

##a function to :
# change list names removing ".tsv"
#change names to all colums of all datasets in the list
#and keep only those with "transmission" in it
newnames<-function (x,y){for(i in c(1:length(x))){colnames(x[[i]])<-colnames(y[[i]])
x}
        x
}

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
Spectra_average<-Spectra_average %>%filter %>% group_by(ID,Body,NM1) %>%  summarise(Reflectance=(mean(Reflectance))) 

LINEDOWN<-Spectra_average %>% filter(Body=="LD")
LINEUP<-Spectra_average %>% filter(Body=="LU")
TAIL<-Spectra_average %>% filter(Body=="T")

LINEDOWNL<-LINEDOWN%>%group_by(ID) %>% pivot_wider(ID,names_from=NM1,values_from =Reflectance)
LINEUPL<-LINEUP%>%group_by(ID) %>% pivot_wider(ID,names_from=NM1,values_from =Reflectance)
TAILL<-TAIL%>%group_by(ID) %>% pivot_wider(ID,names_from=NM1,values_from =Reflectance)

plot(names(LINEDOWNL),LINEDOWNL[2,])
plot(names(LINEUPL),LINEUPL[2,])
plot(names(LINEUPL),TAILL[2,])

names(LINEDOWNL)
#This dataset could be used to calculate the indexes of "reflectance" basically "Brightness" 
#and "Orange Chroma" (See devigili et al EEE), which is the ratio of reflectance of 550-625/400-700

library("FactoMineR")
library("factoextra")

LD_PCA<-PCA(LINEDOWNL[,c(2:ncol(LINEDOWNL))],scale.unit=T,graph=T,quali.sup=1)
fviz_eig(LD_PCA)
summary(LD_PCA)
plot(LD_PCA)
get_eigenvalue(LD_PCA)
plot(LD_PCA$var$contrib[,1])


LD_PCA<-PCA(LINEDOWNL[,c(2:ncol(LINEDOWNL))],scale.unit=T,graph=T,quali.sup=1)
fviz_eig(LD_PCA)
summary(LD_PCA)
plot(LD_PCA)
get_eigenvalue(LD_PCA)
plot(LD_PCA$var$contrib[,1])
