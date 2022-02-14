library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(zoo)

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

##remove rows out of NM interest to the datasets in the list. NB: these numbers works with 
##that specific spectrometer and probably need to be changed if another instrument is used 
lista2<-lapply(lista, function(x) 
        x[c(-278:-1,-1458:-nrow(x)),]) #these rows numbers keep everything
                                       #between 250 and 700

##a function to change names to all colums of all datasets in the list
newnames<-function (x,y){for(i in c(1:length(x))){colnames(x[[i]])<-colnames(y[[i]])
x}
        x
}

#newnames<-function (x,y){for(i in c(1:length(x))){names(x)<-names(y)
#        colnames(x[[i]])<-colnames(y[[i]])
#x}
#        x
#}

#Applying the function to the list 
lista3<-newnames(lista2,listnames2)


#Keep only the columns with the transmission data
lista4<-lapply(lista3, select,contains("Transmission"))

#Check if some files have more measurements
measurements<-data.frame((sapply(lista4,length)))
colnames(measurements)<-"Counts"
morecounts<-filter(measurements,Counts!=9)
measurements
morecounts
rownames(morecounts)

test1<-cbind(Wavelength,lista4$M18B.tsv[,2:10])      
test2<-gather(test1,key="Color",value="Transmission",names(test1)[2:ncol(test1)])
test2 %>%
        mutate(across(Color, factor, levels=c(unique(test2$Color)))) %>%##this to order the facets as they appear in the dataset
        ggplot(aes(x=Wavelength)) + 
        geom_point(aes(y=Transmission,col=Color))+ 
        facet_wrap(Color~.)

test4<-cbind(Wavelength,lista4$M1B.tsv)      
test5<-gather(test4,key="Color",value="Transmission",names(test4)[2:ncol(test4)])
test5 %>%
        mutate(across(Color, factor, levels=c(unique(test5$Color)))) %>%##this to order the facets as they appear in the dataset
        ggplot(aes(x=Wavelength)) + ylim(0, 90)+
        geom_point(aes(y=Transmission,col=Color))+facet_wrap(Color~.)

test6<-cbind(Wavelength,lista4$F11.tsv)      
test7<-gather(test6,key="Color",value="Transmission",names(test6)[2:ncol(test6)])
test7 %>%
        mutate(across(Color, factor, levels=c(unique(test7$Color)))) %>%##this to order the facets as they appear in the dataset
        ggplot(aes(x=Wavelength)) + ylim(0, 50)+
        geom_point(aes(y=Transmission,col=Color))+facet_wrap(Color~.)



cbind(lista4$M1B.tsv[2],lista4$F2.tsv[1])
####################################
###Smoothing the spectra and removing the extra NM (281,299)
##########################################################################
Wavelength<-read.table("Wavelength.txt", header=T)
Wavelength<-as.numeric(Wavelength[c(-278:-1,-1458:-2056),])

length(Wavelength)

#See hoh it look like
plot(Wavelength,lista4[[5]]$Transmission)
plot(Wavelength,rollmean((lista4[[6]]$Transmission.1),50,align="center",fill="extend"))


#This function to smooth all the spectra of a list and merge them in a single file
smootspectraF<-function(x,y){y<-y#x is the list of dataframes, y is the wavelength file
for(i in c(1:length(x))){
        for(j in c(1:ncol(x[[i]]))){
                y<-cbind(y,(rollmean(x[[i]][,j],50,align="center",fill="extend")))}
        }
y}

#This function to smooth all the spectra of a list and merge them in a list
smootspectra<-function(x,w,f=50){y<-vector("list",length(x))#x is the list of dataframes, y is the wavelength file
names(y)=names(x)
for(i in c(1:length(x))){
        z<-w
        for(j in c(1:ncol(x[[i]]))){
                z<-as.data.frame(cbind(z,rollmean(x[[i]][,j],f,align="center",fill="extend")))
                #z<-z[z[1]>300,]
         }
        #names(z)<-c("NM",names(x[[i]]))
        names(z)<-c("NM","LD1","LD2","LD3","LU1","LU2","LU3","T1","T2","T3")
       y[[i]]<-z}
y}


AllSpAverage<-smootspectra(lista4,Wavelength)

plot(AllSpAverage$F1.tsv[,1],AllSpAverage$F1.tsv[,2])

######################################################################
test[[2]]<-list(rollmean(lista4[[1]][,2],50,align="center",fill="extend"))
test

?avey<-vector("list",length(lista4))
y<-list("123")
str(lista4[[1]])
names(lista4)
lista4

list(1,rollmean(lista4[[1]][,9],50,align="center",fill="extend"))

test1<-lista4[[1]]
test1[,1]
lista4[[1]][,2]

plot(AllSpAverage[[1]][[1]])



########################################
sapply(AllSpAverage,length)
names(AllSpAverage) <- substr(names(AllSpAverage),1,nchar(names(AllSpAverage)) - 8)
raw_Spectra_Data <- bind_rows(AllSpAverage, .id = "male_id")
NM<-seq(400,700, by=5)
Spectra_Data_col<-cbind(NM,raw_Spectra_Data)
plot(Spectra_Data_col$NM,Spectra_Data_col$m1m1) #test if it worked
str(Spectra_Data_col)      




#test<-cbind(Spectra_Data_col[,1],gather(Spectra_Data_col[,2:length(Spectra_Data_col)])) #this is another method to put all the males in column. The line below is a bit mmore efficient
Spectra_Data_rows<-Spectra_Data_col %>% 
        pivot_longer(!NM, names_to = "MeasureMale", values_to = "Reflectance")
str(Spectra_Data_rows)
Spectra_Data_rows$Measure<-str_sub(Spectra_Data_rows$MeasureMale, start = 1, end = 2)
Spectra_Data_rows$Male<-str_sub(Spectra_Data_rows$MeasureMale, start = 3, end = str_length(Spectra_Data_rows$MeasureMale))
str_sub(Spectra_Data_rows$Measure,1,1)<-""
Spectra_Data_rows$Measure<-as.factor(Spectra_Data_rows$Measure)
Spectra_Data_rows$MeasureMale<-as.factor(Spectra_Data_rows$MeasureMale)
Spectra_Data_rows$Male<-as.factor(Spectra_Data_rows$Male)
str(Spectra_Data_rows)
names(Spectra_Data_rows)
Spectra_Data_rows %>% filter(Measure==1) %>% ggplot(aes(x=NM, col=Male))+geom_point(aes(y=Reflectance))
Spectra_Data_rows  %>% ggplot(aes(x=NM, col=Male))+geom_point(aes(y=Reflectance))+facet_grid(Measure~.)
Spectra_Data_rows  %>% ggplot(aes(x=as.factor(NM)))+geom_boxplot(aes(y=Reflectance))+facet_grid(Measure~.)

#adding a treatment column
fishnames<-read_excel("C:/Users/Color&Sound/Downloads/MalesTreatmentMortality.xlsx")
fishnames$Male<-str_c("m",fishnames$Male_ID, sep = "", collapse = NULL)
nametreat<-fishnames[,c(9,5,2,3)]

Spectra_Data_rows<-left_join(Spectra_Data_rows, nametreat, by = "Male")

Spectra_Data_rows  %>% ggplot(aes(x=NM, col=Treatment))+geom_point(aes(y=Reflectance))+facet_grid(Measure~.)

std <- function(x) sd(x)/sqrt(length(x))

AverageSpectrum<-Spectra_Data_rows %>% group_by(Treatment,Measure,NM) %>% summarise(Mean_spectrum=mean(Reflectance ),
                                                                se=std(Reflectance))

AverageSpectrum %>% ggplot(aes(x=NM))+
        geom_pointrange(aes(y=Mean_spectrum,ymin=Mean_spectrum+se,ymax=Mean_spectrum-se,col=Treatment))+
        facet_wrap(~Measure)

Spectra_Data_rows %>% filter(Measure!=3)  %>% ggplot(aes(x=NM, col=Measure))+geom_point(aes(y=Reflectance,col=Measure))+facet_wrap(~Male)


#This dataset could be used to calculate the indexes of "reflectance" basically "Brightness" 
#and "Orange Chroma" (See devigili et al EEE), which is the ratio of reflectance of 550-625/400-700

TOTAL<-group_by(Spectra_Data_rows,Treatment,Measure, Male,MeasureMale) %>% summarise(TOTREF=sum(Reflectance))
ORANGE<-filter(group_by(Spectra_Data_rows,Treatment,Measure, Male),NM<=625&NM>=550) %>% summarise(ORREF=sum(Reflectance))
UV<-filter(group_by(Spectra_Data_rows,Treatment,Measure, Male),NM<=425&NM>=400) %>% summarise(UV=sum(Reflectance))


#check male 1 first measure:
sum(Spectra_Data_rows[Spectra_Data_rows$Male=="m1"&Spectra_Data_rows$Measure=="1",3])
#put together the two datasets and calculate the Chroma Index
OrangeChroma<-cbind(TOTAL,ORANGE[,4],UV[,4])
OrangeChroma$Chroma<-(OrangeChroma$ORREF/OrangeChroma$TOTREF)
OrangeChroma$UVChroma<-(OrangeChroma$UV/OrangeChroma$TOTREF)
OrangeChroma[OrangeChroma$Measure!=3,] %>% ggplot(aes(x=Measure))+geom_boxplot(aes(y=Chroma,col=Treatment))
OrangeChroma[OrangeChroma$Measure!=3,] %>% ggplot(aes(x=Measure))+geom_boxplot(aes(y=TOTREF,col=Treatment))
OrangeChroma[OrangeChroma$Measure!=3,] %>% ggplot(aes(x=Measure))+geom_boxplot(aes(y=UVChroma,col=Treatment))


m1<-lmer(UVChroma~Measure+Treatment+(1|Male),OrangeChroma[OrangeChroma$Measure!=3,])
Anova(m1)

OrangeChroma_2<-OrangeChroma[OrangeChroma$Measure!=3,]


##We need to pivot Spectra_Data_rows dataset in order to get the PCA
library("FactoMineR")
library("factoextra")
FORPCA1_2<-Spectra_Data_rows %>% spread(NM,Reflectance) %>% filter(Measure !=3)
PCA1_2<-PCA(FORPCA1_2[,c(7:ncol(FORPCA1_2))],scale.unit=T, ncp=5,graph=T)
fviz_eig(PCA1_2)

PCA1_2<-PCA(FORPCA1_2[,c(4:ncol(FORPCA1_2))],scale.unit=T, ncp=5,graph=T,quali.sup=1)
summary(PCA1_2)
plot(PCA1_2,habillage=1)

WITHPCA1_2<-cbind(FORPCA1_2[,c(1:6)],PCA1_2$ind$coord[,c(1:3)])


m1<-lmer(Dim.3~Treatment*Measure+(1|Male),WITHPCA1_2)
Anova(m1)

WITHPCA1_2  %>% ggplot(aes(x=Measure, col=Treatment))+geom_boxplot(aes(y=Dim.1))
WITHPCA1_2  %>% ggplot(aes(x=Measure, col=Treatment))+geom_boxplot(aes(y=Dim.2))


finalDataset<-cbind(OrangeChroma_2,WITHPCA1_2)

finalDataset<-left_join(OrangeChroma_2, WITHPCA1_2, by = "MeasureMale")

write.csv(file="spectradata.csv",finalDataset)
#plot(PCA1_2, choix="var",shadow=T,select="contrib 25",cex=0.7)

?PCA()
plot(get_eigenvalue(PCA1_2)[,2])
var<-get_pca(PCA1_2)
plot(get_pca(PCA1_2)$contrib[,1])
plot(get_pca(PCA1_2)$contrib[,2])
plot(get_pca(PCA1_2)$contrib[,3])

plot(get_pca(PCA1_2)$coord[,2])

plot(PCA1_2$ind$coord[,1])

head(var$coord)
plot(var$coord[,1])
plot(var$coord[,2])
plot(var$coord[,3])

FORPCA1_2[,c(1:6)]
PCA1_2
PCA1_2$eig

FORPCA1_2


plot(loadings[,1])
















































#############################################################################
#############################################################################
#############################################################################
#############################################################################
sapply(lista, simplify = FALSE)

sapply(lista, nrow) ####per contare le righe degli oggetti nella "lista"
lapply(mean)
justmean<-function(x){rowMeans(x[,2:c(ncol(x))])}

sapply(lista,justmean)

lista[,c(2:3)]

Test<-lista[[]]
justmean(lista[[1]])

rea



###################################################################################################
#######Per leggere, unire in colonna e poi salvare su un file tutti i file CSV di una cartella#####
###################################################################################################



#ALLSPECTRA<-do.call("cbind", sapply(filenames, read.csv, simplify = FALSE)) ##questo quasi funziona MA non riesco a ridurre il nome del file. NB:Attacca il nome del file alla PRIMA RIGA dei file.
ALLSPECTRA<-do.call("cbind", sapply(filenames, function(x){read.csv(file=x,header=FALSE)}, simplify = FALSE)) ##NON riduce il nome del file. Non usa la prima riga dei file come header. 

#prove data<-sapply(filenames, function(x){read.csv(file=x,na.strings=400:700, header=FALSE)}, simplify = FALSE)
#prove data2<-read(data[complete.cases(data), ])
#prove data2
#prove ALLSPECTRA<-do.call("cbind", data) ##NON riduce il nome del file. Non usa la prima riga dei file come header. NON legge la rima colonna di ogni file .CSV

write.table(ALLSPECTRA,file="Allspectra.csv",sep=",",row.names=F)


############################################################################################################################################
####pezzi ci codici e comandi raccattati in internet###

filenames <- list.files()
do.call("cbind", lapply(filenames, read.csv(file=x,header=FALSE), header = TRUE))


############################################################################################################################################
####altri pezzi da stakoverflow. Non funzionano benissimo....


###setwd() serve per decidere la directory con il "path". NB il path va fra virgolette


mypath<-"C:/Users/Color&Sound/OneDrive/Desktop/Orange intensity ColourWorker"

multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=FALSE)})
  #Reduce(function(x,y) {merge(x,y)}, datalist)
}
mydata=multmerge("C:/Users/Color&Sound/OneDrive/Desktop/Orange intensity ColourWorker")

View(mydata)
