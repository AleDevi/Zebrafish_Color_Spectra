##The code below will generate the files .csv necessary as Objects standard
##for colour worker. The file has a structure like that:
matrix(data=c(seq(from=400, to=700, by=5),rep("spectra",61),rep(0,61),rep(0,61)),
               nrow=61,ncol=4)
##the name of each CSV file would be someething like "F1LD", i.e. the individual ID and the body part
##We start from the tibble "Specrta5"
Spectra5

#First thing is to remove the extra information in UV light, create the extra 2 variables and the name
Colwork<-Spectra5 %>% filter(NM5>=400)
Colwork[,c(5,6)]<-0
Colwork[,7]<-str_c(Colwork$ID,Colwork$Body, sep = "", collapse = NULL)
Colwork<-Colwork[,c(3:7)]
#check if we hawe the right amount of lines for each group
table(Colwork[,5])

#create N csv files starting from Colwork
library(readr)
Colwork %>%
        group_by(...7) %>%
        group_walk(~ write_csv(.x, paste0(.y$...7, ".csv"),col_names =NA))
