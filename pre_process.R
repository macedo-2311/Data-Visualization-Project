
###PREPROCESS######
data_set <- read.csv("DataSetProject.csv", sep = ",", header = TRUE)
data_set$GPD<-as.double(as.character(data_set$GPD))
data_set$long<-as.numeric(as.character(data_set$long))
data_set$lat<- as.numeric(as.character(data_set$lat))

#******************MAPA******************************
# Read Africa Data Set
mapafrica<- readOGR(".", "Africa")
projeto_2015r<-  read.csv("DataSetProject2015.csv", sep = ",", header = TRUE)
projeto_2015<- geo_join(mapafrica, projeto_2015r, "COUNTRY", "Entity", how="left")
projeto_2015$GPD[ which( is.na(projeto_2015$GPD))] = 0


data_barplot<-as.data.frame(data_set)


#******************LOLIPOP******************************

lollipop_data<- data_set[- c(1275:1404),c(1,2,3,7,6,10)]
lollipop_data$MalariaDeaths= as.double(as.character(lollipop_data$MalariaDeaths))
lollipop_data$HIVDeaths= as.double(as.character(lollipop_data$HIVDeaths))
lollipop_data$MalariaDeaths= round(lollipop_data$MalariaDeaths,2)
lollipop_data$HIVDeaths= round(lollipop_data$HIVDeaths,2)
lollipop_data=transform(lollipop_data,minimo =pmin(HIVDeaths, MalariaDeaths))
lollipop_data=transform(lollipop_data, maximo= pmax(HIVDeaths, MalariaDeaths))

#******************BARPLOT******************************

data_barplot<-as.data.frame(data_set)
data_barplot=data_barplot[, c(1,3,4,10)]
data_barplot=data_barplot[- c(1275:1404),]


#******************TIME SERIES******************************
timeseries_data=read.csv('DataSetMGD.csv')
timeseries_data$val=round(timeseries_data$val,2)
timeseries_data$year=as.character((timeseries_data$year))
timeseries_data$year=as.Date((timeseries_data$year), "%Y")

