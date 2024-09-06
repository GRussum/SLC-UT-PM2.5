#Project data
library(readr)
library(dplyr)
library(readxl)

#Dec. 2018 data
pollution18 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/TRX01_2018_12.csv.gz"))
View(pollution18) #after viewing the data and seeing -9999.00 for many different numbers, cleaning must begin before analyzation.
#after attempting to clean, I realized this data set contained 113 rows of -9999.0 value data for PM2.5, thus further cleaning had to take place
table(pollution18$ES642_PM2.5_Concentration)
pollution18$X2B_Ozone_Concentration <- as.numeric(pollution18$X2B_Ozone_Concentration)
pollution18 <- pollution18[-c(1:113),] #removal of the first 113 rows from the data
pollution18$ES642_PM2.5_Concentration <- as.numeric(pollution18$ES642_PM2.5_Concentration)
pollution18 <- pollution18[pollution18 >= 0]
pollution18 <- data.frame(ES642_PM2.5_Concentration = pollution18)
pollution18$ES642_PM2.5_Concentration <- as.numeric(pollution18$ES642_PM2.5_Concentration)

avg18<-mean(pollution18$ES642_PM2.5_Concentration, na.rm=TRUE) #resulting in a very high average of 205
pollution18 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/TRX01_2018_12.csv.gz"))
poll18 <- na.omit(pollution18)


#Function for pollution mapping and correlation
library(dplyr)
Pollmap <- function(filename) {
  
  pollution <- read.csv(gzfile(paste0("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/", filename)), skip=1)
  
  names(pollution) <- names(read.csv(gzfile(paste0("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/", filename))))
  
  poll <- pollution[pollution$ES642_PM2.5_Concentration != -9999 &
                      
                      pollution$Latitude != -9999 &
                      
                      pollution$Longitude != -9999,
                    
                    c("Latitude","Longitude","ES642_PM2.5_Concentration")]
  
  poll <- poll %>% mutate(LatLon=paste(round(Latitude,3),round(Longitude,3),sep=":")) %>%
    
    select(LatLon, ES642_PM2.5_Concentration)
  
  pollsum <- poll %>% group_by(LatLon) %>% summarize(meanPM2.5=mean(ES642_PM2.5_Concentration),
                                                     maxPM2.5=max(ES642_PM2.5_Concentration),.groups="keep")
  
  pollsum <- data.frame(pollsum)
  
  pollsum$Latitude <- as.numeric(matrix(unlist(strsplit(pollsum$LatLon,":")),ncol=2, byrow=TRUE)[,1])
  
  pollsum$Longitude <- as.numeric(matrix(unlist(strsplit(pollsum$LatLon,":")),ncol=2, byrow=TRUE)[,2])
  
  PMname <- substr(filename, 7, 14)
  
  library(ggplot2)
  pplot <- ggplot(data=pollsum) +
    geom_point(mapping=aes(y=Latitude,x=Longitude,color=maxPM2.5)) +
    scale_color_gradient(limits=c(0,80)) +
    ggtitle(PMname)
  
  print(pplot)
  return(pplot)
}

save_source <- "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files"

PM112018 <- Pollmap("TRX01_2018_12.csv.gz")
month_num <- substr(PM112018$labels$title, 6, 7)
year <- substr(PM112018$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM112018 <- PM112018 + ggtitle(new_title)
ggsave(filename = "PM112018_plot.jpeg", plot = PM112018, width = 6.5, height = 6.5, dpi = 300) #SAVED TO PROJECT FILES
print(PM112018)

PM112018<-Pollmap("TRX01_2018_11.csv.gz")
month_num <- substr(PM112018$labels$title, 6, 7)
year <- substr(PM112018$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM112018 <- PM112018 + ggtitle(new_title)
ggsave(filename = paste0(save_source, "/PM112018_plot.jpeg"), plot = PM122018, width = 6.5, height = 6.5, dpi = 300)
print(PM112018)

PM122018 <- Pollmap("TRX01_2018_12.csv.gz")
month_num <- substr(PM122018$labels$title, 6, 7)
year <- substr(PM122018$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM122018 <- PM122018 + ggtitle(new_title)
ggsave(filename = paste0(save_source, "/PM122018_plot.jpeg"), plot = PM122018, width = 6.5, height = 6.5, dpi = 300)
print(PM122018)

PM012019<-Pollmap("TRX01_2019_01.csv.gz")
month_num <- substr(PM012019$labels$title, 6, 7)
year <- substr(PM012019$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM012019 <- PM012019 + ggtitle(new_title)
ggsave(filename = "PM012019_plot.jpeg", plot = PM112018, width = 6.5, height = 6.5, dpi = 300)
print(PM012019)


PM022019<-Pollmap("TRX01_2019_02.csv.gz")
month_num <- substr(PM022019$labels$title, 6, 7)
year <- substr(PM022019$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM022019 <- PM022019 + ggtitle(new_title)
ggsave((filename = "PM022019_plot.jpeg"), plot = PM022019, width = 6.5, height = 6.5, dpi = 300)
print(PM022019)


PM032019<-Pollmap("TRX01_2019_03.csv.gz")
month_num <- substr(PM032019$labels$title, 6, 7)
year <- substr(PM032019$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM032019 <- PM032019 + ggtitle(new_title)
ggsave((filename = "PM032019_plot.jpeg"), plot = PM032019, width = 6.5, height = 6.5, dpi = 300)
print(PM032019)

PM042019<-Pollmap("TRX01_2019_04.csv.gz")
month_num <- substr(PM042019$labels$title, 6, 7)
year <- substr(PM042019$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM042019 <- PM042019 + ggtitle(new_title)
ggsave((filename = "PM042019_plot.jpeg"), plot = PM042019, width = 6.5, height = 6.5, dpi = 300)
print(PM042019)

PM052019<-Pollmap("TRX01_2019_05.csv.gz")
month_num <- substr(PM052019$labels$title, 6, 7)
year <- substr(PM052019$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM052019 <- PM052019 + ggtitle(new_title)
ggsave((filename = "PM052019_plot.jpeg"), plot = PM052019, width = 6.5, height = 6.5, dpi = 300)
print(PM052019)

PM062019<-Pollmap("TRX01_2019_06.csv.gz")
month_num <- substr(PM062019$labels$title, 6, 7)
year <- substr(PM062019$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM062019 <- PM062019 + ggtitle(new_title)
ggsave((filename = "PM062019_plot.jpeg"), plot = PM062019, width = 6.5, height = 6.5, dpi = 300)
print(PM062019)

PM072019<-Pollmap("TRX01_2021_07.csv.gz")
month_num <- substr(PM072019$labels$title, 6, 7)
year <- substr(PM072019$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM072019 <- PM072019 + ggtitle(new_title)
ggsave((filename = "PM072019_plot.jpeg"), plot = PM072019, width = 6.5, height = 6.5, dpi = 300)
print(PM072019)

PM082019<-Pollmap("TRX01_2019_08.csv.gz")
month_num <- substr(PM082019$labels$title, 6, 7)
year <- substr(PM082019$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM082019 <- PM082019 + ggtitle(new_title)
ggsave((filename = "PM082019_plot.jpeg"), plot = PM082019, width = 6.5, height = 6.5, dpi = 300)
print(PM082019)

PM092019<-Pollmap("TRX01_2019_09.csv.gz")
month_num <- substr(PM092019$labels$title, 6, 7)
year <- substr(PM092019$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM092019 <- PM092019 + ggtitle(new_title)
ggsave((filename = "PM092019_plot.jpeg"), plot = PM092019, width = 6.5, height = 6.5, dpi = 300)
print(PM092019)

PM102019<-Pollmap("TRX01_2019_10.csv.gz")
month_num <- substr(PM102019$labels$title, 6, 7)
year <- substr(PM102019$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM102019 <- PM102019 + ggtitle(new_title)
ggsave((filename = "PM102019_plot.jpeg"), plot = PM102019, width = 6.5, height = 6.5, dpi = 300)
print(PM102019)

PM112019<-Pollmap("TRX01_2019_11.csv.gz")
month_num <- substr(PM112019$labels$title, 6, 7)
year <- substr(PM112019$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM112019 <- PM112019 + ggtitle(new_title)
ggsave((filename = "PM112019_plot.jpeg"), plot = PM112019, width = 6.5, height = 6.5, dpi = 300)
print(PM112019)

PM122019<-Pollmap("TRX01_2019_12.csv.gz")
month_num <- substr(PM122019$labels$title, 6, 7)
year <- substr(PM122019$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM122019 <- PM122019 + ggtitle(new_title)
ggsave((filename = "PM122019_plot.jpeg"), plot = PM122019, width = 6.5, height = 6.5, dpi = 300)
print(PM122019)

PM012020<-Pollmap("TRX01_2020_01.csv.gz")
month_num <- substr(PM012020$labels$title, 6, 7)
year <- substr(PM012020$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM012020 <- PM012020 + ggtitle(new_title)
ggsave((filename = "PM012020_plot.jpeg"), plot = PM012020, width = 6.5, height = 6.5, dpi = 300)
print(PM012020)

PM022020<-Pollmap("TRX01_2020_02.csv.gz")
month_num <- substr(PM022020$labels$title, 6, 7)
year <- substr(PM022020$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM022020 <- PM022020 + ggtitle(new_title)
ggsave((filename = "PM022020_plot.jpeg"), plot = PM022020, width = 6.5, height = 6.5, dpi = 300)
print(PM022020)

PM032020<-Pollmap("TRX01_2020_03.csv.gz")
month_num <- substr(PM032020$labels$title, 6, 7)
year <- substr(PM032020$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM032020 <- PM032020 + ggtitle(new_title)
ggsave((filename = "PM032020_plot.jpeg"), plot = PM032020, width = 6.5, height = 6.5, dpi = 300)
print(PM032020)

PM042020<-Pollmap("TRX01_2020_04.csv.gz")
month_num <- substr(PM042020$labels$title, 6, 7)
year <- substr(PM042020$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM042020 <- PM042020 + ggtitle(new_title)
ggsave((filename = "PM042020_plot.jpeg"), plot = PM042020, width = 6.5, height = 6.5, dpi = 300)
print(PM042020)

PM052020<-Pollmap("TRX01_2020_05.csv.gz")
month_num <- substr(PM052020$labels$title, 6, 7)
year <- substr(PM052020$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM052020 <- PM052020 + ggtitle(new_title)
ggsave((filename = "PM052020_plot.jpeg"), plot = PM052020, width = 6.5, height = 6.5, dpi = 300)
print(PM052020)

PM062020<-Pollmap("TRX01_2020_06.csv.gz")
month_num <- substr(PM062020$labels$title, 6, 7)
year <- substr(PM062020$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM062020 <- PM062020 + ggtitle(new_title)
ggsave((filename = "PM062020_plot.jpeg"), plot = PM062020, width = 6.5, height = 6.5, dpi = 300)
print(PM062020)

PM072020<-Pollmap("TRX01_2020_07.csv.gz")
month_num <- substr(PM072020$labels$title, 6, 7)
year <- substr(PM072020$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM072020 <- PM072020 + ggtitle(new_title)
ggsave((filename = "PM072020_plot.jpeg"), plot = PM072020, width = 6.5, height = 6.5, dpi = 300)
print(PM072020)

PM082020<-Pollmap("TRX01_2020_08.csv.gz")
month_num <- substr(PM082020$labels$title, 6, 7)
year <- substr(PM082020$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM082020 <- PM082020 + ggtitle(new_title)
ggsave((filename = "PM082020_plot.jpeg"), plot = PM082020, width = 6.5, height = 6.5, dpi = 300)
print(PM082020)

PM092020<-Pollmap("TRX01_2020_09.csv.gz")
month_num <- substr(PM092020$labels$title, 6, 7)
year <- substr(PM092020$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM092020 <- PM092020 + ggtitle(new_title)
ggsave((filename = "PM092020_plot.jpeg"), plot = PM092020, width = 6.5, height = 6.5, dpi = 300)
print(PM092020)

PM102020<-Pollmap("TRX01_2020_10.csv.gz")
month_num <- substr(PM102020$labels$title, 6, 7)
year <- substr(PM102020$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM102020 <- PM102020 + ggtitle(new_title)
ggsave((filename = "PM102020_plot.jpeg"), plot = PM102020, width = 6.5, height = 6.5, dpi = 300)
print(PM102020)

PM112020<-Pollmap("TRX01_2020_11.csv.gz")
month_num <- substr(PM112020$labels$title, 6, 7)
year <- substr(PM112020$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM112020 <- PM112020 + ggtitle(new_title)
ggsave((filename = "PM112020_plot.jpeg"), plot = PM112020, width = 6.5, height = 6.5, dpi = 300)
print(PM112020)

PM122020<-Pollmap("TRX01_2020_12.csv.gz")
month_num <- substr(PM122020$labels$title, 6, 7)
year <- substr(PM122020$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM122020<- PM122020 + ggtitle(new_title)
ggsave((filename = "PM122020_plot.jpeg"), plot = PM122020, width = 6.5, height = 6.5, dpi = 300)
print(PM122020)

PM012021<-Pollmap("TRX01_2021_01.csv.gz")
month_num <- substr(PM012021$labels$title, 6, 7)
year <- substr(PM012021$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM012021<- PM012021 + ggtitle(new_title)
ggsave((filename = "PM012021_plot.jpeg"), plot = PM012021, width = 6.5, height = 6.5, dpi = 300)
print(PM012021)

PM022021<-Pollmap("TRX01_2021_02.csv.gz")
month_num <- substr(PM022021$labels$title, 6, 7)
year <- substr(PM022021$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM022021 <- PM022021 + ggtitle(new_title)
ggsave((filename = "PM022021_plot.jpeg"), plot = PM022021, width = 6.5, height = 6.5, dpi = 300)
print(PM022021)

PM032021<-Pollmap("TRX01_2021_03.csv.gz")
month_num <- substr(PM032021$labels$title, 6, 7)
year <- substr(PM032021$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM032021 <- PM032021 + ggtitle(new_title)
ggsave((filename = "PM032021_plot.jpeg"), plot = PM032021, width = 6.5, height = 6.5, dpi = 300)
print(PM032021)

PM042021<-Pollmap("TRX01_2021_04.csv.gz")
month_num <- substr(PM042021$labels$title, 6, 7)
year <- substr(PM042021$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM042021 <- PM042021 + ggtitle(new_title)
ggsave((filename = "PM042021_plot.jpeg"), plot = PM042021, width = 6.5, height = 6.5, dpi = 300)
print(PM042021)

PM052021<-Pollmap("TRX01_2021_05.csv.gz")
month_num <- substr(PM052021$labels$title, 6, 7)
year <- substr(PM052021$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM052021 <- PM052021 + ggtitle(new_title)
ggsave((filename = "PM052021_plot.jpeg"), plot = PM052021, width = 6.5, height = 6.5, dpi = 300)
print(PM052021)

PM062021<-Pollmap("TRX01_2021_06.csv.gz")
month_num <- substr(PM062021$labels$title, 6, 7)
year <- substr(PM062021$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM062021 <- PM062021 + ggtitle(new_title)
ggsave((filename = "PM062021_plot.jpeg"), plot = PM062021, width = 6.5, height = 6.5, dpi = 300)
print(PM062021)

PM072021<-Pollmap("TRX01_2021_07.csv.gz")
month_num <- substr(PM072021$labels$title, 6, 7)
year <- substr(PM072021$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM072021 <- PM072021 + ggtitle(new_title)
ggsave((filename = "PM072021_plot.jpeg"), plot = PM072021, width = 6.5, height = 6.5, dpi = 300)
print(PM072021)

PM082021<-Pollmap("TRX01_2021_08.csv.gz")
month_num <- substr(PM082021$labels$title, 6, 7)
year <- substr(PM082021$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM082021 <- PM082021 + ggtitle(new_title)
ggsave((filename = "PM082021_plot.jpeg"), plot = PM082021, width = 6.5, height = 6.5, dpi = 300)
print(PM082021)

PM092021<-Pollmap("TRX01_2021_09.csv.gz")
month_num <- substr(PM092021$labels$title, 6, 7)
year <- substr(PM092021$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM092021 <- PM092021 + ggtitle(new_title)
ggsave((filename = "PM092021_plot.jpeg"), plot = PM092021, width = 6.5, height = 6.5, dpi = 300)
print(PM092021)

PM102021<-Pollmap("TRX01_2021_10.csv.gz")
month_num <- substr(PM102021$labels$title, 6, 7)
year <- substr(PM102021$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM102021 <- PM102021 + ggtitle(new_title)
ggsave((filename = "PM102021_plot.jpeg"), plot = PM102021, width = 6.5, height = 6.5, dpi = 300)
print(PM102021)

PM112021<-Pollmap("TRX01_2021_11.csv.gz")
month_num <- substr(PM112021$labels$title, 6, 7)
year <- substr(PM112021$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM112021 <- PM112021 + ggtitle(new_title)
ggsave((filename = "PM112021_plot.jpeg"), plot = PM112021, width = 6.5, height = 6.5, dpi = 300)
print(PM112021)

PM122021<-Pollmap("TRX01_2021_12.csv.gz")
month_num <- substr(PM122021$labels$title, 6, 7)
year <- substr(PM122021$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM122021 <- PM122021 + ggtitle(new_title)
ggsave((filename = "PM122021_plot.jpeg"), plot = PM122021, width = 6.5, height = 6.5, dpi = 300)
print(PM122021)

PM012022<-Pollmap("TRX01_2021_01.csv.gz")
month_num <- substr(PM012022$labels$title, 6, 7)
year <- substr(PM012022$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM012022 <- PM012022 + ggtitle(new_title)
ggsave((filename = "PM012022_plot.jpeg"), plot = PM012022, width = 6.5, height = 6.5, dpi = 300)
print(PM012022)

PM022022<-Pollmap("TRX01_2021_02.csv.gz")
month_num <- substr(PM022022$labels$title, 6, 7)
year <- substr(PM022022$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM022022 <- PM022022 + ggtitle(new_title)
ggsave((filename = "PM022022_plot.jpeg"), plot = PM022022, width = 6.5, height = 6.5, dpi = 300)
print(PM022022)

PM032022<-Pollmap("TRX01_2021_03.csv.gz")
month_num <- substr(PM032022$labels$title, 6, 7)
year <- substr(PM032022$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM032022 <- PM032022+ ggtitle(new_title)
ggsave((filename = "PM032022_plot.jpeg"), plot = PM032022, width = 6.5, height = 6.5, dpi = 300)
print(PM032022)

PM042022<-Pollmap("TRX01_2021_04.csv.gz")
month_num <- substr(PM042022$labels$title, 6, 7)
year <- substr(PM042022$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM042022 <- PM042022 + ggtitle(new_title)
ggsave((filename = "PM042022_plot.jpeg"), plot = PM042022, width = 6.5, height = 6.5, dpi = 300)
print(PM042022)

PM052022<-Pollmap("TRX01_2021_05.csv.gz")
month_num <- substr(PM052022$labels$title, 6, 7)
year <- substr(PM052022$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM052022 <- PM052022 + ggtitle(new_title)
ggsave((filename = "PM052022_plot.jpeg"), plot = PM052022, width = 6.5, height = 6.5, dpi = 300)
print(PM052022)

PM062022<-Pollmap("TRX01_2021_06.csv.gz")
month_num <- substr(PM062022$labels$title, 6, 7)
year <- substr(PM062022$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM062022 <- PM062022 + ggtitle(new_title)
ggsave((filename = "PM062022_plot.jpeg"), plot = PM062022, width = 6.5, height = 6.5, dpi = 300)
print(PM062022)

PM072022<-Pollmap("TRX01_2021_07.csv.gz")
month_num <- substr(PM072022$labels$title, 6, 7)
year <- substr(PM072022$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM072022 <- PM072022 + ggtitle(new_title)
ggsave((filename = "PM072022_plot.jpeg"), plot = PM072022, width = 6.5, height = 6.5, dpi = 300)
print(PM072022)

PM082022<-Pollmap("TRX01_2021_08.csv.gz")
month_num <- substr(PM082022$labels$title, 6, 7)
year <- substr(PM082022$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM082022 <- PM082022 + ggtitle(new_title)
ggsave((filename = "PM082022_plot.jpeg"), plot = PM082022, width = 6.5, height = 6.5, dpi = 300)
print(PM082022)

PM092022<-Pollmap("TRX01_2021_09.csv.gz")
month_num <- substr(PM092022$labels$title, 6, 7)
year <- substr(PM092022$labels$title, 1, 4)
month_name <- month.name[as.integer(month_num)]
new_title <- paste("SLC", month_name, year, "Average PM2.5 Level")
PM092022 <- PM092022 + ggtitle(new_title)
ggsave((filename = "PM092022_plot.jpeg"), plot = PM092022, width = 6.5, height = 6.5, dpi = 300)
print(PM092022)


#Ex: Pollmap(PM112018)

files <- c(
  "TRX01_2018_11.csv.gz", "TRX01_2018_12.csv.gz", "TRX01_2019_01.csv.gz",
  "TRX01_2019_02.csv.gz", "TRX01_2019_03.csv.gz", "TRX01_2019_04.csv.gz",
  "TRX01_2019_05.csv.gz", "TRX01_2019_06.csv.gz", "TRX01_2019_07.csv.gz",
  "TRX01_2019_08.csv.gz", "TRX01_2019_09.csv.gz", "TRX01_2019_10.csv.gz",
  "TRX01_2019_11.csv.gz", "TRX01_2019_12.csv.gz", "TRX01_2020_01.csv.gz",
  "TRX01_2020_02.csv.gz", "TRX01_2020_03.csv.gz", "TRX01_2020_04.csv.gz",
  "TRX01_2020_05.csv.gz", "TRX01_2020_06.csv.gz", "TRX01_2020_07.csv.gz",
  "TRX01_2020_08.csv.gz", "TRX01_2020_09.csv.gz", "TRX01_2020_10.csv.gz",
  "TRX01_2020_11.csv.gz", "TRX01_2020_12.csv.gz", "TRX01_2021_01.csv.gz",
  "TRX01_2021_02.csv.gz", "TRX01_2021_03.csv.gz", "TRX01_2019_04.csv.gz",
  "TRX01_2021_05.csv.gz", "TRX01_2021_06.csv.gz", "TRX01_2021_07.csv.gz",
  "TRX01_2021_08.csv.gz", "TRX01_2021_09.csv.gz")


install.packages(c("shinyFiles", "slickR", "shinydashboard", "shinyjs"))
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFiles)
library(slickR)

#creating a slider function for the pollution levels
ui <- fluidPage(
  titlePanel("SCL, UT PM2.5 Pollution Monthly Levels"),
  sliderInput("imageSlider", "Select Image:", min = 1, max = 47, value = 1),
  imageOutput("displayImage")
)


library(dplyr)
# Define the server (FOR THE RED AND BLUE GRAPHS)
server <- function(input, output) {
  # Function to get the file path of the image based on the slider value
  getImagePath <- function(sliderValue) {
    switch(sliderValue,
           "11/18" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM112018_plot.jpeg",
           "12/18" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM122018_plot.jpeg",
           "01/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM012019_plot.jpeg",
           "02/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM022019_plot.jpeg",
           "03/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM032019_plot.jpeg",
           "04/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM042019_plot.jpeg",
           "05/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM052019_plot.jpeg",
           "06/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM062019_plot.jpeg",
           "07/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM072019_plot.jpeg",
           "08/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM082019_plot.jpeg",
           "09/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM092019_plot.jpeg",
           "10/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM102019_plot.jpeg",
           "11/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM112019_plot.jpeg",
           "12/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM122019_plot.jpeg",
           "01/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM012020_plot.jpeg",
           "02/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM022020_plot.jpeg",
           "03/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM032020_plot.jpeg",
           "04/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM042020_plot.jpeg",
           "05/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM052020_plot.jpeg",
           "06/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM062020_plot.jpeg",
           "07/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM072020_plot.jpeg",
           "08/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM082020_plot.jpeg",
           "09/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM092020_plot.jpeg",
           "10/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM102020_plot.jpeg",
           "11/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM112020_plot.jpeg",
           "12/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM122020_plot.jpeg",
           "01/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM012021_plot.jpeg",
           "02/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM022021_plot.jpeg",
           "03/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM032021_plot.jpeg",
           "04/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM042021_plot.jpeg",
           "05/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM052021_plot.jpeg",
           "06/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM062021_plot.jpeg",
           "07/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM072021_plot.jpeg",
           "08/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM082021_plot.jpeg",
           "09/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM092021_plot.jpeg",
           "10/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM102021_plot.jpeg",
           "11/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM112021_plot.jpeg",
           "12/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM122021_plot.jpeg",
           "01/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM012022_plot.jpeg",
           "02/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM022022_plot.jpeg",
           "03/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM032022_plot.jpeg",
           "04/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM042022_plot.jpeg",
           "05/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM052022_plot.jpeg",
           "06/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM062022_plot.jpeg",
           "07/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM072022_plot.jpeg",
           "08/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM082022_plot.jpeg",
           "09/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/PM092022_plot.jpeg",)
  }
  
  # Render the image based on the slider value
  output$displayImage <- renderImage({
    sliderValue <- input$imageSlider
    imagePath <- getImagePath(sliderValue)
    list(src = imagePath, alt = paste("Image", sliderValue))
  }, deleteFile = FALSE)
}

# Run the Shiny app
shinyApp(ui = ui, server = server)




library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFiles)
library(slickR)

#creating a slider function for the pollution levels
ui <- fluidPage(
  titlePanel("SCL, UT PM2.5 Pollution Monthly Levels"),
  sliderInput("imageSlider", "Select Image:", min = 1, max = 47, value = 1),
  imageOutput("displayImage")
)

# Define the server (FOR THE BLUE AND DARK BLUE GRAPHS)
server <- function(input, output) {
  # Function to get the file path of the image based on the slider value
  getImagePath <- function(sliderValue) {
    switch(sliderValue,
           "11/18" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM112018_plot.jpeg",
           "12/18" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM122018_plot.jpeg",
           "01/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM012019_plot.jpeg",
           "02/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM022019_plot.jpeg",
           "03/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM032019_plot.jpeg",
           "04/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM042019_plot.jpeg",
           "05/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM052019_plot.jpeg",
           "06/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM062019_plot.jpeg",
           "07/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM072019_plot.jpeg",
           "08/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM082019_plot.jpeg",
           "09/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM092019_plot.jpeg",
           "10/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM102019_plot.jpeg",
           "11/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM112019_plot.jpeg",
           "12/19" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM122019_plot.jpeg",
           "01/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM012020_plot.jpeg",
           "02/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM022020_plot.jpeg",
           "03/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM032020_plot.jpeg",
           "04/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM042020_plot.jpeg",
           "05/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM052020_plot.jpeg",
           "06/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM062020_plot.jpeg",
           "07/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM072020_plot.jpeg",
           "08/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM082020_plot.jpeg",
           "09/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM092020_plot.jpeg",
           "10/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM102020_plot.jpeg",
           "11/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM112020_plot.jpeg",
           "12/20" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM122020_plot.jpeg",
           "01/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM012021_plot.jpeg",
           "02/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM022021_plot.jpeg",
           "03/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM032021_plot.jpeg",
           "04/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM042021_plot.jpeg",
           "05/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM052021_plot.jpeg",
           "06/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM062021_plot.jpeg",
           "07/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM072021_plot.jpeg",
           "08/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM082021_plot.jpeg",
           "09/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM092021_plot.jpeg",
           "10/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM102021_plot.jpeg",
           "11/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM112021_plot.jpeg",
           "12/21" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM122021_plot.jpeg",
           "01/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM012022_plot.jpeg",
           "02/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM022022_plot.jpeg",
           "03/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM032022_plot.jpeg",
           "04/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM042022_plot.jpeg",
           "05/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM052022_plot.jpeg",
           "06/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM062022_plot.jpeg",
           "07/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM072022_plot.jpeg",
           "08/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM082022_plot.jpeg",
           "09/22" = "C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Red + Blue Pictures/PM092022_plot.jpeg",)
  }
  
  # Render the image based on the slider value
  output$displayImage <- renderImage({
    sliderValue <- input$imageSlider
    imagePath <- getImagePath(sliderValue)
    list(src = imagePath, alt = paste("Image", sliderValue))
  }, deleteFile = FALSE)
}

# Run the Shiny app
shinyApp(ui = ui, server = server)




PollmapC <- function(filename) {
  
  pollution <- read.csv(gzfile(paste0("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/", filename)), skip=1)
  
  names(pollution) <- names(read.csv(gzfile(paste0("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/", filename))))
  
  poll <- pollution[pollution$ES642_PM2.5_Concentration != -9999 &
                      
                      pollution$Latitude != -9999 &
                      
                      pollution$Longitude != -9999,
                    
                    c("Latitude","Longitude","ES642_PM2.5_Concentration","X2B_Ozone_Concentration")]
  poll2 <- pollution[pollution$ES642_PM2.5_Concentration != -9999 &
                       
                       pollution$X2B_Ozone_Concentration != -9999, 
                     
                     c("ES642_PM2.5_Concentration","X2B_Ozone_Concentration")]
  corrl<-cor(poll2$ES642_PM2.5_Concentration,poll2$X2B_Ozone_Concentration, "pairwise.complete.obs")
  print(corrl)
  poll <- poll %>% mutate(LatLon=paste(round(Latitude,3),round(Longitude,3),sep=":")) %>%
    
    select(LatLon, ES642_PM2.5_Concentration)
  
  pollsum <- poll %>% group_by(LatLon) %>% summarize(meanPM2.5=mean(ES642_PM2.5_Concentration),
                                                     maxPM2.5=max(ES642_PM2.5_Concentration),.groups="keep")
  
  pollsum <- data.frame(pollsum)
  
  pollsum$Latitude <- as.numeric(matrix(unlist(strsplit(pollsum$LatLon,":")),ncol=2, byrow=TRUE)[,1])
  
  pollsum$Longitude <- as.numeric(matrix(unlist(strsplit(pollsum$LatLon,":")),ncol=2, byrow=TRUE)[,2])
  
  PMname <- substr(filename, 7, 14)
  
  library(ggplot2)
  pplot <- ggplot(data = pollsum) +
    geom_point(mapping = aes(y = Latitude, x = Longitude, color = maxPM2.5)) +
    scale_color_gradient(limits = c(0, 80)) +
    ggtitle(PMname)  # Provide the title here
  
  print(pplot)
  return()
  
  # Save the plot to the specified folder
  folder_path <- "C:/Users/GRuss/Downloads/Pictures of averages"
  download_path <- file.path(folder_path, paste0(PMname, "_plot.png"))
  ggsave(download_path, pplot)
  
  return(download_path)
}

#calculating the correlation of PM2.5 and Ozone
PM112018C<-PollmapC("TRX01_2018_11.csv.gz")
PM122018C<-PollmapC("TRX01_2018_12.csv.gz")
PM012019C<-PollmapC("TRX01_2019_01.csv.gz")
PM022019C<-PollmapC("TRX01_2019_02.csv.gz")
PM032019C<-PollmapC("TRX01_2019_03.csv.gz")
PM042019C<-PollmapC("TRX01_2019_04.csv.gz")
PM052019C<-PollmapC("TRX01_2019_05.csv.gz")
PM062019C<-PollmapC("TRX01_2019_06.csv.gz")
PM072019C<-PollmapC("TRX01_2019_07.csv.gz")
PM082019C<-PollmapC("TRX01_2019_08.csv.gz")
PM092019C<-PollmapC("TRX01_2019_09.csv.gz")
PM102019C<-PollmapC("TRX01_2019_10.csv.gz")
PM112019C<-PollmapC("TRX01_2019_11.csv.gz")
PM122019C<-PollmapC("TRX01_2019_12.csv.gz")
PM012020C<-PollmapC("TRX01_2020_01.csv.gz")
PM022020C<-PollmapC("TRX01_2020_02.csv.gz")
PM032020C<-PollmapC("TRX01_2020_03.csv.gz")
PM042020C<-PollmapC("TRX01_2020_04.csv.gz")
PM052020C<-PollmapC("TRX01_2020_05.csv.gz")
PM062020C<-PollmapC("TRX01_2020_06.csv.gz")
PM072020C<-PollmapC("TRX01_2020_07.csv.gz")
PM082020C<-PollmapC("TRX01_2020_08.csv.gz")
PM092020C<-PollmapC("TRX01_2020_09.csv.gz")
PM102020C<-PollmapC("TRX01_2020_10.csv.gz")
PM112020C<-PollmapC("TRX01_2020_11.csv.gz")
PM122020C<-PollmapC("TRX01_2020_12.csv.gz")
PM012021C<-PollmapC("TRX01_2021_01.csv.gz")
PM022021C<-PollmapC("TRX01_2021_02.csv.gz")
PM032021C<-PollmapC("TRX01_2021_03.csv.gz")
PM042021C<-PollmapC("TRX01_2021_04.csv.gz")
PM052021C<-PollmapC("TRX01_2021_05.csv.gz")
PM062021C<-PollmapC("TRX01_2021_06.csv.gz")
PM072021C<-PollmapC("TRX01_2021_07.csv.gz")
PM082021C<-PollmapC("TRX01_2021_08.csv.gz")
PM092021C<-PollmapC("TRX01_2021_09.csv.gz")
PM102021C<-PollmapC("TRX01_2021_10.csv.gz")
PM112021C<-PollmapC("TRX01_2021_11.csv.gz")
PM122021C<-PollmapC("TRX01_2021_12.csv.gz")
PM012022C<-PollmapC("TRX01_2022_01.csv.gz")
PM022022C<-PollmapC("TRX01_2022_02.csv.gz")
PM032022C<-PollmapC("TRX01_2022_03.csv.gz")
PM042022C<-PollmapC("TRX01_2022_04.csv.gz") 
PM052022C<-PollmapC("TRX01_2022_05.csv.gz") 
PM062022C<-PollmapC("TRX02_2022_06.csv.gz") #corrupt file for TRAX01, had to use TRAX02
PM072022C<-PollmapC("TRX02_2022_07.csv.gz") #corrupt file for TRAX01, had to use TRAX02 
PM082022C<-PollmapC("TRX01_2022_08.csv.gz")
PM092022C<-PollmapC("TRX01_2022_09.csv.gz")

#manually entering the correlations
PM112018C = -0.488876,
PM122018C = -0.227631
PM012019C = -0.2749067
PM022019C = -0.5652333
PM032019C = -0.3798984
PM042019C = -0.3318625
PM052019C = NA
PM062019C = -0.1535508
PM072019C = -0.2731573
PM082019C = -0.3841777
PM092019C = -0.1405332
PM102019C = -0.1988919
PM112019C = -0.4307472
PM122019C = -0.4464125
PM012020C = -0.441074
PM022020C = -0.4143102
PM032020C = -0.3686205
PM042020C = -0.2370485
PM052020C = -0.1819816
PM062020C = -0.1286002
PM072020C = -0.2242019
PM082020C = 0.052898
PM092020C = 0.01116856
PM102020C = -0.4703165
PM112020C = -0.4511208
PM122020C = -0.4434968
PM012021C = -0.4456099
PM022021C = -0.4751768
PM032021C = -0.34072
PM042021C = -0.2058999
PM052021C = -0.0924246
PM062021C = -0.0813013
PM072021C = -0.0733656
PM082021C = 0.0159908
PM092021C = -0.0694595
PM102021C = -0.3697021
PM112021C = -0.3601036
PM122021C = -0.720934
PM012022C = -0.3822444
PM022022C = -0.4303925
PM032022C = -0.4197076
PM042022C = NA 
PM052022C = -0.0003727
PM062022C = -0.1552753
PM072022C = -0.0479803
PM082022C = 0.01360179
PM092022C = 0.216324

#assigning a common group for further calculations
scores <- c(
  -0.4888766, -0.227631, -0.2749067, -0.5652333, -0.3798984, -0.3318625, NA, -0.1535508, -0.2731573, -0.3841777, 
  -0.1405332, -0.1988919, -0.4307472, -0.4464125, -0.441074, -0.4143102, -0.3686205, -0.2370485, -0.1819816, -0.1286002, 
  -0.2242019, 0.052898, 0.01116856, -0.4703165, -0.4511208, -0.4434968, -0.4456099, -0.4751768, -0.34072, -0.2058999, 
  -0.09242455, -0.0813013, -0.07336558, 0.0159908, -0.06945951, -0.3697021, -0.3601036, -0.720934, -0.3822444, -0.4303925, 
  -0.4197076, NA, -0.0003726658, -0.1552753, -0.04798032, 0.01360179, 0.216324
)

# Calculating the average
average_score <- mean(scores, na.rm = TRUE)
print(average_score)









str(pollution18$ES642_PM2.5_Concentration) #confirming this data set should not be used

#Jan. 2019 data
pollution1901 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Hawthorn files/2019/HAWTH_2019_01.csv.gz"), header = T)
View(pollution1901) #seeing I need to remove the first row to perform calaculations
pollution1901 <- pollution1901[-1,]
pollution1901 <- as.numeric(pollution1901$ES642_PM2.5_Concentration)
pollution1901 <- pollution1901[pollution1901 >= 0]
summary(pollution1901)
pollution1901 <- data.frame(ES642_PM2.5_Concentration = pollution1901)
avg1901 <- mean(pollution1901$ES642_PM2.5_Concentration,na.rm=TRUE)
cor1901 <- cor(pollution1901$ES642_PM2.5_Concentration,pollution1901$X2B_Ozone_Concentration, rm.na=T)

#Jan. 2019 ANOVA configuration
pollution1901 <- pollution1901[pollution1901 >= 0]
pollution1901$ES642_PM2.5_Concentration <- data.frame(pollution1901$ES642_PM2.5_Concentration)
pollution1901$ES642_PM2.5_Concentration <- as.numeric(pollution1901$ES642_PM2.5_Concentration)
pollution1901$X2B_Ozone_Concentration <- as.numeric(pollution1901$X2B_Ozone_Concentration)
poll1901<- aov(formula=ES642_PM2.5_Concentration ~ X2B_Ozone_Concentration, data=pollution1901,na.rm=T) #the residuals for the sum of squares is extremely high, thus unexplained variation

cor1901 <- cor(pollution1901$ES642_PM2.5_Concentration,pollution1901$X2B_Ozone_Concentration, rm.na=T) #result of NA
pollution1901$X2B_Ozone_Concentration <- as.numeric(pollution1901$X2B_Ozone_Concentration)
pollution1901$ES642_PM2.5_Concentration <- as.numeric(pollution1901$ES642_PM2.5_Concentration)
poll1901aov <- aov(formula = ES642_PM2.5_Concentration ~  X2B_Ozone_Concentration, data = pollution1901)
poll1901aov <- aov(formula =X2B_Ozone_Concentration  ~ X2B_Internal_Air_Temperature + ES642_PM2.5_Concentration, data = pollution1901) #[ozone] (ozone concentration) is corrlating 
summary(poll1901aov) #ozone concentration and internal air temperature have very significant correlation

pollution1901$dataframe1$ES642_PM2.5_Concentration <- as.numeric(pollution1901$dataframe1$ES642_PM2.5_Concentration)
pollution1901$dataframe1$X2B_Internal_Air_Temperature <- as.numeric(pollution1901$dataframe1$X2B_Internal_Air_Temperature)
pollution1901$dataframe1$X2B_Ozone_Concentration <- as.numeric(pollution1901$dataframe1$X2B_Ozone_Concentration)
pollution1901$dataframe1$Train_Top_Temperature <- as.numeric(pollution1901$dataframe1$Train_Top_Temperature)
na.omit(pollution1901)
pollution1901 <- as.numeric(pollution1901)
correlations <- cor(pollution1901[c('ES642_PM2.5_Concentration', 'X2B_Internal_Air_Temperature','X2B_Ozone_Concentration','Train_Top_Temperature')])
#the correlations show that none of these factors are influential to the PM2.5 concentration measured.

poll1901 <- aov(formula = ES642_PM2.5_Concentration ~ X2B_Ozone_Concentration, data = pollution1901, na.rm = TRUE)


# mean computation- reload pollution1901 from datasource first (top line below Jan.2019 data)
mean_ozone <- mean(pollution1901$X2B_Ozone_Concentration, na.rm = TRUE) #calculation of mean ozone for Jan. '19
pollution1901$X2B_Ozone_Concentration[is.na(pollution1901$X2B_Ozone_Concentration)] <- mean_ozone 
pollution1901$X2B_Ozone_Concentration <- as.numeric(pollution1901$X2B_Ozone_Concentration)
pollution1901 <- pollution1901[pollution1901$X2B_Ozone_Concentration >= 0, ] #fixing data table to be only positive values for ozone
summary(pollution1901$X2B_Ozone_Concentration)
table(pollution1901) #unable to make a table with 2^31 elements
PollmapC(pollution1901)

#Feb. 2019 data
pollution1902 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2018 and 2019/TRX01_2019_02.csv.gz"))
View(pollution1902) #same removal as previously
pollution1902 <- pollution1902[-1,]
pollution1902 <- as.numeric(pollution1902$ES642_PM2.5_Concentration)
pollution1902 <- pollution1902[pollution1902 >= 0]
summary(pollution1902)
pollution1902 <- data.frame(ES642_PM2.5_Concentration = pollution1902)
avg1902 <- mean(pollution1902$ES642_PM2.5_Concentration,na.rm=TRUE)
cor1902 <- cor(pollution1902$ES642_PM2.5_Concentration,pollution1902$X2B_Ozone_Concentration, rm.na=T)

#Mar. 2019 data
pollution1903 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2018 and 2019/TRX01_2019_03.csv.gz"))
View(pollution1903)#same as previous
pollution1903 <- pollution1903[-1,]
pollution1903 <- as.numeric(pollution1903$ES642_PM2.5_Concentration)
pollution1903 <- pollution1903[pollution1903 >= 0]
summary(pollution1903)
pollution1903 <- data.frame(ES642_PM2.5_Concentration = pollution1903)
avg1903 <- mean(pollution1903$ES642_PM2.5_Concentration,na.rm=TRUE)
cor1903 <- cor(pollution1903$ES642_PM2.5_Concentration,pollution1903$X2B_Ozone_Concentration, rm.na=T) 

#April 2019 data
pollution1904 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2018 and 2019/TRX01_2019_04.csv.gz"))
View(pollution1904) #same as previous
pollution1904 <- pollution1904[-1,]
pollution1904 <- as.numeric(pollution1904$ES642_PM2.5_Concentration)
pollution1904 <- pollution1904[pollution1904 >= 0]
summary(pollution1904)
pollution1904 <- data.frame(ES642_PM2.5_Concentration = pollution1904)
avg1904 <- mean(pollution1904$ES642_PM2.5_Concentration,na.rm=TRUE)
cor1904 <- cor(pollution19041$ES642_PM2.5_Concentration,pollution1904$X2B_Ozone_Concentration, rm.na=T)

#May 2019 data
pollution1905 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2018 and 2019/TRX01_2019_05.csv.gz"))
View(pollution1905) #same as previous
pollution1905 <- pollution1905[-1,]
pollution1905 <- as.numeric(pollution1905$ES642_PM2.5_Concentration)
pollution1905 <- pollution1905[pollution1905 >= 0]
summary(pollution1905)
pollution1905 <- data.frame(ES642_PM2.5_Concentration = pollution1905)
avg1905 <- mean(pollution1905$ES642_PM2.5_Concentration,na.rm=TRUE)
cor1905 <- cor(pollution1905$ES642_PM2.5_Concentration,pollution1905$X2B_Ozone_Concentration, rm.na=T)

#June 2019 data
pollution1906 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2018 and 2019/TRX01_2019_06.csv.gz"))
View(pollution1906) #same as previous
pollution1906 <- pollution1906[-1,]
pollution1906 <- as.numeric(pollution1906$ES642_PM2.5_Concentration)
pollution1906 <- pollution1906[pollution1906 >= 0]
summary(pollution1906)
pollution1906 <- data.frame(ES642_PM2.5_Concentration = pollution1906)
avg1906 <- mean(pollution1906$ES642_PM2.5_Concentration, na.rm=TRUE)
cor1906 <- cor(pollution1906$ES642_PM2.5_Concentration,pollution1906$X2B_Ozone_Concentration, rm.na=T)

#July 2019 data
pollution1907 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2018 and 2019/TRX01_2019_07.csv.gz"))
View(pollution1907) #same as previous
pollution1907 <- pollution1907[-1,]
pollution1907 <- as.numeric(pollution1907$ES642_PM2.5_Concentration)
pollution1907 <- pollution1907[pollution1907 >= 0]
summary(pollution1907)
pollution1907 <- data.frame(ES642_PM2.5_Concentration = pollution1907)
avg1907 <- mean(pollution1907$ES642_PM2.5_Concentration, na.rm=TRUE)
cor1907 <- cor(pollution1907$ES642_PM2.5_Concentration,pollution1907$X2B_Ozone_Concentration, rm.na=T)

#Aug. 2019 data
pollution1908 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2018 and 2019/TRX01_2019_08.csv.gz"))
View(pollution1908) #same as previous
pollution1908 <- pollution1908[-1,]
pollution1908 <- as.numeric(pollution1908$ES642_PM2.5_Concentration)
pollution1908 <- pollution1908[pollution1908 >= 0]
summary(pollution1908)
pollution1908 <- data.frame(ES642_PM2.5_Concentration = pollution1908)
avg1908 <- mean(pollution1908$ES642_PM2.5_Concentration, na.rm=TRUE)
cor1908 <- cor(pollution1908$ES642_PM2.5_Concentration,pollution1908$X2B_Ozone_Concentration, rm.na=T)

#Sep. 2019 data
pollution1909 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2018 and 2019/TRX01_2019_09.csv.gz"))
View(pollution1909) #same as previous
pollution1909 <- pollution1909[-1,]
pollution1909 <- as.numeric(pollution1909$ES642_PM2.5_Concentration)
pollution1909 <- pollution1909[pollution1909 >= 0]
summary(pollution1909)
pollution1909 <- data.frame(ES642_PM2.5_Concentration = pollution1909)
avg1909 <- mean(pollution1909$ES642_PM2.5_Concentration, na.rm=TRUE)
cor1909 <- cor(pollution1909$ES642_PM2.5_Concentration,pollution1909$X2B_Ozone_Concentration, rm.na=T)

#Oct. 2019 data
pollution1910 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2018 and 2019/TRX01_2019_10.csv.gz"))
View(pollution1910) #same as previous
pollution1910 <- pollution1910[-1,]
pollution1910 <- as.numeric(pollution1910$ES642_PM2.5_Concentration)
pollution1910 <- pollution1910[pollution1910 != -9999.000]
summary(pollution1910)
pollution1910 <- data.frame(ES642_PM2.5_Concentration = pollution1910)
avg1910 <- mean(pollution1910$ES642_PM2.5_Concentration, na.rm=TRUE)
cor1910<- cor(pollution1910$ES642_PM2.5_Concentration,pollution1910$X2B_Ozone_Concentration, rm.na=T)

#Nov. 2019 data
pollution1911 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2018 and 2019/TRX01_2019_11.csv.gz"))
View(pollution1911) #same as previous
pollution1911 <- pollution1911[-1,]
pollution1911 <- as.numeric(pollution1911$ES642_PM2.5_Concentration)
pollution1911 <- pollution1911[pollution1911 != -9999.000]
summary(pollution1911)
pollution1911 <- data.frame(ES642_PM2.5_Concentration = pollution1911)
avg1911 <- mean(pollution1911$ES642_PM2.5_Concentration, na.rm=TRUE)
cor1911 <- cor(pollution1911$ES642_PM2.5_Concentration,pollution1911$X2B_Ozone_Concentration, rm.na=T) 

#Dec. 2019 data
pollution1912 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2018 and 2019/TRX01_2019_12.csv.gz"))
View(pollution1912) #same as previous
pollution1912 <- pollution1912[-1,]
pollution1912 <- as.numeric(pollution1912$ES642_PM2.5_Concentration)
pollution1912 <- pollution1912[pollution1912 != -9999.000]
summary(pollution1912)
pollution1912 <- data.frame(ES642_PM2.5_Concentration = pollution1912)
avg1912 <- mean(pollution1912$ES642_PM2.5_Concentration, na.rm=TRUE)
cor1912 <- cor(pollution1912$ES642_PM2.5_Concentration,pollution1912$X2B_Ozone_Concentration, rm.na=T)

#Jan. 2020 data
pollution2001 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2020/TRX01_2020_01.csv.gz"))
View(pollution2001) #same as previous
pollution2001 <- pollution2001[-1,]
pollution2001 <- as.numeric(pollution2001$ES642_PM2.5_Concentration)
pollution2001 <- pollution2001[pollution2001 != -9999.000]
summary(pollution2001)
pollution2001 <- data.frame(ES642_PM2.5_Concentration = pollution2001)
avg2001 <- mean(pollution2001$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2001 <- cor(pollution2001$ES642_PM2.5_Concentration,pollution2001$X2B_Ozone_Concentration, rm.na=T)

#Feb. 2020 data
pollution2002 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2020/TRX01_2020_02.csv.gz"))
View(pollution2002) #same as previous
pollution2002 <- pollution2002[-1,]
pollution2002 <- as.numeric(pollution2002$ES642_PM2.5_Concentration)
pollution2002 <- pollution2002[pollution2002 != -9999.000]
summary(pollution2002)
pollution2002 <- data.frame(ES642_PM2.5_Concentration = pollution2002)
avg2002 <- mean(pollution2002$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2002 <- cor(pollution2002$ES642_PM2.5_Concentration,pollution2002$X2B_Ozone_Concentration, rm.na=T)

#Mar. 2020 data
pollution2003 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2020/TRX01_2020_03.csv.gz"))
View(pollution2003) #same as previous
pollution2003 <- pollution2003[-1,]
pollution2003 <- as.numeric(pollution2003$ES642_PM2.5_Concentration)
pollution2003 <- pollution2003[pollution2003 != -9999.000]
summary(pollution2003)
pollution2003 <- data.frame(ES642_PM2.5_Concentration = pollution2003)
avg2003 <- mean(pollution2003$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2003 <- cor(pollution2003$ES642_PM2.5_Concentration,pollution2003$X2B_Ozone_Concentration, rm.na=T)

#Apr. 2020 data
pollution2004 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2020/TRX01_2020_04.csv.gz"))
View(pollution2004) #same as previous
pollution2004 <- pollution2004[-1,]
pollution2004 <- as.numeric(pollution2004$ES642_PM2.5_Concentration)
pollution2004 <- pollution2004[pollution2004 != -9999.000]
summary(pollution2004)
pollution2004 <- data.frame(ES642_PM2.5_Concentration = pollution2004)
avg2004 <- mean(pollution2004$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2004 <- cor(pollution2004$ES642_PM2.5_Concentration,pollution2004$X2B_Ozone_Concentration, rm.na=T)

#May 2020 data
pollution2005 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2020/TRX01_2020_05.csv.gz"))
View(pollution2005) #same as previous
pollution2005 <- pollution2005[-1,]
pollution2005 <- as.numeric(pollution2005$ES642_PM2.5_Concentration)
pollution2005 <- pollution2005[pollution2005 != -9999.000]
summary(pollution2005)
pollution2005 <- data.frame(ES642_PM2.5_Concentration = pollution2005)
avg2005 <- mean(pollution2005$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2005 <- cor(pollution2005$ES642_PM2.5_Concentration,pollution2005$X2B_Ozone_Concentration, rm.na=T)

#June 2020 data
pollution2006 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2020/TRX01_2020_06.csv.gz"))
View(pollution2006) #same as previous
pollution2006 <- pollution2006[-1,]
pollution2006 <- as.numeric(pollution2006$ES642_PM2.5_Concentration)
pollution2006 <- pollution2006[pollution2006 != -9999.000]
summary(pollution2006)
pollution2006 <- data.frame(ES642_PM2.5_Concentration = pollution2006)
avg2006 <- mean(pollution2006$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2006 <- cor(pollution2006$ES642_PM2.5_Concentration,pollution2006$X2B_Ozone_Concentration, rm.na=T)

#July 2020 data
pollution2007 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2020/TRX01_2020_07.csv.gz"))
View(pollution2007) #same as previous
pollution2007 <- pollution2007[-1,]
pollution2007 <- as.numeric(pollution2007$ES642_PM2.5_Concentration)
pollution2007 <- pollution2007[pollution2007 != -9999.000]
summary(pollution2007)
pollution2007 <- data.frame(ES642_PM2.5_Concentration = pollution2007)
avg2007 <- mean(pollution2007$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2007 <- cor(pollution2007$ES642_PM2.5_Concentration,pollution2007$X2B_Ozone_Concentration, rm.na=T)

#Aug. 2020 data
pollution2008 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2020/TRX01_2020_08.csv.gz"))
View(pollution2008) #same as previous
pollution2008 <- pollution2008[-1,]
pollution2008 <- as.numeric(pollution2008$ES642_PM2.5_Concentration)
pollution2008 <- pollution2008[pollution2008 != -9999.000]
summary(pollution2008)
pollution2008 <- data.frame(ES642_PM2.5_Concentration = pollution2008)
avg2008 <- mean(pollution2008$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2008 <- cor(pollution2008$ES642_PM2.5_Concentration,pollution2008$X2B_Ozone_Concentration, rm.na=T)

#Sept. 2020 data
pollution2009 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2020/TRX01_2020_09.csv.gz"))
View(pollution2009) #same as previous
pollution2009 <- pollution2009[-1,]
pollution2009 <- as.numeric(pollution2009$ES642_PM2.5_Concentration)
pollution2009 <- pollution2009[pollution2009 != -9999.000]
summary(pollution2009)
pollution2009 <- data.frame(ES642_PM2.5_Concentration = pollution2009)
avg2009 <- mean(pollution2009$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2009 <- cor(pollution2009$ES642_PM2.5_Concentration,pollution2009$X2B_Ozone_Concentration, rm.na=T)

#Oct. 2020 data
pollution2010 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2020/TRX01_2020_10.csv.gz"))
View(pollution2010) #same as previous
pollution2010 <- pollution2010[-1,]
pollution2010 <- as.numeric(pollution2010$ES642_PM2.5_Concentration)
pollution2010 <- pollution2010[pollution2010 != -9999.000]
summary(pollution2010)
pollution2010 <- data.frame(ES642_PM2.5_Concentration = pollution2010)
avg2010 <- mean(pollution2010$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2010 <- cor(pollution2010$ES642_PM2.5_Concentration,pollution2010$X2B_Ozone_Concentration, rm.na=T)

#Nov. 2020 data
pollution2011 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2020/TRX01_2020_11.csv.gz"))
View(pollution2011) #same as previous
pollution2011 <- pollution2011[-1,]
pollution2011 <- as.numeric(pollution2011$ES642_PM2.5_Concentration)
pollution2011 <- pollution2011[pollution2011 != -9999.000]
summary(pollution2011)
pollution2011 <- data.frame(ES642_PM2.5_Concentration = pollution2011)
avg2011 <- mean(pollution2011$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2011 <- cor(pollution2011$ES642_PM2.5_Concentration,pollution2011$X2B_Ozone_Concentration, rm.na=T)

#Dec. 2020 data
pollution2012 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2020/TRX01_2020_12.csv.gz"))
View(pollution2012) #same as previous
pollution2012 <- pollution2012[-1,]
pollution2012 <- as.numeric(pollution2012$ES642_PM2.5_Concentration)
pollution2012 <- pollution2012[pollution2012 != -9999.000]
summary(pollution2012)
pollution2012 <- data.frame(ES642_PM2.5_Concentration = pollution2012)
avg2012 <- mean(pollution2012$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2012 <- cor(pollution2012$ES642_PM2.5_Concentration,pollution2012$X2B_Ozone_Concentration, rm.na=T)

#Jan. 2021 data
pollution2101 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2021/TRX01_2021_01.csv.gz"))
View(pollution2101) #same as previous
pollution2101 <- pollution2101[-1,]
pollution2101 <- as.numeric(pollution2101$ES642_PM2.5_Concentration)
pollution2101 <- pollution2101[pollution2101 != -9999.000]
summary(pollution2101)
pollution2101 <- data.frame(ES642_PM2.5_Concentration = pollution2101)
avg2101 <- mean(pollution2101$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2101 <- cor(pollution2101$ES642_PM2.5_Concentration,pollution2101$X2B_Ozone_Concentration, rm.na=T)

#Feb. 2021 data
pollution2102 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2021/TRX01_2021_02.csv.gz"))
View(pollution2102) #same as previous
pollution2102 <- pollution2102[-1,]
pollution2102 <- as.numeric(pollution2102$ES642_PM2.5_Concentration)
pollution2102 <- pollution2102[pollution2102 != -9999.000]
summary(pollution2102)
pollution2102 <- data.frame(ES642_PM2.5_Concentration = pollution2102)
avg2102 <- mean(pollution2102$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2102 <- cor(pollution2102$ES642_PM2.5_Concentration,pollution2102$X2B_Ozone_Concentration, rm.na=T)

#Mar. 2021 data
pollution2103 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2021/TRX01_2021_03.csv.gz"))
View(pollution2103) #same as previous
pollution2103 <- pollution2103[-1,]
pollution2103 <- as.numeric(pollution2103$ES642_PM2.5_Concentration)
pollution2103 <- pollution2103[pollution2103 != -9999.000]
summary(pollution2103)
pollution2103 <- data.frame(ES642_PM2.5_Concentration = pollution2103)
avg2103 <- mean(pollution2103$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2103 <- cor(pollution2103$ES642_PM2.5_Concentration,pollution2103$X2B_Ozone_Concentration, rm.na=T)

#Apr. 2021 data
pollution2104 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2021/TRX01_2021_04.csv.gz"))
View(pollution2104) #same as previous
pollution2104 <- pollution2104[-1,]
pollution2104 <- as.numeric(pollution2104$ES642_PM2.5_Concentration)
pollution2104 <- pollution2104[pollution2104 != -9999.000]
summary(pollution2104)
pollution2104 <- data.frame(ES642_PM2.5_Concentration = pollution2104)
avg2104 <- mean(pollution2104$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2104 <- cor(pollution2104$ES642_PM2.5_Concentration,pollution2104$X2B_Ozone_Concentration, rm.na=T)

#May 2021 data
pollution2105 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2021/TRX01_2021_05.csv.gz"))
View(pollution2105) #same as previous
pollution2105 <- pollution2105[-1,]
pollution2105 <- as.numeric(pollution2105$ES642_PM2.5_Concentration)
pollution2105 <- pollution2105[pollution2105 != -9999.000]
summary(pollution2105)
pollution2105 <- data.frame(ES642_PM2.5_Concentration = pollution2105)
avg2105 <- mean(pollution2105$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2105 <- cor(pollution2105$ES642_PM2.5_Concentration,pollution2105$X2B_Ozone_Concentration, rm.na=T)

#June 2021 data
pollution2106 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2021/TRX01_2021_06.csv.gz"))
View(pollution2106) #same as previous
pollution2106 <- pollution2106[-1,]
pollution2106 <- as.numeric(pollution2106$ES642_PM2.5_Concentration)
pollution2106 <- pollution2106[pollution2106 != -9999.000]
summary(pollution2106)
pollution2106 <- data.frame(ES642_PM2.5_Concentration = pollution2106)
avg2106 <- mean(pollution2106$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2106 <- cor(pollution2106$ES642_PM2.5_Concentration,pollution2106$X2B_Ozone_Concentration, rm.na=T)

#July 2021 data
pollution2107 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2021/TRX01_2021_07.csv.gz"))
View(pollution2107) #same as previous
pollution2107 <- pollution2107[-1,]
pollution2107 <- as.numeric(pollution2107$ES642_PM2.5_Concentration)
pollution2107 <- pollution2107[pollution2107 != -9999.000]
summary(pollution2107)
pollution2107 <- data.frame(ES642_PM2.5_Concentration = pollution2107)
avg2107 <- mean(pollution2107$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2107 <- cor(pollution2107$ES642_PM2.5_Concentration,pollution2107$X2B_Ozone_Concentration, rm.na=T)

#Aug. 2021 data
pollution2108 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2021/TRX01_2021_08.csv.gz"))
View(pollution2108) #same as previous
pollution2108 <- pollution2108[-1,]
pollution2108 <- as.numeric(pollution2108$ES642_PM2.5_Concentration)
pollution2108 <- pollution2108[pollution2108 != -9999.000]
summary(pollution2108)
pollution2108 <- data.frame(ES642_PM2.5_Concentration = pollution2108)
avg2108 <- mean(pollution2108$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2108 <- cor(pollution2108$ES642_PM2.5_Concentration,pollution2108$X2B_Ozone_Concentration, rm.na=T)

#Sept. 2021 data
pollution2109 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2021/TRX01_2021_09.csv.gz"))
View(pollution2109) #same as previous
pollution2109 <- pollution2109[-1,]
pollution2109 <- as.numeric(pollution2109$ES642_PM2.5_Concentration)
pollution2109 <- pollution2109[pollution2109 != -9999.000]
summary(pollution2109)
pollution2109 <- data.frame(ES642_PM2.5_Concentration = pollution2109)
avg2109 <- mean(pollution2109$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2109 <- cor(pollution2109$ES642_PM2.5_Concentration,pollution2109$X2B_Ozone_Concentration, rm.na=T)

#Oct. 2021 data
pollution2110 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2021/TRX01_2021_10.csv.gz"))
View(pollution2110) #same as previous
pollution2110 <- pollution2110[-1,]
pollution2110 <- as.numeric(pollution2110$ES642_PM2.5_Concentration)
pollution2110 <- pollution2110[pollution2110 != -9999.000]
summary(pollution2110)
pollution2110 <- data.frame(ES642_PM2.5_Concentration = pollution2110)
avg2110 <- mean(pollution2110$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2110 <- cor(pollution2110$ES642_PM2.5_Concentration,pollution2110$X2B_Ozone_Concentration, rm.na=T)

#Nov. 2021 data
pollution2111 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2021/TRX01_2021_11.csv.gz"))
View(pollution2111) #same as previous
pollution2111 <- pollution2111[-1,]
pollution2111 <- as.numeric(pollution2111$ES642_PM2.5_Concentration)
pollution2111 <- pollution2111[pollution2111 != -9999.000]
summary(pollution2111)
pollution2111 <- data.frame(ES642_PM2.5_Concentration = pollution2111)
avg2111 <- mean(pollution2111$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2111 <- cor(pollution2111$ES642_PM2.5_Concentration,pollution2111$X2B_Ozone_Concentration, rm.na=T)

#Dec. 2021 data
pollution2112 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2021/TRX01_2021_12.csv.gz"))
View(pollution2112) #same as previous
pollution2112 <- pollution2112[-1,]
pollution2112 <- as.numeric(pollution2112$ES642_PM2.5_Concentration)
pollution2112 <- pollution2112[pollution2112 != -9999.000]
summary(pollution2112)
pollution2112 <- data.frame(ES642_PM2.5_Concentration = pollution2112)
avg2112 <- mean(pollution2112$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2112 <- cor(pollution2112$ES642_PM2.5_Concentration,pollution2112$X2B_Ozone_Concentration, rm.na=T)

#Jan. 2022 data
pollution2201 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2022/TRX01_2022_01.csv.gz"))
View(pollution2201) #same as previous
pollution2201 <- pollution2201[-1,]
pollution2201 <- as.numeric(pollution2201$ES642_PM2.5_Concentration)
pollution2201 <- pollution2201[pollution2201 != -9999.000]
summary(pollution2201)
pollution2201 <- data.frame(ES642_PM2.5_Concentration = pollution2201)
avg2201 <- mean(pollution2201$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2201 <- cor(pollution2201$ES642_PM2.5_Concentration,pollution2201$X2B_Ozone_Concentration, rm.na=T)

#Feb. 2022 data
pollution2202 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2022/TRX01_2022_02.csv.gz"))
View(pollution2202) #same as previous
pollution2202 <- pollution2202[-1,]
pollution2202 <- as.numeric(pollution2202$ES642_PM2.5_Concentration)
pollution2202 <- pollution2202[pollution2202 != -9999.000]
summary(pollution2202)
pollution2202 <- data.frame(ES642_PM2.5_Concentration = pollution2202)
avg2202 <- mean(pollution2202$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2202 <- cor(pollution2202$ES642_PM2.5_Concentration,pollution2202$X2B_Ozone_Concentration, rm.na=T)

#Mar. 2022 data
pollution2203 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2022/TRX01_2022_03.csv.gz"))
View(pollution2203) #same as previous
pollution2203 <- pollution2203[-1,]
pollution2203 <- as.numeric(pollution2203$ES642_PM2.5_Concentration)
pollution2203 <- pollution2203[pollution2203 != -9999.000]
summary(pollution2203)
pollution2203 <- data.frame(ES642_PM2.5_Concentration = pollution2203)
avg2203 <- mean(pollution2203$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2203 <- cor(pollution2203$ES642_PM2.5_Concentration,pollution2203$X2B_Ozone_Concentration, rm.na=T)

#Apr. 2022 data
#no data in this file, will omit from the data set
pollution2204 <- NA #need to put an NA for this data set within the graphs


#May 2022 data
pollution2205 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2022/TRX01_2022_05.csv.gz"))
View(pollution2205) #same as previous
pollution2205 <- pollution2205[-1,]
pollution2205 <- as.numeric(pollution2205$ES642_PM2.5_Concentration)
pollution2205 <- pollution2205[pollution2205 != -9999.000]
summary(pollution2205)
pollution2205 <- data.frame(ES642_PM2.5_Concentration = pollution2205)
avg2205 <- mean(pollution2205$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2205 <- cor(pollution2205$ES642_PM2.5_Concentration,pollution2205$X2B_Ozone_Concentration, rm.na=T)

#June 2022 data
#no data in this file either, will omit as well)
pollution2206 <- NA 

#July 2022 data missing (not within the system)
pollution2207 <- NA


#Aug. 2022 data
pollution2208 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2022/TRX01_2022_08.csv.gz"))
View(pollution2208) #same as previous
pollution2208 <- pollution2208[-1,]
pollution2208 <- as.numeric(pollution2208$ES642_PM2.5_Concentration)
pollution2208 <- pollution2208[pollution2208 != -9999.000]
summary(pollution2208)
pollution2208 <- data.frame(ES642_PM2.5_Concentration = pollution2208)
avg2208 <- mean(pollution2208$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2208 <- cor(pollution2208$ES642_PM2.5_Concentration,pollution2208$X2B_Ozone_Concentration, rm.na=T)

#Sept. 2022 data
pollution2209 <- read.csv(gzfile("C:/Users/GRuss/Documents/COLLEGE/The U (Grad)/5th + 6th semesters- Stats Certificate/Summer 2023- PBHLT6102/Project files/Trax pollution files/2022/TRX01_2022_09.csv.gz"))
View(pollution2209) #same as previous
pollution2209 <- pollution2209[-1,]
pollution2209 <- as.numeric(pollution2209$ES642_PM2.5_Concentration)
pollution2209 <- pollution2209[pollution2209 != -9999.000]
summary(pollution2209)
pollution2209 <- data.frame(ES642_PM2.5_Concentration = pollution2209)
avg2209 <- mean(pollution2209$ES642_PM2.5_Concentration, na.rm=TRUE)
cor2209 <- cor(pollution2209$ES642_PM2.5_Concentration,pollution2209$X2B_Ozone_Concentration, rm.na=T)

avg18 <- NA #NA - skewed value
avg1901 #12.95431
avg1902 #3.182136
avg1903 #4.597242
avg1904 #2.283061
avg1905 #2.283061
avg1906 #2.705759
avg1907 #3.515416
avg1908 #3.515416
avg1909 #3.931518
avg1910 #4.209831
avg1911 #8.654948
avg1912 #12.30095
avg2001 #5.855509
avg2002 #4.701291
avg2003 #2.338989
avg2004 #2.503082
avg2005 #2.396949
avg2006 #2.335419
avg2007 #4.673838
avg2008 #10.10941
avg2009 #12.61728
avg2010 #4.001714
avg2011 #4.901281
avg2012 #11.01936
avg2101 #8.293857
avg2102 #2.937097
avg2103 #3.654064
avg2104 #3.567193
avg2105 #3.115763
avg2106 #3.995147
avg2107 #8.90065
avg2108 #25.44662
avg2109 #7.709987
avg2110 #3.826401
avg2111 #4.653405
avg2112 #11.57151
avg2201 #16.39874
avg2202 #5.692645
avg2203 #4.913905
avg2204 <- NA #missing data
avg2205 #3.083605
avg2206 <- NA #missing data
avg2207 <- NA #missing data
avg2208 #3.473876
avg2209 #5.917205
avg2210 <- NA #missing data
avg2211 <- NA #missing data
avg2212 <- NA #missing data
library(ggplot2)
library(dplyr)
tdata<-c(avg1901,avg1902,avg1903,avg1904,avg1905,avg1906,avg1907,avg1908,avg1909,avg1910,avg1911,avg1912,avg2001,avg2002,avg2003,avg2004,avg2005,avg2006,avg2007,avg2008,avg2009,avg2010,avg2011,avg2012,avg2101,avg2102,avg2103,avg2104,avg2105,avg2106,avg2107,avg2108,avg2109,avg2110,avg2111,avg2112,avg2201,avg2202,avg2203,avg2204,avg2205,avg2206,avg2207,avg2208,avg2209,avg2210,avg2211,avg2212)
plot(tdata,pch=c(15),xlab="Months from 2019 to 2022",ylab="Average Monthly PM2.5 level (ug/m3)", main = "PM2.5 level measured by TRAX")
boxplot(tdata,xlab="Months from 2019 to 2022",ylab="Average Monthly PM2.5 level", main = "PM2.5 level measured by TRAX")
PMtimeseries <- ts(tdata)
plot(PMtimeseries,xlab="Months from 2019 to 2022",ylab="Average Monthly PM2.5 level (ug/m3)", main = "PM2.5 level measured by TRAX")
mean(c(avg1901,avg1902,avg1903,avg1904,avg1905,avg1906,avg1907,avg1908,avg1909,avg1910,avg1911,avg1912,avg2001,avg2002,avg2003,avg2004,avg2005,avg2006,avg2007,avg2008,avg2009,avg2010,avg2011,avg2012,avg2101,avg2102,avg2103,avg2104,avg2105,avg2106,avg2107,avg2108,avg2109,avg2110,avg2111,avg2112,avg2201,avg2202,avg2203,avg2204,avg2205,avg2206,avg2207,avg2208,avg2209,avg2210,avg2211,avg2212),na.rm=T)
#the average PM2.5 level was 6.166617 ug/m3

str(pollution1902)
cols_to_convert <- c(
  "Latitude", "Longitude", "Elevation", "Battery_Voltage", 
  "Train_Box_Temperature", "ES642_PM2.5_Concentration", 
  "X2B_Ozone_Concentration"
)

pollution1902[cols_to_convert] <- lapply(pollution1902[cols_to_convert], as.numeric)

cols_to_convert <- c(
  "ES642_Air_Flow_Rate", "ES642_Internal_Air_Temperature", 
  "ES642_Internal_Relative_Humidity", "ES642_Internal_Air_Pressure",
  "ES642_Error_Code", "X2B_Air_Flow_Rate", "X2B_Internal_Air_Temperature",
  "X2B_Internal_Air_Pressure", "PM2.5_Data_Flagged", "Ozone_Data_Flagged"
)

pollution1902[cols_to_convert] <- lapply(pollution1902[cols_to_convert], as.numeric)

# Check the structure again
str(pollution1902)
pollution1902$Timestamp <- as.Date(pollution1902$Timestamp)
daily_measurements <- subset(pollution1902$Timestamp, format(pollution1902$Timestamp, "%H:%M:%S") == "00:00:00")
write.csv(daily_measurements, "daily_pollution_data.csv", row.names = FALSE)

# Step 4: Filter the data to keep only daily measurements
daily_measurements <- subset(pollution_data, format(Date, "%H:%M:%S") == "00:00:00")

# Step 5: Save the filtered data if needed
write.csv(daily_measurements, "daily_pollution_data.csv", row.names = FALSE)

daily_measurements <- subset(pollution1902, format(Timestamp, "%H:%M:%S") == "00:00:00")
write.csv(daily_measurements, "daily_pollution_data.csv", row.names = FALSE)
getwd()


#installing the mapview package to display the PM2.5 pollution
install.packages("mapview")
library(mapview)
library(sf)
pollution1902 <- read.csv(gzfile("C:/Users/GRuss/Downloads/daily_pollution_data.csv"))
View(pollution1902)
pollution1902 <- pollution1902[-1,]
pollution1902 <- na.omit(pollution1902)
pmtest <- st_as_sf(pollution1902, coords = c("Longitude","Latitude"), crs = 4326)
mapview(pmtest, zcol = "ES642_PM2.5_Concentration", map.types = "OpenStreetMap",zoom = 50) 
#the map is very slow and unresponsive to movement. Can zoom in and out, but am unable to view Salt Lake City
#the processing power needed exceeds my laptop's ability

#Using maptools package
install.packages("maptools")
library(maptools)
library(sp)
library(ggplot2)

# Reading in the file and preparing spatial data
pollution1902 <- read.csv(gzfile("C:/Users/GRuss/Downloads/daily_pollution_data.csv"))
pollution1902 <- pollution1902[-1,]
pollution1902$Latitude <- as.numeric(pollution1902$Latitude)
pollution1902$Longitude <- as.numeric(pollution1902$Longitude)
coordinates <- pollution1902[, c("Longitude", "Latitude")]

spatial_points <- SpatialPointsDataFrame(
  coords = coordinates,
  data = pollution1902,
  proj4string = CRS("+proj=longlat +datum=WGS84")
)
spatial_points$ES642_PM2.5_Concentration<-as.numeric(spatial_points$ES642_PM2.5_Concentration)
# Plot using maptools
spatial_points <- spatial_points[spatial_points$ES642_PM2.5_Concentration > 0,]
plot(spatial_points, pch = 19, col = "red", cex = sqrt(spatial_points$ES642_PM2.5_Concentration) * 0.1)
spatial_points$ES642_PM2.5_Concentration <- as.numeric(spatial_points$ES642_PM2.5_Concentration)

class(spatial_points$ES642_PM2.5_Concentration)


library(maptools)
library(sp)
library(ggplot2)
#reading in the file once more for demonstration purposes and as a refresher to regain the latitude and longitudinal data, etc.
pollution1902 <- read.csv(gzfile("C:/Users/GRuss/Downloads/daily_pollution_data.csv"))
pollution1902 <- pollution1902[-1,]
pollution1902$Latitude <- as.numeric(pollution1902$Latitude)
pollution1902$Longitude <- as.numeric(pollution1902$Longitude)
coordinates <- pollution1902[, c("Longitude", "Latitude")]
spatial_points <- SpatialPointsDataFrame(
  coords = coordinates,
  data = pollution1902,
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Plot using maptools
spatial_points$ES642_PM2.5_Concentration <- as.numeric(class(spatial_points$ES642_PM2.5_Concentration))
plot(spatial_points, pch = 19, col = "red", cex = sqrt(spatial_points$ES642_PM2.5_Concentration) * 0.1)
#unfortunately, the above plot did not come up
