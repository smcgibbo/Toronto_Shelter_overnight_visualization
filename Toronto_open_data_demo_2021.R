#Author: Sophie McGibbon-Gardner
#This is a demonstration of data visualization using Toronto open data 
#https://www.toronto.ca/city-government/data-research-maps/open-data/
#Created August 2021

library(opendatatoronto)#library can be downloaded from CRAN with install.packages("opendatatoronto")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(zoo)
library(reshape2)

#__________________________________________________________________________________
#Extracting data using suggested code from Open Data Toronto
# get package
package <- show_package("21c83b32-d5a8-4106-a54f-010dbe49f6f2")

# get all resources for this package
resources <- list_package_resources("21c83b32-d5a8-4106-a54f-010dbe49f6f2")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()
#data

#______________________________________________________________________________________
#Plot occupancy rates by SECTOR(user type) and PROGRAM_MODEL (intended use) separately

#begin by initializing lists to loop through for plotting
#choose custom colours (To generate colourblind friendly colour sets see https://colorbrewer2.org/)
sector_fill_colours = c('#7b3294','#c2a5cf','#f7f7f7','#a6dba0','#008837')
prog_mod_fill_colours = c('#fc8d62','#8da0cb')
#combine colour sets in a list
c_list <- list( sector_fill_colours,prog_mod_fill_colours)
#select x data
x_list = list(data$SECTOR, data$PROGRAM_MODEL)
#set save file name and graphing information
save_files <- list("Bed_occupancy_rate_by_user.jpeg", "Bed_occupancy_rate_by_program_model.jpeg")
title_list <- list("Bed Occupancy Rate by User Group, Jan 2021 - Aug 2021", "Bed Occupancy Rate by Model, Jan 2021 - Aug 2021")
xlabel_list <- list("User Group", "Model")

for (i in 1:length(c_list)) {
  col = c_list[[i]]
  f = save_files[[i]]
  violin_plot <- ggplot(data, aes(x = x_list[[i]], y = (OCCUPANCY_RATE_BEDS)))+ geom_violin(aes(fill = x_list[[i]]), trim = FALSE) +
    stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
                 geom = "pointrange", color = "black")+
    scale_fill_manual(values = col)+
    ggtitle(title_list[[i]]) +
    xlab(xlabel_list[[i]]) + ylab("Bed Occupancy Rate")

  p <-violin_plot+theme(
    plot.title = element_text(family = "Helvetica", color="black", size=16, face="bold"),
    axis.title.x = element_text(family = "Helvetica", color="black", size=16, face="bold"),
    axis.title.y = element_text(family = "Helvetica", color="black", size=16, face="bold"),
    axis.text.x = element_text(size=14), 
    axis.text.y = element_text( size=14),
    legend.position = "none",
    panel.background = element_rect(fill = 'white', colour = 'black'),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour= 'grey'))
  
  jpeg(file=f, width = 640, height = 420, units = "px")
  print(p)
  dev.off()
  
}

#______________________________________________________________________________________
#Plot occupancy rates by SECTOR(user type) and PROGRAM_MODEL (intended use) together

sector_fill_colours = c('#7b3294','#c2a5cf','#f7f7f7','#a6dba0','#008837')
save_file <- "Bed_occupancy_rate_by_user_and_model.jpeg"
title <- "Bed Occupancy Rate by Model and User Group, Jan 2021 - Aug 2021"
xlabel <- "Model"

violin_plot <- ggplot(data, aes(x = PROGRAM_MODEL, y = (OCCUPANCY_RATE_BEDS)))+ geom_violin(aes(fill = SECTOR), trim = FALSE) +
  #stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
  #             geom = "pointrange", color = "black")+
  scale_fill_manual(values = sector_fill_colours, name = '')+
  ggtitle(title) +
  xlab(xlabel) + ylab("Bed Occupancy Rate")

p <-violin_plot+theme(
  plot.title = element_text(family = "Helvetica", color="black", size=16, face="bold"),
  axis.title.x = element_text(family = "Helvetica", color="black", size=16, face="bold"),
  axis.title.y = element_text(family = "Helvetica", color="black", size=16, face="bold"),
  axis.text.x = element_text(size=14), 
  axis.text.y = element_text( size=14),
  panel.background = element_rect(fill = 'white', colour = 'black'),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(colour= 'grey'),
  legend.text=element_text(size=14))

jpeg(file=save_file, width = 1280, height = 420, units = "px")
print(p)
dev.off()


#______________________________________________________________________________________
#create stacked time series plot of occupancy rates by date

#data is organized as each unique location/type having its own date and occupancy rate
#this means we must decide how to represent the data. One choice (which is implemented below)
#is to average the total occupancy for each date, then show the share of occupancy by sector

#find the average of each user type group by date using aggregate

#create dataframe with only the columns we need, omit rows with NA values
data_pruned <- na.omit(select(data, OCCUPANCY_DATE, OCCUPANCY_RATE_BEDS))

#aggregate using mean of occupancy rate
time_data_ave <- setNames(aggregate(data_pruned$OCCUPANCY_RATE_BEDS, list(data_pruned$OCCUPANCY_DATE), mean), c('Date','Rate' ))

#set date column to be of type Date
time_data_ave$Date <- as.Date(time_data_ave$Date)

#add rolling mean to data, using two week average
time_data_ave$roll14 = rollmean(time_data_ave$Rate, 14, na.pad=TRUE)

time_plot <- ggplot(data=time_data_ave, aes(x=Date, y=Rate, group=1)) +
  geom_point(position=position_jitter(1,3), pch=21, fill="#1f78b4", alpha = 0.7) +
  geom_line(aes(x=Date,y=roll14), size = 1.5)+
  scale_x_date(date_breaks="months", date_labels="%b")+
  ggtitle("Average bed occupancy rate with rolling 14 day average, 2021") +
  xlab("Date") + 
  ylab("Bed Occupancy Rate")

P <- time_plot+theme(
  plot.title = element_text(family = "Helvetica", color="black", size=15, face="bold"),
  axis.title.x = element_text(family = "Helvetica", color="black", size=15, face="bold"),
  axis.title.y = element_text(family = "Helvetica", color="black", size=15, face="bold"),
  axis.text.x = element_text(size=14), 
  axis.text.y = element_text( size=14),
  panel.background = element_rect(fill = 'white', colour = 'black'),
  panel.grid.major = element_blank())


jpeg(file="Average_occupancy_rate_beds_by_date.jpeg", width = 640, height = 420, units = "px")
print(P)
dev.off()

#______________________________________________________________________________________
#Create time series plot of number of bed occupied with breakdown of bed usage by sector

#data_pruned was created above as a dataframe with columns
data_pruned <- na.omit(select(data, OCCUPANCY_DATE, SECTOR, OCCUPIED_BEDS))

data_averaged <- setNames((aggregate(data_pruned$OCCUPIED_BEDS, 
          list(data_pruned$OCCUPANCY_DATE, data_pruned$SECTOR ), mean)), c('Date',"User",'Beds' ))

#set date column to be of type Date
data_averaged$Date <- as.Date(data_averaged$Date)
time_series <- ggplot(data_averaged, aes(fill = User,
                                                x = Date,
                                                y = Beds,
                                                color = User,
                                                group = User)) +
                                                geom_area(position="stack", 
                                                  stat="identity",
                                                  alpha = 0.5)+scale_fill_manual(values = sector_fill_colours, name = '')+
                                                geom_line(position = "stack", size = 2)+scale_colour_manual(values = sector_fill_colours, name = '')+
                                                scale_x_date(date_breaks="months", date_labels="%b")+
                                                ggtitle("Average bed occupancy, 2021") +
                                                xlab("Date") + 
                                                ylab("Bed Occupancy")+
                                                labs(fill = 'User Type\n')
                                                

P <- time_series+theme(
  plot.title = element_text(family = "Helvetica", color="black", size=15, face="bold"),
  axis.title.x = element_text(family = "Helvetica", color="black", size=15, face="bold"),
  axis.title.y = element_text(family = "Helvetica", color="black", size=15, face="bold"),
  axis.text.x = element_text(size=14), 
  axis.text.y = element_text( size=14),
  panel.background = element_rect(fill = 'white', colour = 'black'),
  panel.grid.major = element_line(colour= 'grey'),
  legend.position = "bottom",
  legend.text=element_text(size=14))

jpeg(file="Average_occupancy_beds_by_date_stacked_by_user.jpeg", width = 640, height = 440, units = "px")
print(P)
dev.off()

