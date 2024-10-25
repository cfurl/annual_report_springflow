## Springflow graphic for annual report

## Caption:

# Springflow hydrographs for Comal and San Marcos springs for calendar year 2020 (USGS 08168710 and USGS 08170000).
# Plots on the right display percentile curves of average calendar year springflow for the  period of record at 
# the gauge.  2020 average springflow is marked with a dot.   

# visit https://webapps.usgs.gov/spring_calc/ for further explanation of data

#libraries
library(dataRetrieval)
library(tidyverse) # squeeze this down if you build a docker container
library(lubridate)
library(gridExtra)

# collect_discharge function. 
# this function returns mean daily usgs discharge. Use complete years only if you
# plan to feed this to 'calculate_percentile_annual_discharge()'
# data from this function are used in annual hydrograph line graphs (large panels)

collect_discharge <- function (siteNo, start.date, end.date, station.name) {

  as_tibble(readNWISdv(siteNumbers = siteNo,
                              parameterCd = "00060", # this parameter code is daily mean discharge in cfs
                              startDate = start.date,
                              endDate = end.date)) |>
  select(2:4) |>
  set_names(c("usgs_no","date","cfs")) |>
  relocate (usgs_no, .after = last_col()) |>
  mutate (site = station.name)
}

# calculate_percentile_annual_discharge () function. (use complete years only)
# this function uses collect_discharge() as input and
# returns a percentile of mean annual flow from 1:100 using quantile() method.
# data from this function create the line graphs in the percentile
# plots, small graphs on right

calculate_percentile_annual_discharge <- function (collect_discharge_output) {
  
  mean_annual_flow <- collect_discharge_output |>
    mutate(year = year(date)) |>
    group_by (year) |>
    mutate(annual_flow = mean(cfs)) |>
    filter(date==date[1]) |>
    select(3:6)
  
    as_tibble(quantile(mean_annual_flow$annual_flow, probs = seq(0.01, 1, by= .01),na.rm=TRUE)) |>
    mutate(percentile = seq(1,100,by=1)) |>
    relocate(value, .after = last_col()) |>
    set_names(c("percentile","cfs"))
}

# return_percentile_4_year () function. 
# this function uses collect_discharge() and calculate_percentile_annual_discharge() as input and
# returns a point relating mean annual discharge for 
# a year of interest to percentile of mean annual flow from 1:100 using quantile() method.
# data from this function create the point on the graph in the percentile plots, small graphs on right

return_percentile_4_year <- function(collect_discharge_output,calculate_percentile_annual_discharge_output, year_chr) {
  
  mean_annual_flow <- collect_discharge_output |>
    mutate(year = year(date)) |>
    group_by (year) |>
    mutate(annual_flow = mean(cfs)) |>
    filter(date==date[1]) |>
    select(3:6) |>
    filter (year == year_chr)
  
  calculate_percentile_annual_discharge_output[which.min(abs(calculate_percentile_annual_discharge_output$cfs-mean_annual_flow$annual_flow)),]
}

##### Run functions to create datasets ########

# Create data for Comal line graphs and current year percentile point
# Comal springflow period of record with full historical calendar years: 1928 - 2023

comal_por <- collect_discharge("08168710","1928-01-01","2023-12-31","Comal")
comal_percentile_distribution <- calculate_percentile_annual_discharge (comal_por)
comal_percentile_point <- return_percentile_4_year(comal_por,comal_percentile_distribution,"2023")

# Create data for San Marcos line graphs and current year percentile point
# San Marcos springflow of record with full historical calendar years: 1957 - 2023
san_marcos_por <- collect_discharge("08170000","1957-01-01","2023-12-31","San Marcos")
san_marcos_percentile_distribution <- calculate_percentile_annual_discharge (san_marcos_por)
san_marcos_percentile_point <- return_percentile_4_year(san_marcos_por,san_marcos_percentile_distribution,"2023")

# ggplot() theme
theme_set(theme_bw())
theme_update(axis.text.y = element_text(size = 8), axis.text.x = element_text(size = 8),strip.text.x = element_text(size = 8),axis.title = element_text(size = 8),legend.position=c(.5, .5) )

# p1 and p2 are 1-year hydrographs

p1<- ggplot(data = comal_por) +
  geom_line(mapping = aes(x = date, y = cfs),show.legend = TRUE)+
  labs(y="cubic feet per second",x=NULL)+
  scale_x_date(limits=as.Date(c("2023-01-01","2024-01-01")), breaks=as.Date(c("2023-01-01","2023-04-01","2023-07-01","2023-10-01","2024-01-01"),format="%Y-%m-%d"), date_labels = "%b %Y",expand = c(0, 0))+      
  scale_y_continuous(expand = c(0, 0), limits = c(0, 350))+
  geom_hline(yintercept=30, linetype = "solid", color='red')+
  geom_hline(yintercept=130, linetype = "dashed", color='green')+
  geom_hline(yintercept=225, linetype = "dotdash", color='blue')+
  annotate(geom="text",x=as.Date("2023-12-15",format="%Y-%m-%d"),y=55,label="Comal",size=5,hjust=1)+
  annotate(geom="text",x=as.Date("2023-1-15",format="%Y-%m-%d"),y=45,label="Minimum flow objective 30 cfs",size=3,hjust=0)+
  annotate(geom="text",x=as.Date("2023-1-07",format="%Y-%m-%d"),y=152,label="Condition M 130 cfs",size=3,hjust=0)+
  annotate(geom="text",x=as.Date("2023-12-15",format="%Y-%m-%d"),y=210,label="Long-term flow objective 225 cfs",size=3,hjust=1)+
  theme(plot.margin=unit(c(5.5,16.5,5.5,5.5),"pt"))


p2<- ggplot(data = san_marcos_por) +
  geom_line(mapping = aes(x = date, y = cfs),show.legend = FALSE)+
  labs(y="cubic feet per second",x=NULL)+
  scale_x_date(limits=as.Date(c("2023-01-01","2024-01-01")), breaks=as.Date(c("2023-01-01","2023-04-01","2023-07-01","2023-10-01","2024-01-01"),format="%Y-%m-%d"), date_labels = "%b %Y",expand = c(0, 0))+    
  scale_y_continuous(expand = c(0, 0), limits = c(0, 225))+
  geom_hline(yintercept=45, linetype = "solid", color='red')+
  geom_hline(yintercept=120, linetype = "dashed", color='green')+
  geom_hline(yintercept=140, linetype = "dotdash", color='blue')+
  annotate(geom="text",x=as.Date("2023-12-15",format="%Y-%m-%d"),y=20,label="San Marcos",size=5,hjust=1)+
  annotate(geom="text",x=as.Date("2023-1-15",format="%Y-%m-%d"),y=55,label="Minimum flow objective 45 cfs",size=3,hjust=0)+
  annotate(geom="text",x=as.Date("2023-12-15",format="%Y-%m-%d"),y=111,label="Condition M 120 cfs",size=3,hjust=1)+
  annotate(geom="text",x=as.Date("2023-12-15",format="%Y-%m-%d"),y=131,label="Long-term flow objective 140 cfs",size=3,hjust=1)+
  theme(plot.margin=unit(c(5.5,16.5,5.5,5.5),"pt"))

# p3 and p4 are percentile plots

p3<- ggplot (data=comal_percentile_distribution)+
  geom_line(mapping=aes(x=percentile,y=cfs),size=.8)+
  labs(y = "cubic feet per second", x = "Percentile",size=10)+
  geom_point(data=comal_percentile_point,mapping=aes(x=percentile,y=cfs),color="black",size=3) +
  theme(axis.title = element_text(size = 8),axis.text = element_text(size = 8))+
  scale_x_continuous(expand = c(0, 0))+
  theme(plot.margin=unit(c(5.5,7,5.5,5.5),"pt"))


p4<- ggplot (data=san_marcos_percentile_distribution)+
  geom_line(mapping=aes(x=percentile,y=cfs),size=.8)+
  labs(y = "cubic feet per second", x = "Percentile",size=10)+
  geom_point(data=san_marcos_percentile_point,mapping=aes(x=percentile,y=cfs),color="black",size=3) +
  theme(axis.title = element_text(size = 8),axis.text = element_text(size = 8))+
  scale_x_continuous(expand = c(0, 0))+
  theme(plot.margin=unit(c(5.5,7,5.5,5.5),"pt"))

# setup grid and save plot
lay<-rbind(c(1,1,1,1,3,3),
           c(1,1,1,1,3,3),
           c(1,1,1,1,3,3),
           c(2,2,2,2,4,4),
           c(2,2,2,2,4,4),
           c(2,2,2,2,4,4))

dd<-grid.arrange(p1, p2,p3, p4,layout_matrix=lay)

ggsave(filename = "annual_report_discharge.png", device = "png", path = ".\\figure", plot = dd, width = 6.5, height = 4.5, units = "in")

