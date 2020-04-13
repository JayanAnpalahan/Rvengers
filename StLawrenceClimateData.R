library("dplyr")
library(ggplot2)

StLawrenceCsv1 = read.csv("Climate/St Lawrence/St 1970-1976.csv")
StLawrenceCsv1 = filter(StLawrenceCsv1, Year <= 1976)
StLawrenceCsv1 = filter(StLawrenceCsv1, Year >= 1970)
StLawrenceCsv2 = read.csv("Climate/St Lawrence/St 1976-2006.csv")
StLawrenceCsv2 = filter(StLawrenceCsv2, Year <= 2005)
StLawrenceCsv2 = filter(StLawrenceCsv2, Year >= 1977)
StLawrenceCsv = rbind(StLawrenceCsv1, StLawrenceCsv2)

write.csv(StLawrenceCsv, "Climate/St Lawrence (1970-2005).csv")


StLawrenceCsvFiltered = filter(StLawrenceCsv, Year == 1970 
                               | Year == 1975
                               | Year == 1980
                               | Year == 1985
                               | Year == 1990
                               | Year == 1995
                               | Year == 2000
                               | Year == 2005)


qplot(x=Date.Time,y=Mean.Temp...C.,data=StLawrenceCsvFiltered,colour = Month)






years=StLawrenceCsvFiltered$Year
years=list()
years[1]=1970
years[13]=1975
years[25]=1980
years[37]=1985
years[49]=1990
years[61]=1995
years[73]=2000
years[85]=2005
years[96]=NULL


StLawrenceCsv$Yr = seq(1,432)
#Scatter plot showing monthly trends for every year
p0<-ggplot(StLawrenceCsv, aes(x=Yr,y=Mean.Temp...C.)) + 
  geom_point(aes(group=1,colour = cut(Month,c(0,1,2,3,4,5,6,7,8,9,10,11,12))), size=I(5)) +
  scale_color_manual(name = "Month",
                     values = c("(0,1]" = "navyblue",
                                "(1,2]" = "blue3",
                                "(2,3]" = "steelblue2",
                                "(3,4]" = "lightblue2",
                                "(4,5]" = "khaki2",
                                "(5,6]" = "yellow1",
                                "(6,7]" = "orange2",
                                "(7,8]" = "rosybrown",
                                "(8,9]" = "darkorange1",
                                "(9,10]" = "lightgoldenrod3",
                                "(10,11]" = "cadetblue2",
                                "(11,12]" = "snow4"),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")) +
  xlab("Years (1970-2005)") +
  ylab("Mean Temperature (°C)") +
  theme_minimal() +
  geom_smooth(data=subset(StLawrenceCsv, Month == 6), method = "lm", colour = "red", se = FALSE)+
  geom_smooth(method = "lm", colour = "black", se = FALSE)+
  geom_smooth(data=subset(StLawrenceCsv, Month == 12), method = "lm", se = FALSE)+
  scale_x_discrete(limits = c(1970,2005))
p0



#Stacked Graphs which has ALL the years 1970-2004
p2<-ggplot(StLawrenceCsv, aes(x=Month,y=Mean.Temp...C.)) +
  geom_point(aes(colour = cut(Month,c(0,1,2,3,4,5,6,7,8,9,10,11,12))),size=5) +
  scale_color_manual(name = "Month",
                     values = c("(0,1]" = "navyblue",
                                "(1,2]" = "blue3",
                                "(2,3]" = "steelblue2",
                                "(3,4]" = "lightblue2",
                                "(4,5]" = "khaki2",
                                "(5,6]" = "yellow1",
                                "(6,7]" = "orange2",
                                "(7,8]" = "rosybrown",
                                "(8,9]" = "darkorange1",
                                "(9,10]" = "lightgoldenrod3",
                                "(10,11]" = "cadetblue2",
                                "(11,12]" = "snow4"),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))+
  facet_wrap(StLawrenceCsv$Year, scales = "fixed") +
  xlab("Months") +
  ylab("Mean Temperature")+
  scale_x_discrete(limits = c(1,6,12))
p2



#graph showing yearly trends (no months shown)
StLawrenceCsvLimited = select(StLawrenceCsv, Date.Time, Year, Month, Mean.Temp...C., Total.Precip..mm.)
StLData.Yearly = StLawrenceCsvLimited[FALSE,]
for (i in seq(0,35)){
  annualdata = filter(StLawrenceCsvLimited, Year == 1970+i)
  
  avgPrec = mean(annualdata$Total.Precip..mm.,na.rm=TRUE)
  avgTemp = mean(annualdata$Mean.Temp...C.,na.rm=TRUE)
  
  annualdata$Total.Precip..mm.[1] = avgPrec
  annualdata$Mean.Temp...C.[1] = avgTemp
  
  StLData.Yearly = rbind(StLData.Yearly,annualdata[1,])
}
StLData.Yearly = select(StLData.Yearly, -Month)

#Yearly temp data
p5<-ggplot(StLData.Yearly,aes(Year,Mean.Temp...C.)) +
  geom_point(aes(colour = Mean.Temp...C.),size=6) +
  labs(colour = "Mean Temperature (°C)")+
  scale_color_gradient2() +
  xlab("Years") +
  ylab("Mean Temperature (°C)") +
  theme_minimal() +
  geom_smooth(method = "lm")
p5


#ICE COVER graphs
StLData.IceCover = StLawrenceCsvLimited[FALSE,]
for (i in seq(0,35)){
  annualdata = filter(StLawrenceCsvLimited, Year == 1970+i)
  ice = 0
  noice = 0
  for (j in seq(1,nrow(annualdata))){
    data = annualdata[j,]
    if (!is.na(data$Mean.Temp...C.) & data$Mean.Temp...C. <= -2) {
      ice = ice + 1
    }else{
      noice = noice + 1
    }
  }
  annualdata$Condition[1] = "Ice Cover"
  annualdata$Condition[2] = "No Ice Cover"
  annualdata$Value[1] = ice
  annualdata$Value[2] = noice
  
  annualdata= select(annualdata, Year, Condition, Value)
  
  StLData.IceCover = rbind(StLData.IceCover,annualdata[1:2,])
}



p3<-ggplot(StLData.IceCover, aes(fill=Condition, y=Value, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Years (1970-2005)") +
  ylab("Months (1-12)")
p3

