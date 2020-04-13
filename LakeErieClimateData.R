library("dplyr")
library(ggplot2)

LakeErieCsv = read.csv("Climate/Lake Erie/Erie 1968-2006.csv")
LakeErieCsv = filter(LakeErieCsv, Year <= 2005)
LakeErieCsv = filter(LakeErieCsv, Year >= 1970)

write.csv(LakeErieCsv, "Climate/Lake Erie (1970-2005).csv")



LakeErieCsv$Yr = seq(1,432)
#Scatter plot showing monthly trends every five years so 1970-1975-1980-1985-1990-1995-2005
pi<-ggplot(LakeErieCsv, aes(x=Yr,y=Mean.Temp...C.)) + 
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
  scale_x_discrete(limits = c(1970,2005))
pi


LakeErieCsv$Yr = seq(1,432)
#Scatter plot showing monthly trends every five years so 1970-1975-1980-1985-1990-1995-2005
p0<-ggplot(LakeErieCsv, aes(x=Yr,y=Mean.Temp...C.)) + 
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
  geom_smooth(data=subset(LakeErieCsv, Month == 6), method = "lm", colour = "red", se = FALSE)+
  geom_smooth(method = "lm", colour = "black", se = FALSE)+
  geom_smooth(data=subset(LakeErieCsv, Month == 12), method = "lm", se = FALSE)+
  scale_x_discrete(limits = c(1970,2005))
p0

#Stacked Graphs which has ALL the years 1970-2004
p2<-ggplot(LakeErieCsv, aes(x=Month,y=Mean.Temp...C.)) +
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
  facet_wrap(LakeErieCsv$Year, scales = "fixed") +
  xlab("Months") +
  ylab("Mean Temperature")+
  scale_x_discrete(limits = c(1,6,12))
p2

#graph showing yearly trends (no months shown)
LakeErieCsvLimited = select(LakeErieCsv, Date.Time, Year, Month, Mean.Temp...C., Total.Precip..mm.)
LEData.Yearly = LakeErieCsvLimited[FALSE,]
for (i in seq(0,35)){
  annualdata = filter(LakeErieCsvLimited, Year == 1970+i)
  
  avgPrec = mean(annualdata$Total.Precip..mm.,na.rm=TRUE)
  avgTemp = mean(annualdata$Mean.Temp...C.,na.rm=TRUE)
  
  annualdata$Total.Precip..mm.[1] = avgPrec
  annualdata$Mean.Temp...C.[1] = avgTemp
  
  LEData.Yearly = rbind(LEData.Yearly,annualdata[1,])
}
LEData.Yearly = select(LEData.Yearly, -Month)

#Yearly temp data
p5<-ggplot(LEData.Yearly,aes(Year,Mean.Temp...C.)) +
  geom_point(aes(colour = Mean.Temp...C.),size=6) +
  labs(colour = "Mean Temperature (°C)")+
  scale_color_gradient2() +
  xlab("Years") +
  ylab("Mean Temperature (°C)") +
  theme_minimal() +
  geom_smooth(method = "lm")
p5


#ICE COVER graphs
LEData.IceCover = LakeErieCsvLimited[FALSE,]
for (i in seq(0,35)){
  annualdata = filter(LakeErieCsvLimited, Year == 1970+i)
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
  
  LEData.IceCover = rbind(LEData.IceCover,annualdata[1:2,])
}



p3<-ggplot(LEData.IceCover, aes(fill=Condition, y=Value, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Years (1970-2005)") +
  ylab("Months (1-12)")
p3



