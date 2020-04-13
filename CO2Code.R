CO2Data = read.csv("C:/Users/Jayan/Desktop/BIOL432/Rvengers/CO2NationalData.csv")
#Note that this data is form a source that is international, and therefore has been filtered 
#to show the relevant country (Canada) and relevant factors (GHG and CO2) only. 

#Now we filter it for the relevant years,and remove other unneccessary elements! ("dplyr" required)
library("dplyr")

#The names are adjusted to make it easier to work with

names(CO2Data) = c("Year", "FossilFuelCO2", "GHGCO2Total")

#The years are filtered for relevant years

CO2DataFinal = filter(CO2Data, Year <= 2005, Year >= 1970)

#Now we can graph!
library("ggplot2")


#Just the Fossil Fuel emissions plotted

FossilLineGraph = ggplot(CO2DataFinal, aes(y = FossilFuelCO2, x = Year)) +
  geom_point(data = CO2DataFinal, colour = 'red', size = 3) +
  ylab("Fossil Fuel and Cement CO2 (10^12 kg)") +
  theme_minimal() + 
  geom_smooth(method = "lm", colour = "black") +
  ggtitle("Fossil Fuel CO2 Emitted 1970-2005")

png("FossilLineGraph.png")
FossilLineGraph
dev.off()



#GHG Total CO2 emissions plotted

GHGLineGraph = ggplot(CO2DataFinal, aes(y = GHGCO2Total, x = Year)) +
  geom_point(data = CO2DataFinal, colour = 'blue', size = 3) +
  ylab("Greenhouse Gas Total Emissions (10^12 kg CO2 eq)") +
  theme_minimal()+ 
  geom_smooth(method = "lm", colour = "black")+
  ggtitle("Greenhouse Gas Total CO2 Emitted 1970-2005")

png("GHGLineGraph.png")
GHGLineGraph
dev.off()



#For better visualization, lets see how much Fossil Fuels makes up in the GHG total
#First we need to set their values to the same name (Emmited), and then label each one (Emittor)

FossilCO2 = select(CO2DataFinal, Year, FossilFuelCO2, -GHGCO2Total)
FossilCO2dat = data.frame(Year = FossilCO2$Year,
                       Emitted = FossilCO2$FossilFuelCO2,
                       Emitter = rep("Fossil Fuel CO2", 36))



GHGTotal = select(CO2DataFinal, Year, GHGCO2Total, -FossilFuelCO2)
GHGTotaldat = data.frame(Year = GHGTotal$Year,
                          Emitted = GHGTotal$GHGCO2Total,
                          Emitter = rep("GHG Total CO2", 36))


PercentGas = rbind(GHGTotaldat, FossilCO2dat)

#Then we use this data to make a stacked plot with "fill" set in the position for the bars.

#This will fill the bars based on the conditions specified, in this case we want the bars to be filled
#as a percentage of GHG.

StackedPlot = ggplot(PercentGas, aes(x = Year, y = Emitted, fill = Emitter)) + 
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal()+
  ylab("Percent of CO2 Emissions (%)") +
  ggtitle("Percentage of Fossil Fuel CO2 to Total GHG CO2 1970-2005")

png("PercentEmissions.png")
StackedPlot
dev.off()







