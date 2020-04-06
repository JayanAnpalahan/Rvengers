#Calling upong survey data
ShorebirdSurvey = read.csv("C:/Users/Jayan/Desktop/BIOL432/Rvengers/ShorebirdSurvey.csv")

#The survey data has codenames instead of common names, so this text file will be useful 
#for graphs and other visualizations (replace codenames in those visualizations with the Common Names)

CodeNames = read.delim("C:/Users/Jayan/Desktop/BIOL432/Rvengers/Shorebird Common Names.txt")


library("dplyr")

#Arranged by location to view what locations we have more easily
LocationArrange = arrange(ShorebirdSurvey, Location)

#Isolating each dataset based on location

#Here, I am separating the locations into their own dataframes so that they can be observed individually,
#merged together, or removed. We should have a final survey data set.


ArakanFlats = filter(ShorebirdSurvey, Location == "Arakan Flats")

ChestermanBeach = filter(ShorebirdSurvey, Location == "Chesterman Beach")

DougBanksFlats = filter(ShorebirdSurvey, Location == "Doug Banks' Flats")

DuckingFlats = filter(ShorebirdSurvey, Location == "Ducking Flats")


#These Grice Bay Locations could likely be merged given the similar location
GriceBay = filter(ShorebirdSurvey, Location == "Grice Bay")

GriceBayBoatLaunch = filter(ShorebirdSurvey, Location == "Grice Bay - Boat Launch")

GriceBayRoadIslet = filter(ShorebirdSurvey, Location == "Grice Bay - Road, Islet")

View(GriceBay)
View(GriceBayBoatLaunch)
View(GriceBayRoadIslet)

#Want to take surveys and where there is an equal date, add the surveyed populations together.
#This will give the total for Grice Bay 

GriceBayF = rbind(GriceBay, GriceBayBoatLaunch, GriceBayRoadIslet)

View(GriceBayF)


#********************************************************************************************************
#Trying to make a loop that goes and reads each row of the dataframe (GriceBayF) and if the "Date" 
#is equal, then to add the values of the species counts.
#Probably need a variable which excludes all other measures except the species counts, so that these
#can be summed (so I called this variable "Species")

SpeciesDF = select(GriceBayF, -Date, -DOY, -Year, -Location) 
  
View(SpeciesDF)

Species = colnames(SpeciesDF)

Species

#My code from the first assignment. Figured I had to use this somehow since it is a similar procedure.
#This checks a column (MData$unitsW) at row ([i]) and changes another column (MData$Limb.Width) at row ([i]).
#Just need to change the variables and set the right conditions

  
for (i in 1:nrow(MData)){
  if (MData$unitsW [i] == "cm") {
    MData$Limb.Width [i] = (MData$Limb.Width[i]*10)
    MData$unitsW [i] = "mm"}
}
#**********************************************************************************************************


IndianIslandRoost = filter(ShorebirdSurvey, Location == "Indian Island Roost")

MorpheusIsland = filter(ShorebirdSurvey, Location == "Morpheus Island")

SharpRoadLookout = filter(ShorebirdSurvey, Location == "Sharp Road Lookout")

SouthBayMaltbySlough = filter(ShorebirdSurvey, Location == "South Bay/Maltby Slough")

StubbsIsland = filter(ShorebirdSurvey, Location == "Stubbs Island")

TofinoAirport = filter(ShorebirdSurvey, Location == "Tofino Airport")

TofinoRocks = filter(ShorebirdSurvey, Location == "Tofino Rocks")








