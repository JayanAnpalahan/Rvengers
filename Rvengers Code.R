#**********************************************************************************************************
#Calling upong survey data
SurveyInitial = read.csv("C:/Users/Jayan/Desktop/BIOL432/Rvengers/Initial Files/SurveyDataSetInitial.csv")


#Will need this package for cleaning up data!
library("dplyr")


#For clarity, codenames will be replaced with Common Names later on. 
CommonNamesGuide = read.csv("C:/Users/Jayan/Desktop/BIOL432/Rvengers/Initial Files/SpeciesCommonNames.csv")

#**********************************************************************************************************



#**********************************************************************************************************

#Filtered out species that were not the focus. Species of Interest codenames are: 
# "LTDU" (Long-tailed Duck), "RUDU" (Ruddy Duck), "ABDU" (American Black Duck)
# Species selected based on having a large amount of data and 
# also listed as at least population decreasing in IUCN redlist


LongTailOnly = filter(SurveyInitial, Species == "LTDU")
RuddyDuckOnly = filter(SurveyInitial, Species == "RUDU")
AmericanDuckOnly = filter(SurveyInitial, Species == "ABDU")

SurveyMerge = rbind(LongTailOnly, RuddyDuckOnly, AmericanDuckOnly)       



#**********************************************************************************************************


#**********************************************************************************************************

#Since our climate data set only goes up to 2005 (and then makes future predictions) lets take 
# years inbetween 1968 (earliest recording year) and 2005.


SurveyMerge = filter(SurveyMerge, Year <= 2005)

#Can check and see if there is data for 2005 using %in%, but there is none
#There is, however, data for 2004. So it is safe to assume that within our climate data range, the latest
# year for the survey that we can use is 2004.
2005 %in% SurveyMerge$Year 
2004 %in% SurveyMerge$Year

write.csv(SurveyMerge, file = "SurveyMerged.csv")


#**********************************************************************************************************


#**********************************************************************************************************

#Our goal is to filter the data so that it is just the total population count at each major location, annually.

#Let's first define the major locations and rename them from their codenames (Site ID)

# NOTE: Other locations were present in the metadata but since 
# we are focusing on three species, they are not listed

# ELO-STLR = Eastern Lake Ontario - St Lawrence River
# LH-GB = Lake Huron- Georgian Bay
# LSup = Lake Superior
# Rideau = Rideau Canal
# WLO-LE = Western Lake Ontario - Lake Erie

#**********************************************************************************************************



#**********************************************************************************************************

#First off, need to filter the Location ID column using Regex.
#To keep the original data set, so we keep set it to another variable and work with the new variable
#Then, using this new variable, run the regexs to replace the subsetted columns, in this case "SiteID,
#with the new criteria. 

IDFilteredSurvey = SurveyMerge

IDFilteredSurvey$SiteID = gsub("ELO-STLR_.*", 
                               "Eastern Lake Ontario - St Lawrence River", 
                               IDFilteredSurvey$SiteID)

IDFilteredSurvey$SiteID = gsub("LH-GB.*", 
                               "Lake Huron- Georgian Bay", 
                               IDFilteredSurvey$SiteID)

IDFilteredSurvey$SiteID = gsub("LSup.*", 
                               "Lake Superior", 
                               IDFilteredSurvey$SiteID)

IDFilteredSurvey$SiteID = gsub("Rideau.*", 
                               "Rideau Canal", 
                               IDFilteredSurvey$SiteID)

IDFilteredSurvey$SiteID = gsub("WLO-LE.*", 
                               "Western Lake Ontario - Lake Erie", 
                               IDFilteredSurvey$SiteID)

#**********************************************************************************************************





#**********************************************************************************************************

#We can do the same thing, except this time with the subset being the species names!

IDFilteredSurvey$Species = gsub("LTDU",
                                "Long-Tailed Duck", 
                                IDFilteredSurvey$Species)

IDFilteredSurvey$Species = gsub("RUDU",
                                "Ruddy Duck", 
                                IDFilteredSurvey$Species)

IDFilteredSurvey$Species = gsub("ABDU",
                                "American Black Duck", 
                                IDFilteredSurvey$Species)
#**********************************************************************************************************




#**********************************************************************************************************

#Removing Days and the objects ID since these are not relevant
IDFilteredSurvey = select(IDFilteredSurvey, -Day, -OBJECTID)

#**********************************************************************************************************





#**********************************************************************************************************

#We can now start doing totals! To do totals of the counts based on conditions, we just use aggregate

#So we sum the "Count" subset based on the subsets "Year" and "Species"

AnnualSpeciesSummed = aggregate(IDFilteredSurvey$Count,
                                list(IDFilteredSurvey$Year, IDFilteredSurvey$Species),
                                sum)




#Using the list format changes the column names, therefore use "colnames" to set it to desired column names
colnames(AnnualSpeciesSummed) = c("Year", "Species", "Count")

write.csv(AnnualSpeciesSummed, file = "AnnualSpeciesSummed.csv")
#**********************************************************************************************************




#**********************************************************************************************************

#Similar process repeated, only this time the list of subsets to aggregate by has months included!
#Note that months needs to go before years because we want it to sort the months within the years,
#within the species
#NOT
#sort the years within the months, within the species.

MonthlySpeciesSummed = aggregate(IDFilteredSurvey$Count,
                                 list(IDFilteredSurvey$Month, IDFilteredSurvey$Year, 
                                      IDFilteredSurvey$Species),
                                 sum)
colnames(MonthlySpeciesSummed) = c("Month", "Year", "Species", "Count")

write.csv(MonthlySpeciesSummed, file = "MonthlySpeciesSummed.csv")
#**********************************************************************************************************







#**********************************************************************************************************
#If we wanted to look at just within particular regions, we can using similar code!

SpeciesAtLocationSummed = aggregate(IDFilteredSurvey$Count,
                                 list(IDFilteredSurvey$Year, IDFilteredSurvey$Species, 
                                      IDFilteredSurvey$SiteID),
                                 sum)

colnames(SpeciesAtLocationSummed) = c("Year", "Species", "Location", "Count")

write.csv(SpeciesAtLocationSummed, file = "SpeciesAtLocationSummed.csv")
#**********************************************************************************************************





#**********************************************************************************************************
TotalAtLocationSummed  = aggregate(IDFilteredSurvey$Count,
                                    list(IDFilteredSurvey$Year, 
                                         IDFilteredSurvey$SiteID),
                                    sum)

colnames(TotalAtLocationSummed) = c("Year", "Location", "Count")

write.csv(TotalAtLocationSummed , file = "TotalAtLocationSummed .csv")

#**********************************************************************************************************




#**********************************************************************************************************

#Now we can break everything into individual components (By Species, By location) via filter

#By Species

AmericanDuckAnnualCounts = filter(AnnualSpeciesSummed, Species == "American Black Duck")
write.csv(AmericanDuckAnnualCounts, file = "AmericanDuckAnnualCounts.csv")

AmericanDuckMonthlyCounts = filter(MonthlySpeciesSummed, Species == "American Black Duck")
write.csv(AmericanDuckMonthlyCounts, file = "AmericanDuckMonthlyCounts.csv")

RuddyDuckAnnualCounts = filter(AnnualSpeciesSummed, Species == "Ruddy Duck")
write.csv(RuddyDuckAnnualCounts, file = "RuddyDuckAnnualCounts.csv")

RuddyDuckMonthlyCounts = filter(MonthlySpeciesSummed, Species == "Ruddy Duck")
write.csv(AmericanDuckMonthlyCounts, file = "RuddyDuckMonthlyCounts.csv")


LongTailedDuckAnnualCounts = filter(AnnualSpeciesSummed, Species == "Long-Tailed Duck")
write.csv(LongTailedDuckAnnualCounts, file = "Long-TailedDuckAnnualCounts.csv")

LongTailedDuckMonthlyCounts = filter(MonthlySpeciesSummed, Species == "Long-Tailed Duck")
write.csv(LongTailedDuckMonthlyCounts, file = "LongTailedDuckMonthlyCounts.csv")


#**********************************************************************************************************



#**********************************************************************************************************

StLawrenceAnnualCounts = filter(SpeciesAtLocationSummed, 
                                Location == "Eastern Lake Ontario - St Lawrence River")
write.csv(StLawrenceAnnualCounts, file = "StLawrenceAnnualCounts.csv")


#******IDEAL TO EXCLUDE, TOO FEW TEMPORAL POINTS*******
GeorgianBayAnnualCounts = filter(SpeciesAtLocationSummed, 
                                Location == "Lake Huron- Georgian Bay")
write.csv(GeorgianBayAnnualCounts, file = "GeorgianBayAnnualCounts.csv")


#******IDEAL TO EXCLUDE, TOO FEW TEMPORAL POINTS*******
LakeSuperiorAnnualCounts = filter(SpeciesAtLocationSummed, 
                                Location == "Lake Superior")
write.csv(LakeSuperiorAnnualCounts, file = "LakeSuperiorAnnualCounts.csv")


#******IDEAL TO EXCLUDE, TOO FEW TEMPORAL POINTS*******
RideauAnnualCounts = filter(SpeciesAtLocationSummed, 
                                Location == "Rideau Canal")
write.csv(RideauAnnualCounts, file = "RideauAnnualCounts.csv")



LakeErieAnnualCounts = filter(SpeciesAtLocationSummed, 
                                Location == "Western Lake Ontario - Lake Erie")
write.csv(LakeErieAnnualCounts, file = "LakeErieAnnualCounts.csv")

#**********************************************************************************************************




#**********************************************************************************************************

#Note that the names are based on water locations but the actual survey was done on shorelines,
#therefore, we used climate data from stations very close to the shorelines

#Mean T

StLawrenceMeanT = read.csv("C:/Users/Jayan/Desktop/BIOL432/Rvengers/Initial Files/WolfeIslandMeanT.csv")

#Filtered the first two rows since they were irrelevant headers, and filtered out all columns except the 
#first two, which are the historical modeling values (most relevant for observing trends)

StLawrenceMeanT = StLawrenceMeanT[3:nrow(StLawrenceMeanT), 1:2]

#Reset the header names to the desired
colnames(StLawrenceMeanT) = c("Year", "Mean Temperature")

#The "Year" column was formatted as the full date and time, so I selected just the first 4 digits corresponding
#to the year, and removed everything else after
StLawrenceMeanT$Year = gsub("(\\d\\d\\d\\d).*", "\\1", StLawrenceMeanT$Year)

#Now, need to filter out data before 1968 (earliest in survey), and data after 2004 (latest in survey).

StLawrenceMeanT = filter(StLawrenceMeanT, Year <= 2004,Year >= 1968)

write.csv(StLawrenceMeanT, file = "StLawrenceMeanT.csv")


#Can now repeat this code for each csv file, ensuring correct variables and correct headers used

LakeErieMeanT = read.csv("C:/Users/Jayan/Desktop/BIOL432/Rvengers/Initial Files/WellandMeanT.csv")

LakeErieMeanT = LakeErieMeanT[3:nrow(LakeErieMeanT), 1:2]

colnames(LakeErieMeanT) = c("Year", "Mean Temperature")

LakeErieMeanT$Year = gsub("(\\d\\d\\d\\d).*", "\\1", LakeErieMeanT$Year)

LakeErieMeanT = filter(LakeErieMeanT, Year <= 2004, Year >= 1968)

write.csv(LakeErieMeanT, file = "LakeErieMeanT.csv")


#**********************************************************************************************************





#**********************************************************************************************************

#Tmax > 25 degrees celsius = Summer Days

StLawrenceSummerDays = read.csv("C:/Users/Jayan/Desktop/BIOL432/Rvengers/Initial Files/WolfeIslandSummerDays.csv")

StLawrenceSummerDays = StLawrenceSummerDays[3:nrow(StLawrenceSummerDays), 1:2]

colnames(StLawrenceSummerDays) = c("Year", "Days Tmax > 25 degrees Celsius")

StLawrenceSummerDays$Year = gsub("(\\d\\d\\d\\d).*", "\\1", StLawrenceSummerDays$Year)

StLawrenceSummerDays = filter(StLawrenceSummerDays, Year <= 2004, Year >= 1968)

write.csv(StLawrenceSummerDays, file = "StLawrenceSummerDays.csv")



LakeErieSummerDays = read.csv("C:/Users/Jayan/Desktop/BIOL432/Rvengers/Initial Files/WellandSummerDays.csv")

LakeErieSummerDays = LakeErieSummerDays[3:nrow(LakeErieSummerDays), 1:2]

colnames(LakeErieSummerDays) = c("Year", "Days Tmax > 25 degrees Celsius")

LakeErieSummerDays$Year = gsub("(\\d\\d\\d\\d).*", "\\1", LakeErieSummerDays$Year)

LakeErieSummerDays = filter(LakeErieSummerDays, Year <= 2004, Year >= 1968)

write.csv(LakeErieSummerDays, file = "LakeErieSummerDays.csv")

#**********************************************************************************************************



#**********************************************************************************************************

#Tmin < -15 degrees celsius = Cold Days

StLawrenceColdDays = read.csv("C:/Users/Jayan/Desktop/BIOL432/Rvengers/Initial Files/WolfeIslandColdDays.csv")

StLawrenceColdDays = StLawrenceColdDays[3:nrow(StLawrenceColdDays), 1:2]

colnames(StLawrenceColdDays) = c("Year", "Days Tmin < -15 degrees Celsius")

StLawrenceColdDays$Year = gsub("(\\d\\d\\d\\d).*", "\\1", StLawrenceColdDays$Year)

StLawrenceColdDays = filter(StLawrenceColdDays, Year <= 2004, Year >= 1968)

write.csv(StLawrenceColdDays, file = "StLawrenceColdDays.csv")



LakeErieColdDays = read.csv("C:/Users/Jayan/Desktop/BIOL432/Rvengers/Initial Files/WellandColdDays.csv")

LakeErieColdDays = LakeErieColdDays[3:nrow(LakeErieColdDays), 1:2]

colnames(LakeErieColdDays) = c("Year", "Days Tmin < -15 degrees Celsius")

LakeErieColdDays$Year = gsub("(\\d\\d\\d\\d).*", "\\1", LakeErieColdDays$Year)

LakeErieColdDays = filter(LakeErieColdDays, Year <= 2004, Year >= 1968)

write.csv(LakeErieColdDays, file = "LakeErieColdDays.csv")

#**********************************************************************************************************



#**********************************************************************************************************

#Max T does not exceed 0 degrees = Ice Days
#Could be useful for indicating poor growth conditions for plant life, low abundance of food
#could signal earlier migration or migration to a different location over time

StLawrenceIceDays = read.csv("C:/Users/Jayan/Desktop/BIOL432/Rvengers/Initial Files/WolfeIslandIceDays.csv")

StLawrenceIceDays = StLawrenceIceDays[3:nrow(StLawrenceIceDays), 1:2]

colnames(StLawrenceIceDays) = c("Year", "Days Tmax < 0 degrees Celsius")

StLawrenceIceDays$Year = gsub("(\\d\\d\\d\\d).*", "\\1", StLawrenceIceDays$Year)

StLawrenceIceDays = filter(StLawrenceIceDays, Year <= 2004, Year >= 1968)

write.csv(StLawrenceIceDays, file = "StLawrenceIceDays.csv")


LakeErieIceDays = read.csv("C:/Users/Jayan/Desktop/BIOL432/Rvengers/Initial Files/WolfeIslandIceDays.csv")

LakeErieIceDays = LakeErieIceDays[3:nrow(LakeErieIceDays), 1:2]

colnames(LakeErieIceDays) = c("Year", "Days Tmax < 0 degrees Celsius")

LakeErieIceDays$Year = gsub("(\\d\\d\\d\\d).*", "\\1", LakeErieIceDays$Year)

LakeErieIceDays = filter(LakeErieIceDays, Year <= 2004, Year >= 1968)

write.csv(LakeErieIceDays, file = "LakeErieIceDays.csv")

#**********************************************************************************************************

