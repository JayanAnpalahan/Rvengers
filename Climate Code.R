library("dplyr")

LakeErieCsv = read.csv("Climate/Lake Erie/Erie 1968-2006.csv")
LakeErieCsv = filter(LakeErieCsv, Year <= 2004)
LakeErieCsv = filter(LakeErieCsv, Year >= 1970)

StLawrenceCsv1 = read.csv("Climate/St Lawrence/St 1970-1976.csv")
StLawrenceCsv2 = read.csv("Climate/St Lawrence/St 1976-2006.csv")
StLawrenceCsv = rbind(StLawrenceCsv1, StLawrenceCsv2)
StLawrenceCsv = filter(StLawrenceCsv, Year <= 2004)
StLawrenceCsv = filter(StLawrenceCsv, Year >= 1970)

write.csv(LakeErieCsv, "Climate/Lake Erie (1970-2004).csv")
write.csv(StLawrenceCsv, "Climate/St Lawrence (1970-2004).csv")
