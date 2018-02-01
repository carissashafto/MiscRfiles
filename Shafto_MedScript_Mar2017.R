#merge patient data into one data file
m1 <- merge(admins, dem)
m2 <- merge(orders, patients)
patientdata <- merge(m1, m2)

#some patients have 'unknown' and others are <NA>. make all missing 'unknown' so they will group
patientdata$gender[is.na(patientdata$gender)] <- "unknown"

#view patient data; look for anomalies
library(plyr)
DemTbl <- ddply(patientdata, .(gender, race), summarize, N=length(unique(patient_id)), 
                MinAge = min(age), MaxAge=max(age), Age=round(mean(age), 2), sdAge=round(sd(age), 2))
DemTbl

#check dates for oddities
length(unique(patientdata$administered_date))

patientdata$administered_date <- as.Date(as.character(patientdata$administered_date), "%d-%b-%Y")
#this removed 3 dates that were not in the %d-%b-%Y format

length(unique(patientdata$administered_date))

#this shows there are 2 dates in the future
test <- subset(patientdata, administered_date > as.Date("2017-03-07"))
length(unique(test$administered_date))

#replace the future administration dates with NA
patientdata$administered_date[which(patientdata$administered_date > as.Date("2017-03-07"))]<- NA

#create table to see how many times each patient was given drugs overall and by drug
AdminTbl <- ddply(patientdata, .(patient_id), summarize, TotalAdmins=length(unique(administered_date)))
DrugTbl <- ddply(patientdata, .(patient_id, drug_name), summarize, DrugAdmins=length(unique(administered_date)))

DrugSum <- ddply(DrugTbl, .(drug_name), summarize, Count=unique(DrugAdmins))

length(patientdata$administered_date)

#if desired, can merge AdminTbl with patientdata to include number of drug administrations
#  as part of the dataset
#  fulldata <- merge(AdminTbl, patientdata)

#*******************************************************************************************

#use those data tables to visualize frequencies
#create plot for count of medication administration
Medication <- NULL

pdfPath="filepath" 
pdf(file=pdfPath, height=9, width=7.5)

MedCounts <- table(AdminTbl$TotalAdmins)
Medication <- barplot(MedCounts, main="Distribution of Medication Administration", 
        xlab="Count of Medication Administration", ylab="Number of Patients Per Count",
        col = "blue")
dev.off()

#*****************************************************************************************
#What is the average time elapsed between a patient's initial diagnosis date and a patient's first treatment? 
#Does this time vary by gender? 

patientdata <- merge(m1, m2)
patientdata$gender[is.na(patientdata$gender)] <- "unknown"

#not every patient has an original diagnosis date, so some diffs will be NA
sum(is.na(patientdata$diagnosis_date))

#set dates to be a date
patientdata$diagnosis_date <- as.Date(as.character(patientdata$diagnosis_date), "%d-%b-%Y")
patientdata$administered_date <- as.Date(as.character(patientdata$administered_date), "%d-%b-%Y")

#remove dates in the future
patientdata$administered_date[which(patientdata$administered_date > as.Date("2017-03-07"))]<- NA

#calculate a new variable for difference between original diagnosis and first treatment
patientdata$DiagToTreat <- difftime(patientdata$administered_date, 
    patientdata$diagnosis_date, units="days")

#remove duplicate patient rows and calculate mean lapse from original to advanced diagnosis
#first sort to be sure to keep the shortest lag time
sorted <- patientdata[order(-patientdata$DiagToTreat),]
cleanData <- sorted[!duplicated(sorted[1]),]
mean(cleanData$DiagToTreat, na.rm=TRUE)

library(plyr)
TreatTable <- ddply(cleanData, .(gender), summarize, N=length(unique(patient_id)), Treatment_Lag=round(mean(DiagToTreat, na.rm=TRUE), 2))
TreatTable

#view to see if there is a normal distribution; there's not!
cleanData$DiagToTreat <- as.numeric(cleanData$DiagToTreat)
qqnorm(cleanData$DiagToTreat)
shapiro.test(cleanData$DiagToTreat)

#do a statistical test for effect of gender; must use non-parametric
kruskal.test(cleanData$DiagToTreat ~ cleanData$gender)


#don't use this because data are not normally distributed
#GenderLag <- aov(cleanData$DiagToTreat ~ cleanData$gender, data=cleanData) 
#summary(GenderLag)


#***************************************************************************************
#How many patients are on [drug] from 2012 to 2016?
#look at only treatments from 2012 to 2016
Data2012to2016 <- subset(patientdata, administered_date > as.Date("2012-01-01") & administered_date < as.Date("2017-01-01"))

#table of drugs for just the subset of 2012 to 2016, by unique patient
Drugs <- ddply(Data2012to2016, .(drug_name), summarize, NumPatients=length(unique(patient_id)))
Drugs

#***************************************************************************************

#create groups for high, medium, and low risk patients; use original dataset, but remove duplicates

#HighRisk = Female; any age; NON_WHITE OR Male; >= 70; NON_WHITE
#MediumRisk = Female; >=75, WHITE OR Male; <70; NON_WHITE
#LowRisk = Female; < 75, WHITE OR Male; any age; WHITE

#start with original dataset so each patient only appears on one row
dem <- read.csv("filepath", na.strings="")

#remove NAs
dem <- dem[complete.cases(dem$gender),] 

#create risk groups based on the stratification rules
dem$RiskCat <- ifelse((dem$gender=="female") & (dem$race=="NON_WHITE"), "High Risk", 
      ifelse ((dem$gender=="male") & (dem$age >= 70) & (dem$race=="NON_WHITE"), "High Risk",
      ifelse ((dem$gender=="female") & (dem$age >= 75) & (dem$race=="WHITE"), "Medium Risk", 
      ifelse((dem$gender=="male") & (dem$age < 70) & (dem$race=="NON_WHITE"), "Medium Risk", 
      ifelse ((dem$gender=="female") & (dem$age < 75) & (dem$race=="WHITE"), "Low Risk", 
      ifelse ((dem$gender=="male") & (dem$race=="WHITE"), "Low Risk",      
      NA)))))) 

#convert risk category to a factor and set levels in correct order (not alphabetical)
dem$RiskCat <- factor(dem$RiskCat, levels=c("High Risk", "Medium Risk", "Low Risk"))

#summarize risk categories by gender
library(plyr)
RiskTbl <- ddply(dem, .(RiskCat, gender), summarize, Count=length(patient_id))

RiskTbl

#*********************************************************************************

#Visualization that could be used to help a medical researcher 
#  understand how drug prevalence has changed over time.

patientdata <- merge(m1, m2)

#set date format
patientdata$administered_date <- as.Date(as.character(patientdata$administered_date), "%d-%b-%Y")

#replace the future administration dates with NA and then subset
patientdata$administered_date[which(patientdata$administered_date > as.Date("2017-03-07"))]<- NA

compdata <- patientdata[complete.cases(patientdata$administered_date),] 

compdata$AdminYear <- format(compdata$administered_date, '%Y')

#count total administrations per year to see if that is a good way to divide data
Trends <- ddply(compdata, .(AdminYear, drug_name), summarize, DrugAdmins=length(administered_date))
Trends

#create plot for count of medication administration
library(ggplot2)

TrendsPlot <- NULL

pdfPath="filepath" 
pdf(file=pdfPath, height=7.5, width=9)

#set different bars for the different medications
TrendsPlot <- ggplot(compdata, aes(factor(AdminYear)))+
  geom_bar() +
  ggtitle("Prevalence of Drug Administration: 2005 to 2015") +
  ylab("Number of Administrations") +
  xlab("Year") +
  facet_wrap(~ drug_name)

#make those bars different colors and larger and increase font size
TrendsPlot + geom_bar(aes(color=factor(drug_name)), size=2) +
  theme(axis.title = element_text(size = rel(1.5))) +
  theme(strip.text = element_text(size = rel(1.5))) +
  theme(plot.title = element_text(size = 20))

dev.off()

#check to see if changes in drug administration are clinic specific.
Clinics <- NULL

pdfPath="filepath" 
pdf(file=pdfPath, height=7.5, width=9)

#set different bars for the different medications
Clinics <- ggplot(compdata, aes(factor(AdminYear)))+
  geom_bar() +
  ggtitle("Drug Administration By Practice: 2005 to 2015") +
  ylab("Number of Administrations") +
  xlab("Year") +
  facet_wrap(~ external_practice_id)

#make drugs different colors, increase font size, rotate axis tick labels
Clinics + geom_bar(aes(color=factor(drug_name)), size=1) +
  theme(axis.title = element_text(size = rel(1.5))) +
  theme(strip.text = element_text(size = rel(1.25))) +
  theme(axis.text = element_text(angle= -90)) +
  theme(plot.title = element_text(size = rel(1.5)))

dev.off()
