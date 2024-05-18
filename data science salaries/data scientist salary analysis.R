#factors that affect the salary in the  data science field
getwd()
setwd("D:/ERICK/Kinuthia R/data science salaries")#here we are setting our working directory
options("install.lock"=FALSE)
if(!require("tidyverse"))install.packages("tidyverse",dependencies = T)
library(tidyverse)
dss <- read_csv("data_science_salaries.csv")#importing our csv(comma separated values) in R
View(dss) #we want to view our data
dim(dss)#the output (6599   11)means that we have 11 variables and 6599 recorded observations in our dataset
#summary data
summary(dss)
str(dss)

#setting the variables that we want to work with,thus removing salary because it was recorded in different currencies.
dss<-dss[,-which(names(dss)=="salary")]
summary(dss)
#Data cleaning
table(dss$job_title)
# clean data for machine learning and ML, and BI and business Intelligence
dss$job_title<-replace(dss$job_title,dss$job_title=="Data Science","Data Scientist")
dss$job_title<-replace(dss$job_title,dss$job_title=="ML Engineer","Machine Learning Engineer")
dss$job_title<-replace(dss$job_title,dss$job_title=="Data Modeler","Data Modeller")
dss$job_title<-replace(dss$job_title,dss$job_title=="Head of Data","Head of Data Science")
dss$job_title<-replace(dss$job_title,dss$job_title=="BI Analyst"|dss$job_title=="Business Intelligence Analyst"|dss$job_title=="BI Data Analyst","Business Intelligence Data Analyst")
dss$job_title<-replace(dss$job_title,dss$job_title=="BI Developer","Business Intelligence Developer")
dss$job_title<-replace(dss$job_title,dss$job_title=="BI Data Engineer","Business Intelligence Engineer")
table(dss$job_title)

#identifying duplicates
duplicate<-dss[duplicated(dss),] #this line code finds the duplicated in our data,arranges it into rows then it is asigned to the name duplicate which in the next line we print it out
duplicate

#removing the duplicates 
dss<-unique(dss)
dss
dim(dss)#the number of observations recorded reduced due to the removal of duplicates

#Data manipulation

# Job title encoding
table(dss$job_title)
dss$jobttcat<-ifelse(dss$job_title=="Data Scientist",0,1)%>% factor(levels = c(0,1) ,labels  =c("Data scientist","Others"))
table(dss$jobttcat)

# Experience level encoding
table(dss$experience_level)
dss$experience_levelcat<-ifelse(dss$experience_level=="Entry-level"|
                                  dss$experience_level=="Mid-level",0,1)%>%factor(levels = c(0,1),labels = c("Junior","Senior"))

table(dss$experience_levelcat)

# Employment type encoding
table(dss$employment_type)
dss$employment_typecat<-ifelse(dss$employment_type=="Full-time",0,1)%>%factor(levels = c(0,1), labels = c("Full-time","Part-time"))
table(dss$employment_typecat)

#Employee residence encoding
table(dss$employee_residence)
dss$employee_residencecat=dss$employee_residence
dss$employee_residencecat<-ifelse(dss$employee_residence==" China"|
                                    dss$employee_residence=="Thailand"|
                                    dss$employee_residence=="Vietnam"|
                                    dss$employee_residence=="Qatar "|
                                    dss$employee_residence=="Saudi Arabia"|
                                    dss$employee_residence=="India"|
                                    dss$employee_residence=="Iraq"|
                                    dss$employee_residence=="Iran"|
                                    dss$employee_residence=="Japan"|
                                    dss$employee_residence=="Hong Kong "|
                                    dss$employee_residence=="Indonesia"|
                                    dss$employee_residence=="South Korea"|
                                    dss$employee_residence=="Pakistan "|
                                    dss$employee_residence=="Philippines"|
                                    dss$employee_residence=="Algeria"|
                                    dss$employee_residence=="Central African Republic"|                   dss$employee_residence=="Egypt"|
                                    dss$employee_residence=="Ghana"|
                                    dss$employee_residence=="Kenya"|
                                    dss$employee_residence=="Nigeria"|
                                    dss$employee_residence=="Singapore"|
                                    dss$employee_residence=="South Africa"|
                                    dss$employee_residence=="Uganda"|
                                    dss$employee_residence=="Ukraine"|
                                    dss$employee_residence=="Tunisia"|
                                    dss$employee_residence=="Uzbekistan"|
                                    dss$employee_residence=="Cyprus",0,1)%>% factor(levels = c(0,1),labels=c("East","West"))
table(dss$employee_residencecat)

# Salary currency encoding
table(dss$salary_currency)
dss$salary_currencycat<-ifelse(dss$salary_currency=="USD",0,1)%>% factor(levels=c(0,1),labels=c("USD","Other currencies"))
table(dss$salary_currencycat)

# Salary in USD encoding
table(dss$salary_in_usd)
summary(dss$salary_in_usd)
dss$salary_in_usdcat<-ifelse(dss$salary_in_usd<=138000,0,1)%>%factor(levels=c(0,1),labels=c("Below USD 138000","Above USD 138000"))
table(dss$salary_in_usdcat)

# Company location encoding
table(dss$company_location)
dss$company_locationcat<-ifelse(dss$company_location=="Algeria"|
                                  dss$company_location=="Canada Central African Republic"|              dss$company_location=="China"|
                                  dss$company_location=="Egypt"|
                                  dss$company_location=="Ghana"|
                                  dss$company_location=="Hong Kong"|
                                  dss$company_location=="India"|
                                  dss$company_location=="Indonesia"|
                                  dss$company_location=="Iraq"|
                                  dss$company_location=="Japan"|
                                  dss$company_location=="Kenya"|
                                  dss$company_location=="Nigeria"|
                                  dss$company_location=="Qatar"|  
                                  dss$company_location=="Singapore"| 
                                  dss$company_location=="Saudi Arabia"| 
                                  dss$company_location=="Thailand"| 
                                  dss$company_location=="Vietnam"| 
                                  dss$company_location=="South Africa"| 
                                  dss$company_location=="South Korea"| 
                                  dss$company_location=="Ukraine"|
                                  dss$company_location=="United Arab Emirates" |
                                  dss$company_location=="Pakistan"  |
                                  dss$company_location=="Philippines",0,1)%>%factor(levels=c(0,1),labels=c("East","West"))
table(dss$company_locationcat)

#work year encoding
table(dss$work_year)
dss$work_yearcat<-ifelse(dss$work_year<=2022,0,1)%>%factor(labels=c("Before 2022","After 2022"))
table(dss$work_yearcat)



#work models encoding
table(dss$work_models)
dss$work_modelscat<-ifelse(dss$work_models=="Hybrid"|
                             dss$work_models=="Remote",0,1)%>%factor(levels = c(0,1),labels=c("Flexible employment","Non-flexible employment"))

table(dss$work_modelscat)


table(dss$company_size)
dss$company_sizecat<-ifelse(dss$company_size=="Large",0,ifelse(dss$company_size=="Medium",1,2))%>%factor(levels=c(0,1,2),labels=c("Large","Medium","Small"))
table(dss$company_sizecat)

# Creating a frequency table for bivariate analysis for factors that affect the salary in the  data science field 
if(!require(xfun))install.packages("xfun",dependencies = T)
library(xfun)
if(!require("table1"))install.packages("table1",dependencies = T)
library(table1)

table1(~factor(jobttcat)+ factor(experience_levelcat)+factor(employment_typecat)+factor(employee_residencecat)+factor(salary_currencycat)+factor(company_locationcat)+factor(work_yearcat)+factor(work_modelscat)+factor(company_size)|salary_in_usdcat,dss)


#Data visualization 

boxplot(dss$salary_in_usd,main="Salary in USD boxplot",frame=T)#we notice presence of outliers

hist(dss$salary_in_usd,freq = F,col="lightblue",main="Histogram",xlab = "Salary")
curve(dnorm(x,mean=mean(dss$salary_in_usd),sd=sd(dss$salary_in_usd)),add=T,col="red",lwd=2) #adding a line curve
#this shows that our data is not normally distributed because it is right skewed





#Proving with Anderson-darling test for large data size
if(!require("nortest"))install.packages("nortest",dependencies = T)
library(nortest)
ad.test(dss$salary_in_usd)
#the output shows that A = 42.676 which states that our data is not normally distributed(the value is a large value),this is later supported y our p-value < 2.2e-16 which helps us reject the null hypothesis giving us enough evidence that our data is not normally distributed.

#Normality fix test(remove the # in the next line ), if you would prefer to use the parametric tests,but in this dataset I am going to use non-parametric test.
#let us use the square-root transformation 
#dss$salary_in_usd<-sqrt(dss$salary_in_usd)#this fixes the normality,homoginity of variance and the outliers(points which deviates far away from the mean and other observations)



#data visualization of the mean salary based on the job title
salary_summary<-aggregate(salary_in_usd~job_title,data = dss,FUN=mean)#this code calculates the mean of each job title's salary 
salary_summary<-salary_summary[order(-salary_summary$salary_in_usd),]#here we are ordering the salary mean ranges from the highest to lowest,(descending order where we added the - sign for r to know is the descending order ),then saving it out in rows 
top_10<-head(salary_summary,10)#here we are assigning the first top 10 job titles with the highest salary mean the name top 10
num_job_titles<-nrow(top_10) #this code is used to determine the number of colors to be generated based on the number of the job titles indicated
colors<-rainbow(num_job_titles)
par(mar=c(5,10,5,5))
barplot(top_10$salary_in_usd,names.arg = top_10$job_title,
        main = "Mean Salary by Job Title",
        xlab="Mean Salary",
        ylab="",
        col = colors,
        horiz = TRUE,
        las=2,#this line makes the job titles to be in a horizontal layout
        las=1,#this line makes the  mean salary to be in a horizontal layout
        cex.names = 0.7,#this reduces the size of the labels
)

dev.off()
#data visualization of mean salary via job experience
salary_summary<-aggregate(salary_in_usd~experience_level,data = dss,FUN=mean)#this code calculates the mean of each Experience level's salary 
num_experience_level<-nrow(salary_summary)
colors<-rainbow(num_experience_level)
barplot(salary_summary$salary_in_usd,names.arg = salary_summary$experience_level,
        main = "Mean Salary by Experience level",
        xlab="Experience Level",
        ylab="Mean Salary",
        col = colors)

#visualization of salary by employment type using barcharts
salarymean<-aggregate(salary_in_usd~employment_type,data=dss,FUN=mean)
colors1<-rainbow(nrow(salarymean))
barplot(salarymean$salary_in_usd,names.arg =salarymean$employment_type,main = "Mean salary by Employment type",xlab = "Employment Type",ylab="Mean Salary",col=colors1) #names.arg prints out the names of our categorical data at the x axis

#data visualization (boxplot) for the type of employment to salary
boxplot(salary_in_usd~employment_type,data=dss,col=colors1,outline=F,ann=T,main="type of employment to salary",frame = FALSE)#the outline function when its set to false it omits the outliers


#barchart visualization for type of company size located in different locations in our data 
dss<-dss%>%group_by(company_size)%>%mutate(percent_company_locationcat=sprintf("%.1f%%",100*prop.table(table(company_locationcat))[company_locationcat])) #we start by piping the data,we then group the data by company size so we can do operation in each group separately,ie the percentage,we piped it to the next operation where we mutate it,the sprintf(%.1f%%) formats the percentage as a string with one decimal place,table(company_locationcat) creates a frequency table(percent_company_locationcat) of company_locationcat in each group and 100*prop.table converts the frequency table to proportions then multiplies it with 100 to make it into percentage.

ggplot(dss,aes(company_size,fill=company_locationcat))+
  geom_bar(position = "dodge",alpha=0.7)+
  labs(title = "Company location to company size barchart",
       x="Company size",
       y="Company location",
       fill="Company location")+
  geom_text(aes(label=percent_company_locationcat,y=..count..),stat=
              "count",position=position_dodge(width=0.9),vjust=-0.5) #ggplot()function is where we 1st writes the data we want to work with, then sets the aesthetics ie the x an y axis,the fill in the y axis tells r to fill in different type of colours of its choice,for the geom_bar(),position function tells r where we want the 2 groups (within one group) to be places,and with dogde,they are placed aside,alpha function sets the visibility of your graph,labs functionis where we add test to our graph,ie, title and x and y axis,in  geom_text, label=percent_company_locationcat shows that the labels for the text shoud come from this variable,y=..count.. sets the vertical position of the labels to the count of observations in each group,stat="count" sets the statistical transformation to be used, where we are using count, position=position_dodge(width=0.9) sets the position of the text label to be on each bar with the given width, vjust=-0.5 adjusts the vertical positioning of the text labels,the - means the text to move above the bars


#Barchart visualization of experience level of the employees to work models
ggplot(dss,aes(work_models,fill=experience_levelcat))+
  geom_bar(alpha=0.5,position="dodge")+
  labs(title="Barchart for experience level and the work model",
       x="Work models",y="Experience level",fill="Experience level")

#barchart visualization for the employees type of experience levels hired in different sizes of companies
dss<-dss%>%group_by(company_size)%>%mutate(percent_experience_levelcat=sprintf("%.1f%%",100*prop.table(table(experience_levelcat))[experience_levelcat]))

ggplot(dss,aes(company_size,fill=experience_levelcat))+
  geom_bar(alpha=0.5,position="dodge")+
  labs(title="Barchart for Experience level and Company size",
       x="Company size",y="Experience level",fill="Experience level")+
  geom_text(aes(label=percent_experience_levelcat,y=..count..),stat=
              "count",position=position_dodge(width=0.9),vjust=-0.5)


#map visualization of the salary category to world map

# Install and load necessary packages
if (!require("rnaturalearth")) install.packages("rnaturalearth",dependencies = T)
if (!require("sf")) install.packages("sf",dependencies = T)
if (!require("countrycode")) install.packages("countrycode",dependencies = T)
if (!require("Rcpp"))install.packages("Rcpp", dependencies = TRUE, INSTALL_opts = '--no-lock')
if (!require("maps")) install.packages("maps",dependencies = T)
library(rnaturalearth)
library(sf)
library(countrycode)
library(maps)

unlink("C:/Users/erick/AppData/Local/R/win-library/4.3/00LOCK")
options("install.lock"=FALSE)
devtools::install_github("ropensci/rnaturalearth")



#Map plot for continents

#Africa
# Assuming your salary data is stored in 'salary_by_country'
world_map <- map_data("world")
salary_by_country <- dss %>%
  group_by(employee_residence) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE))

# Merge world map data with salary data
merged_data <- merge(world_map, salary_by_country, by.x = "region", by.y = "employee_residence", all.x = TRUE)
merged_data <- merged_data %>%
  mutate(continent = countrycode(region, origin = "country.name", destination = "continent")) %>%
  filter(continent == "Africa")
par(mar=c(2,2,2,2))
# Plot for Africa
ggplot() +
  geom_map(data = merged_data, map = merged_data,  # Use world_map for mapping
           aes(x = long, y = lat, map_id = region, fill = avg_salary),
           color = "grey", size = 0.2) +
  scale_fill_gradient(name = "Average salary", low ="yellow", high = "darkgreen",
                      na.value = "lightgrey", guide = guide_colorbar(barwidth = 10, barheight = 1)) +
  theme(panel.background = element_rect(fill = "lightblue")) +
  coord_equal() +
  labs(title = "Average Salary in Africa") +
  theme(legend.position = "bottom",
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black"))

#Americans

world_map <- map_data("world")

# Assuming dss is your dataset containing salary data
salary_by_country <- dss %>%
  group_by(employee_residence) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE))

# Merge world map data with salary data
merged_data1 <- merge(world_map, salary_by_country, by.x = "region", by.y = "employee_residence", all.x = TRUE)
merged_data1 <- merged_data1 %>%
  mutate(continent = countrycode(region, origin = "country.name", destination = "continent")) %>%
  filter(continent == "Americas")

# Plot for Americas
ggplot() +
  geom_map(data = merged_data1, map = merged_data1,  # Use world_map for mapping
           aes(x = long, y = lat, map_id = region, fill = avg_salary), size = 0.2) +
  scale_fill_gradient(name = "Average salary", low ="yellow", high = "darkgreen",
                      na.value = "lightgrey", guide = guide_colorbar(barwidth = 10, barheight = 1)) +
  theme(panel.background = element_rect(fill = "lightblue")) +
  coord_equal() +
  labs(title = "Average Salary in America") +
  theme(legend.position = "bottom",
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black"))


# Asia

world_map <- map_data("world")

# Assuming dss is your dataset containing salary data
salary_by_country <- dss %>%
  group_by(employee_residence) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE))

# Merge world map data with salary data
merged_data2 <- merge(world_map, salary_by_country, by.x = "region", by.y = "employee_residence", all.x = TRUE)
merged_data2 <- merged_data2 %>%
  mutate(continent = countrycode(region, origin = "country.name", destination = "continent")) %>%
  filter(continent == "Asia")

# Plot for  Asia
ggplot() +
  geom_map(data = merged_data2, map = merged_data2,  # Use world_map for mapping
           aes(x = long, y = lat, map_id = region, fill = avg_salary),
           size = 0.2) +
  scale_fill_gradient(name = "Average salary", low ="yellow", high = "darkgreen",
                      na.value = "lightgrey", guide = guide_colorbar(barwidth = 10, barheight = 1)) +
  theme(panel.background = element_rect(fill = "lightblue")) +
  coord_equal() +
  labs(title = "Average Salary in Asia") +
  theme(legend.position = "bottom",
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black"))



#Europe

world_map <- map_data("world")

# Assuming dss is your dataset containing salary data
salary_by_country <- dss %>%
  group_by(employee_residence) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE))

# Merge world map data with salary data
merged_data3 <- merge(world_map, salary_by_country, by.x = "region", by.y = "employee_residence", all.x = TRUE)
merged_data3 <- merged_data3 %>%
  mutate(continent = countrycode(region, origin = "country.name", destination = "continent")) %>%
  filter(continent == "Europe")

# Plot for Europe
ggplot() +
  geom_map(data = merged_data3, map = merged_data3,  # Use world_map for mapping
           aes(x = long, y = lat, map_id = region, fill = avg_salary), size = 0.2) +
  scale_fill_gradient(name = "Average salary", low ="yellow", high = "darkgreen",
                      na.value = "lightgrey", guide = guide_colorbar(barwidth = 10, barheight = 1)) +
  theme(panel.background = element_rect(fill = "lightblue")) +
  coord_equal() +
  labs(title = "Average Salary in Europe") +
  theme(legend.position = "bottom",
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black"))





#Oceania

world_map <- map_data("world")

# Assuming dss is your dataset containing salary data
salary_by_country <- dss %>%
  group_by(employee_residence) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE))

# Merge world map data with salary data
merged_data4 <- merge(world_map, salary_by_country, by.x = "region", by.y = "employee_residence", all.x = TRUE)
merged_data4 <- merged_data4 %>%
  mutate(continent = countrycode(region, origin = "country.name", destination = "continent")) %>%
  filter(continent == "Oceania")

# Plot for Oceania
ggplot() +
  geom_map(data = merged_data4, map = merged_data4,  # Use world_map for mapping
           aes(x = long, y = lat, map_id = region, fill = avg_salary),
           size = 0.2) +
  scale_fill_gradient(name = "Average salary", low ="yellow", high = "darkgreen",
                      na.value = "lightgrey", guide = guide_colorbar(barwidth = 10, barheight = 1)) +
  theme(panel.background = element_rect(fill = "lightblue")) +
  coord_equal() +
  labs(title = "Average Salary in Oceania") +
  theme(legend.position = "bottom",
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black"))




#Statistical Analysis Tests

######### Non-Parametric Tests

#Chi-square test
cst1<-table(dss$salary_in_usdcat,dss$experience_levelcat) #creating a contingency table
summary(cst1)
#when the df=1,calculated by =(number of rows -1)*(number of columns-1),where in our case is (2-1)*(2-1)=1

cq1<-chisq.test(cst1,correct=F)
cq1
#the p-value  < 2.2e-16 being less than 5% means that we reject the null hypothesis,this means that there is a relationship between salary category and experience level,thus there is a statistically significant difference in the distribution of salary among different experience levels

cst2<-table(dss$salary_in_usdcat,dss$jobttcat)
summary(cst2)
#when the df=1,calculated by =(number of rows -1)*(number of columns-1),where in our case is (2-1)*(2-1)=1

cq2<-chisq.test(cst2,correct=F)
cq2
#the p-value  = 7.3e-05 being less than 5% means that we reject the null  hypothesis,this means that there is a relationship between the job title and the salary, thus there is significant difference in salary among different type of job titles

cst3<-table(dss$salary_in_usdcat,dss$employment_typecat)
summary(cst3)
#when the df=1,calculated by =(number of rows -1)*(number of columns-1),where in our case is (2-1)*(2-1)=1

cq3<-chisq.test(cst3,correct=F)
cq3
#the p-value  = 1.26e-08 being less than 5% means that we reject the null  hypothesis,this means that there is a relationship between the employment type and the salary paid to an employee,thus  there is significant difference in salary among different type of employment among the employees


cst4<-table(dss$salary_in_usdcat,dss$company_locationcat)
summary(cst4)
#when the df=1,calculated by =(number of rows -1)*(number of columns-1),where in our case is (2-1)*(2-1)=1

cq4<-chisq.test(cst4,correct=F)
cq4
#the p-value  < 2.2e-16 being less than 5% means that we reject the null hypothesis,this means that there is a relationship between salary  and the company location,thus there is a statistically significant difference between the company's location and the distribution of salary paid to employees 

cst5<-table(dss$jobttcat,dss$work_yearcat)
summary(cst5)
#when the df=1,calculated by =(number of rows -1)*(number of columns-1),where in our case is (2-1)*(2-1)=1

cq5<-chisq.test(cst5,correct=F)
cq5
#the p-value = 0.007319 being less than 5% means that we reject the null hypothesis,this means that there is a relationship between salary  and the work year(the year the salary was paid),thus there is a statistically significant difference between the year the salary was paid and the distribution of salary paid to employees 
cst6<-table(dss$salary_in_usdcat,dss$work_modelscat)
summary(cst6)
#when the df=1,calculated by =(number of rows -1)*(number of columns-1),where in our case is (2-1)*(2-1)=1

cq6<-chisq.test(cst6,correct=F)
cq6
#the p-value = 8.884e-09 being less than 5% means that we reject the null hypothesis,this means that there is a relationship between salary  and the work model(the type of work arrangement),thus there is a statistically significant difference between the work arrangement and the distribution of salary paid to employees 

cst7<-table(dss$salary_in_usdcat,dss$company_size)
summary(cst7)
#when the df=2,calculated by =(number of rows -1)*(number of columns-1),where in our case is (2-1)*(3-1)=2

cq7<-chisq.test(cst7,correct=F)
cq7
#the p-value < 2.2e-16 being less than 5% means that we reject the null hypothesis,this means that there is a relationship between salary  and the company size,thus there is a statistically significant difference between the size of the company and the distribution of salary paid to employees


#mann whitney u test

#comparing the salary between the job titles category
wilcox.test(salary_in_usd~jobttcat,conf.int=T,conf.level=0.95,data=dss)
#the p-value = 0.0001831 which is less that 5%,means that we reject the null hypothesis,thus its statistically significant,the 95% CI also supports the null hypothesis because zero  does not lie in between the given ranges

#comparing the salary between the  experience type category
wilcox.test(salary_in_usd~experience_levelcat,conf.int=T,conf.level=0.95,data=dss)
#the p-value < 2.2e-16 which is less that 5%,means that we reject the null hypothesis,thus its statistically significant,the 95% CI also supports the null hypothesis because zero  does not lie in between the given ranges


#comparing the salary between the company location category
wilcox.test(salary_in_usd~company_locationcat,conf.int=T,conf.level=0.95,data=dss)
#the p-value < 2.2e-16 which is less that 5%,means that we reject the null hypothesis,thus its statistically significant,the 95% CI also supports the null hypothesis because zero  does not lie in between the given ranges

#comparing the salary between the employee's residence category
wilcox.test(salary_in_usd~employee_residencecat,conf.int=T,conf.level=0.95,data=dss)
#the p-value < 2.2e-16 which is less that 5%,means that we reject the null hypothesis,thus its statistically significant,the 95% CI also supports the null hypothesis because zero  does not lie in between the given ranges

#comparing the salary between the work model category
wilcox.test(salary_in_usd~work_modelscat,conf.int=T,conf.level=0.95,data=dss)
#the p-value < 2.2e-16 which is less that 5%,means that we reject the null hypothesis,thus its statistically significant,the 95% CI also supports the null hypothesis because zero  does not lie in between the given ranges

#comparing the salary between the work year  category
wilcox.test(salary_in_usd~work_yearcat,conf.int=T,conf.level=0.95,data=dss)
#the p-value < 2.2e-16 which is less that 5%,means that we reject the null hypothesis,thus its statistically significant,the 95% CI also supports the null hypothesis because zero  does not lie in between the given ranges

#comparing the salary between the employment type category
wilcox.test(salary_in_usd~employment_typecat,conf.int=T,conf.level=0.95,data=dss)
#the p-value = 1.072e-13 which is less that 5%,means that we reject the null hypothesis,thus its statistically significant,the 95% CI also supports the null hypothesis because zero  does not lie in between the given ranges



#fishers test (test for association)
levels(dss$jobttcat)
levels(dss$salary_in_usdcat)
#test for association between salary category and job category
fisher.test(table(dss$salary_in_usdcat,dss$jobttcat))
#the p-value = 7.573e-05 which is less that 5%,means that we reject the null hypothesis,thus its statistically significant,the 95% CI also supports the null hypothesis because zero  does not lie in between the given ranges.In odds ratio explanation,we first recognize the reference categories for the both characteristic variables as done above in line 513 and 514,the the other category is rated to the reference category,in our case, With an odds ratio of  0.7806202,this means that there is a less likelihood ( 0.7806202) that individuals who have other jobs titles earn more than USD 138000 compared to data scientists.

#test for association between salary category and experience level
levels(dss$experience_levelcat)
fisher.test(table(dss$salary_in_usdcat,dss$experience_levelcat))
#the p-value < 2.2e-16,which is less than 5%, indicates that we reject the null hypothesis,by this the test is statistically significant, also,the 95% CI supports our previous argument because zero  does not fall in the range,having the odds ratio as 4.762031,and the reference category for salarycat and experiencecat being below USD 138000 and Learner respectifully,we conclude that individuals who are experts are significantly more likely to have a salary which is above USD 138000 compared to ones who are experts.   


#test for association between salary category and employment type
levels(dss$employment_typecat)
fisher.test(table(dss$salary_in_usdcat,dss$employment_typecat))
#the p-value = 2.493e-09 being less than 5% suggests that we reject the null hypothesis,meaning that the test is statistically significant,also the 95%CI supports our argument because zero  does not lie inside its ranges,the odds ratio 0.09243705 suggests that individuals that are employed part time are less likely to have a salary above USD 138000 compared to ones employed fulltime.

#test for association between salary category and employee's residence
levels(dss$employee_residencecat)
fisher.test(table(dss$salary_in_usdcat,dss$employee_residencecat))
#The p-value < 2.2e-16 being less than 5% suggests that we reject the null hypothesis,meaning that the test is statistically significant,also the 95%CI supports our argument because zero  does not lie inside its ranges,the odds ratio  11.89316,suggests that employees who reside in the west are statistically more likely to earn a salary above USD 138000 compared to those who reside in the East.

#test for association between salary category and company's location category
levels(dss$company_locationcat)
fisher.test(table(dss$salary_in_usdcat,dss$company_locationcat))
#The p-value < 2.2e-16 being less than 5% suggests that we reject the null hypothesis,meaning that the test is statistically significant,also the 95%CI supports our argument because zero  does not lie inside its ranges,the odds ratio 10.6419 ,suggests that companies that reside in the west are statistically more likely to be paying their employees a salary above USD 138000 compared to those that are in the East.

#test for association between salary category and work model category
levels(dss$work_modelscat)
fisher.test(table(dss$salary_in_usdcat,dss$work_modelscat))
#The p-value = 9.848e-09 being less than 5% suggests that we reject the null hypothesis,meaning that the test is statistically significant,also the 95%CI supports our argument because zero  does not lie inside its ranges,the odds ratio 1.33419 ,suggests that employees with not flexible employment are statistically more likely to be earning a salary above USD 138000 compared to those who employment is flexible.


#test for association between salary category and work year
levels(dss$work_yearcat)
fisher.test(table(dss$salary_in_usdcat,dss$work_yearcat))
#The p-value < 2.2e-16 being less than 5% suggests that we reject the null hypothesis,meaning that the test is statistically significant,also the 95%CI supports our argument because zero  does not lie inside its ranges,the odds ratio 1.734689,suggests that employees that were paid after 2022 are statistically more likely to be earning a salary above USD 138000 compared to those that were paid before 2022.




#correlation(testing for any relationship)
#we use point-biserial correlation(used to test relationship between nominal categorical data and a continous data)
if(!require(polycor))install.packages("polycor",dependencies = T)
library(polycor)

#correlation of salary to job titles 
dss$jobttcat2=as.numeric(dss$jobttcat)
cor.test(dss$salary_in_usd,dss$jobttcat2)
#the job title and salary have a weak negative correlation(r= -0.02729048),this means that,a increase in one,corresponds to a decrease in the other,in our case,data scientists(encoded 0) have more salary than other job titles(encoded 1),the p-value = 0.02733,which is less than 5%(we reject the null hypothesis), gives us the confidence to say that the test is statistically significant,also looking at our 95% CI, zero does not lie in between its ranges.

#correlation of salary to experience level
dss$experience_levelcat2<-as.numeric(dss$experience_levelcat)
cor.test(dss$salary_in_usd,dss$experience_levelcat2)
#the relationship between salary and experience level is a moderate positive correlation(r=0.3547685),which means that,employees who are experts(encoded as 1) tends to be paid higher than learners(encoded as 0).The p-value < 2.2e-16,which is less than 5%(we reject the null hypothesis), gives us the confidence to say that the test is statistically significant,also looking at our 95% CI, zero does not lie in between its ranges.

#correlation of salary to employment type
dss$employment_typecat2<-as.numeric(dss$employment_typecat)
cor.test(dss$salary_in_usd,dss$employment_typecat2)
##the relationship between salary and employment type  is a weak negative  correlation(r=-0.07894822),which means that,employees who are employed in part-time bases(encoded as 1) tends to be paid less than those who are employed full-time(encoded as 0).The p-value = 1.628e-10,which is less than 5%(we reject the null hypothesis), gives us the confidence to say that the test is statistically significant,also looking at our 95% CI, zero does not lie in between its ranges.

#correlation of salary to work models
dss$work_modelscat2<-as.numeric(dss$work_modelscat)
cor.test(dss$salary_in_usd,dss$work_modelscat2)
#the relationship between salary and work model is a weak positive correlation(r=0.1155592),which means that,employees who are on non-flexible employment plan  (encoded as 1) tends to be paid higher than those with flexible employment plan (encoded as 0).The p-value < 2.2e-16,which is less than 5%(we reject the null hypothesis), gives us the confidence to say that the test is statistically significant,also looking at our 95% CI, zero does not lie in between its ranges.

#Correlation of salary to work year
dss$work_yearcat2<-as.numeric(dss$work_yearcat)
cor.test(dss$salary_in_usd,dss$work_yearcat2)
#the relationship between salary and work year is a weak positive correlation(r=0.1474666),which means that,employees who were paid after 2022 (encoded as 1) tends to be paid higher than those who were paid before 2022 (encoded as 0).The p-value < 2.2e-16,which is less than 5%(we reject the null hypothesis), gives us the confidence to say that the test is statistically significant,also looking at our 95% CI, zero does not lie in between its ranges.


#Correlation of salary to employee residence
dss$employee_residencecat2<-as.numeric(dss$employee_residencecat)
cor.test(dss$salary_in_usd,dss$employee_residencecat2)
#the relationship between salary and employee's residence is a weak positive correlation(r=0.1840217),which means that,employees who reside in the west part continents (encoded as 1) tends to be paid higher than those who reside in the east part continents (encoded as 0).The p-value < 2.2e-16,which is less than 5%(we reject the null hypothesis), gives us the confidence to say that the test is statistically significant,also looking at our 95% CI, zero does not lie in between its ranges.


#Correlation of salary to company location
dss$company_locationcat2<-as.numeric(dss$company_locationcat)
cor.test(dss$salary_in_usd,dss$company_locationcat2)
#the relationship between salary and company's location is a weak positive correlation(r=0.1645287),which means that,companies located in the west part continents (encoded as 1) tends to pay employees higher than those located in the east part continents (encoded as 0).The p-value < 2.2e-16,which is less than 5%(we reject the null hypothesis), gives us the confidence to say that the test is statistically significant,also looking at our 95% CI, zero does not lie in between its ranges.

table(dss$company_sizecat)
#Correlation of salary to company size
dss$company_sizecat2<-as.numeric(dss$company_sizecat)
cor.test(dss$salary_in_usd,dss$company_sizecat2)
#The p-value < 2.2e-16,which is less than 5%(we reject the null hypothesis), gives us the confidence to say that the test is statistically significant,also looking at our 95% CI, zero does not lie in between its ranges.The relationship between salary and company's size is a very weak positive linear  correlation(r=0.02768125),which means that,as company size increases,so do the salary paid to the employees,but the relationship is not strong enough to make meaningful predictions about salary based solely on company size.

#Machine learning
#Linear regression
if(!require(caTools))install.packages("caTools",dependencies = T)
library(caTools)
#we first create a dataset for the variables to be used in machine learning
dss<-subset(dss,select=c("salary_in_usdcat","jobttcat","employment_typecat","employee_residencecat","company_locationcat","company_sizecat","experience_levelcat","work_modelscat","work_yearcat"))

dim(dss)

set.seed(2)#here we are telling R that we want to use the same type of sample selected form the random sequence used
split<-sample.split(dss$salary_in_usdcat,SplitRatio = 0.7)#here we have split the dataset into 70-30 groups using the sample.split function found in caTool package,In this code snippet, dss2$salary_in_usdcat is used as the splitting variable, meaning the data will be split based on the values of the salary_in_usdcat column in the dss2 dataset. This column is likely chosen as the splitting variable because it may contain categorical or continuous data that is relevant to the analysis or problem being addressed.
split
prop.table(table(split))# checking the proportion of the data

train<-subset(dss,split==TRUE)#we assign the 70% sample (TRUE)to train,we will use this to train our data,notice that the TRUE output is logical,not a string
train
View(train)
str(train)
dim(train)
test<-subset(dss,split==FALSE)#we assign the remaining 30% sample (FALSE)to test,we will use this to test our data
test
View(test)


model=glm(salary_in_usdcat~.,data=train,family=binomial(link="logit"))
summary(model)

#let write some code to  help us view the model
if(!require(gtsummary))install.packages("gtsummary",dependencies = T)
library(gtsummary)
tbl=tbl_regression(model,exponentiate=TRUE,intercept=TRUE)
tbl

#prediction
prediction<-predict(model,test,type = "response")
prediction

prediction<-predict(model,train,type = "response")
prediction


cm<-table(Actual_values=train$salary_in_usdcat,Predicted_values=prediction>0.5)
cm

#accuracy
accuracy <- (cm[1, 1] + cm[2, 2]) / sum(cm)
accuracy# the accuracy is 67.13%,let try another model to se if we get a higher accuracy level



#modeling using decision tree
if(!require(caret))install.packages("caret",dependencies = T)
library(caret)

if(!require(data.tree))install.packages("data.tree",dependencies = T)
library(data.tree)

if(!require(rpart.plot))install.packages("rpart.plot",dependencies = T)
library(rpart.plot)

if(!require(rpart))install.packages("rpart",dependencies = T)
library(rpart)

if(!require(caTools))install.packages("caTools",dependencies = T)
library(caTools)
#we first create a dataset for the variables to be used in machine learning
dss<-subset(dss,select=c("salary_in_usdcat","jobttcat","employment_typecat","employee_residencecat","company_locationcat","company_sizecat","experience_levelcat","work_modelscat","work_yearcat"))

dim(dss)

set.seed(2)#here we are telling R that we want to use the same type of sample selected form the random sequence used
split<-sample.split(dss$salary_in_usdcat,SplitRatio = 0.7)#here we have split the dataset into 70-30 groups using the sample.split function found in caTool package,In this code snippet, dss2$salary_in_usdcat is used as the splitting variable, meaning the data will be split based on the values of the salary_in_usdcat column in the dss2 dataset. This column is likely chosen as the splitting variable because it may contain categorical or continuous data that is relevant to the analysis or problem being addressed.
split
prop.table(table(split))# checking the proportion of the data

train<-subset(dss,split==TRUE)#we assign the 70% sample (TRUE)to train,we will use this to train our data,notice that the TRUE output is logical,not a string
train
View(train)
str(train)
dim(train)
test<-subset(dss,split==FALSE)#we assign the remaining 30% sample (FALSE)to test,we will use this to test our data
test
View(test)
dim(test)

tree<-rpart(salary_in_usdcat~.,data=train)
tree

#prediction
prediction2<-predict(tree,test,type="class")
prediction2

#Confusion matrix
confusionMatrix(prediction2,test$salary_in_usdcat)
#the accuracy is 65.99% which is less tha the above model,so we try another model

#visualising the decision tree
# Load required packages
library(rpart)
library(rpart.plot)

# Fit a decision tree model
tree_model <- rpart(salary_in_usdcat ~ ., data = test, method = "class")

# Visualize the decision tree
rpart.plot(tree_model, type = 5, extra = 101)




#Random Forest
if(!require("randomForest"))install.packages("randomForest",dependencies = T)
library(randomForest)
#we first create a dataset for the variables to be used in machine learning
dss<-subset(dss,select=c("salary_in_usdcat","jobttcat","employment_typecat","employee_residencecat","company_locationcat","company_sizecat","experience_levelcat","work_modelscat","work_yearcat"))

dim(dss)

set.seed(2)#here we are telling R that we want to use the same type of sample selected form the random sequence used
train_index <- sample(1:nrow(dss), 0.7 * nrow(dss))
train_data <- dss[train_index, ]
test_data <- dss[-train_index, ]
dim(train_data)
dim(test_data)
# Train the random forest model
rf_model <- randomForest(salary_in_usdcat ~ ., data = train_data)

# Print the model summary(confusion matrix)
print(rf_model)#the OOB estimate of the error rate being 33.56%. This means that, on average, the model misclassifies approximately 33.56% of the observations when predicting the outcome variable.
#in the confusion matrix,the diagonal elements of the confusion matrix represent the correctly classified observations (true positives and true negatives), while the off-diagonal elements represent the misclassified observations (false positives and false negatives)
#The class error rates indicate the proportion of misclassified observations for each class:For the "Below USD 138000" class, the error rate is approximately 47.25%.For the "Above USD 138000" class, the error rate is approximately 19.44%.


# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model performance
accuracy <- mean(predictions == test_data$salary_in_usdcat)
print(paste("Accuracy:", accuracy))#the model has an accuracy of 69.06% 



#  Gradient Boosting Machines
if(!require("gbm"))install.packages("gbm",dependencies = T)# For Gradient Boosting Machines
library(gbm)
if(!require("caret"))install.packages("caret",dependencies = T)
library(caret)

if (!require("pROC")) install.packages("pROC", dependencies = TRUE)
library(pROC)

# Set up cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Gradient Boosting Machines (GBM)
gbm_model <- train(salary_in_usdcat ~ ., data = train_data, method = "gbm", trControl = ctrl)
gbm_predictions <- predict(gbm_model, newdata = test_data)



# Print the accuracy of the model
gbm_accuracy <- mean(gbm_predictions == test_data$salary_in_usdcat)
gbm_accuracy

# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = gbm_predictions, reference = test_data$salary_in_usdcat)

# Print confusion matrix
print(conf_matrix)

#The top-left cell (525) indicates the number of observations that are actually in the class "Below USD 138000" and were predicted to be in the same class, (429) indicates the number of observations that are actually in the class "Above USD 138000" but were predicted to be in the "Below USD 138000" class.(177) indicates the number of observations that are actually in the class "Below USD 138000" but were predicted to be in the "Above USD 138000" class. (831) indicates the number of observations that are actually in the class "Above USD 138000" and were predicted to be in the same class
#the accuracy(69.11%) is a little better than the other models, so we use this model for prediction, the p-value < 2.2e-16 which is less than 5% gives us the confidence to reject the null hypothesis,also the 95%CI supports our argument
# Cohen's Kappa statistic, which measures the agreement between the model's predictions and the actual classes, corrected for agreement by chance. In this case, it's approximately 0.3774, indicating moderate agreement


#End of analysis 


