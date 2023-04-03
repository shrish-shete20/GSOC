#*********************************************************#          

##################### GROUP 19 ~ COVID 19 #################

#********************************************************#

#install.packages("xts")
library(rvest)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(dplyr)
library(xts)
library(leaflet)

#########################
######DATA SCRAPPING#####
#########################

#covid cases
covid_table <-
  read_html("https://www.worldometers.info/coronavirus/") %>% html_table()
covid_country <- covid_table[[1]][["Country,Other"]][9:237]
total_cases <-
  covid_table[[1]][["TotalCases"]][9:237] %>% gsub(pattern = ',', replacement = '') %>%
  as.numeric()
Total_deaths <-
  covid_table[[1]][["TotalDeaths"]][9:237] %>% gsub(pattern = ',', replacement = '') %>%
  as.numeric()
population <-
  read_html(
    "https://ceoworld.biz/2022/03/28/list-of-countries-and-territories-with-the-largest-population-2022/"
  ) %>% html_table()

covid_country[1] <- "United States"        #setting country as per data available
covid_country[6] <- "South Korea"
covid_country[7] <- "United Kingdom"

countries <- population[[1]][["Country"]]

n <- length(countries)
cov_pop <- numeric(229)
case_pop <- numeric(229)
for (i in 1:229)
{
  for (j in 1:n)
  {
    if (covid_country[i] == countries[j])
    {
      cov_pop[i] <-
        population[[1]][["2022 Population"]][j]         #population is in billion
    }
  }
}
for (i in 1:229)
{
  case_pop[i] <- total_cases[i] / (cov_pop[i] * 1000)
}


#Total population
populations <- cov_pop * 1000


# GDP Economy
gdp <-
  read_html("https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)_per_capita") %>% html_table()
coun <- as.data.frame(gdp[[1]][[1]][3:219])

country = character(229)

for (i in 1:217)
{
  country[i] <-
    substring(coun[i, 1], first = 0, last = nchar(coun[i, 1]) -
                2)
}

gdp <- as.data.frame(gdp[[1]][5])
gdp_new <-
  gdp[3:219, 1] %>% gsub(pattern = ",", replacement = "") %>% as.numeric()
gdp_cov <- numeric(229)

for (i in 1:229)
{
  for (j in 1:217)
  {
    if (covid_country[i] == country[j])
    {
      gdp_cov[i] <- gdp_new[j]
      #UN gdp of correspoinding countries 2020
    }
  }
}



#Happiness Index
happy <-
  read_html("https://dmerharyana.org/world-happiness-index/") %>% html_table()
happy <- as.data.frame(happy[[1]])

happy <- happy[, 2:3]

performance = numeric(229)
for (i in 1:229)
{
  for (j in 1:146)
  {
    if (covid_country[i] == happy[j, 1])
    {
      performance[i] <- happy[j, 2]    #happiness index
    }
  }
}


#Life Expectancy
life <-
  read_html("https://www.worldometers.info/demographics/life-expectancy/") %>% html_table()
life <- as.data.frame(life[[1]])
life <- life[, 2:3]
Life_expectancy <- numeric(229)
for (i in 1:229)
{
  for (j in 1:202)
  {
    if (covid_country[i] == life[j, 1])
    {
      Life_expectancy[i] <- life[j, 2] #life expectancy of each country
    }
  }
}

#Number of tests 
x <- read.csv("testing.csv")
countr <- unique(x$Entity)
tests <- numeric(188)
for (j in 1 : 188)
{
  y <- which (x$Entity == countr[j])
  tests[j] <- max(x$total_tests[y]) 
}
Number_of_tests <- numeric(229)

for (i in 1 : 229) 
{
  for (j in 1 : 188)
  {
    if (covid_country[i] == countr[j])
    {
      Number_of_tests[i] <- tests[j]
    }
  }
}


Rank <- c(1:229)

Country_name <- covid_country
cases_per_pop <- case_pop
GDP_per_capita_income_in_US_dollars <- gdp_cov
Happiness_index <- performance
Total_number_of_cases <- total_cases
Population_of_the_country <- populations
death_rate <- Total_deaths / Total_number_of_cases


#Economy as per Happiness report
eco <- read.csv("2022.csv") %>% as.data.frame()
eco_coun <- eco$Country
eco_gdp <- eco$Explained.by..GDP.per.capita
eco_vec <- numeric(229)
for (i in 1:229)
{
  for (j in 1:146)
  {
    if (covid_country[i] == eco_coun[j])
    {
      eco_vec[i] = eco_gdp[j]
    }
  }
}
eco_vec <- gsub(eco_vec, pattern = ",", replacement = '') %>% as.numeric()


####Dataframe####

covid_report <-
  data.frame(
    Rank,
    Country_name,
    Population_of_the_country,
    Number_of_tests,
    Total_number_of_cases,
    Total_deaths,
    death_rate ,
    cases_per_pop,
    GDP_per_capita_income_in_US_dollars,
    eco_vec,
    Happiness_index,
    Life_expectancy
  )

x = which(covid_report$Happiness_index == 0.000)
y = which(covid_report$GDP_per_capita_income_in_US_dollars == 0)
z = which(covid_report$eco_vec == 0)
a = which(covid_report$Life_expectancy == 0)
b = which(covid_report$Number_of_tests == 0)
covid_report = covid_report[-c(x, y, z, a,b),]
covid_report = covid_report[complete.cases(covid_report),]






###########################
######VISUALISATIONS#######
###########################
  

#1) geographical distribution of vaccines
CountryVaccinations <- read.csv("CountryVaccinations.csv")
CountryVaccinations<-CountryVaccinations%>%mutate(popup_info=paste("Name of Country:",country,"<br/>","ISO_Code:",iso_code,"<br/>","Name of the Vaccines Distributed:",vaccines,"<br/>","Amount of People Vaccinated:",people_vaccinated,"<br/>","Amount of People Fully Vaccinated:",people_fully_vaccinated))
CountryVaccinations$popup_info
colors<-c("red","navy")
pal<-colorBin(palette = colors,CountryVaccinations$people_vaccinated)
leaflet()%>%addProviderTiles(provider = "Esri.NatGeoWorldMap")%>%addCircleMarkers(data=CountryVaccinations,lat = ~lat,lng = ~long,radius = ~6,popup = ~popup_info,color = ~pal(people_vaccinated),stroke = FALSE,fillOpacity = 1.5)%>% addLegend(position = "bottomright",pal = pal,values = CountryVaccinations$people_vaccinated, title = "AMOUNT OF PEOPLE VACCINATED" , opacity = 1)


#2)Barplot with dual y axis. Plotting Charts and adding a secondary axis
ggp <- ggplot(covid_report[c(1:15),]) +
  geom_bar(aes(x=Country_name, y=GDP_per_capita_income_in_US_dollars),stat="identity", fill="cyan",colour="#006000")+
  geom_point(aes(x=Country_name, y=((Total_deaths/Total_number_of_cases)*1000000)),stat="identity",color="red",size=2)+
  labs(
    title = "**Plot of Gdp vs Total deaths Country wise**  
    <span style='color:#006000;'>  GDP  </span>  ~ 
    <span style='color:red;'>Death rates</span>
    </span>",
    x="country",y="gdp"  ) +  theme_minimal() +
  theme(
    plot.title = element_markdown(lineheight = 1.1))+
   scale_y_continuous(sec.axis=sec_axis(~.*1000,name="death rates"))
ggp



#3)scatter plot 
#following graph represents that as death rate increases life expectancy decreases
ggplot(covid_report, aes(x = Life_expectancy, y = death_rate)) + geom_point() +
  geom_vline(xintercept = mean(covid_report$Life_expectancy)) + geom_smooth(method = "lm", se = FALSE)+labs(title = "Relation between Life Expectancy and Death rate")

#4)following graph represents that as death rate increases gdp decreases implies
#country with better gdp has better medical facilites
ggplot(covid_report, aes(x = eco_vec, y = death_rate)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)+labs(title = "Relation between GDP and Death rate")


#5)Is the country with more population has the more risk of deaths?
Q = quantile(covid_report$Population_of_the_country)
Q1= Q[2]
Q2 = Q[4]
Q3 = Q[5]
pop_category<-numeric(105)
for(i in 1:105)
{
  if(covid_report$Population_of_the_country [i]<= Q1)
  {
    pop_category[i]=1
  }
  else if(covid_report$Population_of_the_country[i]>Q1&  covid_report$Population_of_the_country[i]<=Q2)
  {
    pop_category[i]=2
  }
  
  else if(covid_report$Population_of_the_country [i]>= Q1)
  {
    pop_category[i] = 3
  }
}
covid_report = cbind(covid_report,pop_category)

plot(covid_report$Total_number_of_cases,covid_report$Total_deaths, pch = 16, xlim = c(0,5000000),ylim = c(0,70000),
     col = unique(pop_category),
     xlab = "Total No. of Cases", ylab = "Total No. of Deaths", main = "Is the country with more population has the more risk of deaths?")

for(i in unique(pop_category)) #1:black #2:pink #3:green
{
  foo <- subset(covid_report, covid_report$pop_category == i)
  abline(lm(foo$Total_deaths ~ foo$Total_number_of_cases), col = i)
}
legend("topright", col = unique(covid_report$pop_category), 
       legend = c("High","Medium","Low"),
       title = "Populations" ,cex=1,pch =16,pt.cex = 1.4)


#6)Pie chart: AGE WISE DISTRIBUTION OF DEATHS DUE TO COVID 19
age65 <- 693887100
age18 <- 351135400
age0 <- 9733000
age_sum <- sum(age0, age18, age65)
age0_per <- as.integer((age0 / age_sum) * 100) + 1
age18_per <- as.integer((age18 / age_sum) * 100) + 1
age65_per <- as.integer((age65 / age_sum) * 100) + 1
age <-
  data.frame(
    category = c("0-17", "18-65", "65+"),
    no_of_people = c(age0_per, age18_per, age65_per)
  )
ggplot(age, aes(x = "", y = no_of_people, fill = category)) +
  geom_col(color = "black") + geom_text(aes(label = no_of_people ),
                                        position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+labs(title = "AGE WISE DISTRIBUTION OF DEATHS DUE TO COVID 19")


#7)Bubble plot :How No. of tests per population varies with the GDP of the country with size of the bubble depicting the population of the country
ggplot(covid_report,aes(x=GDP_per_capita_income_in_US_dollars,y=(Number_of_tests/Population_of_the_country) ,size=(Population_of_the_country/1e7)))+geom_point(show.legend = TRUE, alpha = 0.7,colour = "orange")+ylim(0,12)+scale_x_log10()+scale_size(range=c(0.1,20),name = "POPULATION") +labs(y = "No. of tests per population" , x = "GDP per capita income in US dollars" , title = "No. of tests per population vs GDP of the country with size of the bubble depicting the population of the country")



#********************************************************#
#********************************************************#

