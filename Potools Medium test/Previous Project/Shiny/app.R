#*********************************************************#          

##################### GROUP 19 ~ COVID 19 #################
####################     SHINY APP        #################

#********************************************************#


install.packages("xts")

library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(leaflet)
library(ggplot2)
library(ggthemes)
library(ggtext)
library(ggcorrplot)
library(htmlwidgets)
library(reshape2)
library(xts)
library(rvest)
library(plotly)
library(DT)
library(fontawesome)
require(devtools)


####code for data scrapping####

CountryVaccinations = read.csv("CountryVaccinations.csv")
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

covid_country[1] <- "United States"
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


#economy
gdp <-
  read_html("https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)_per_capita") %>% html_table()
coun <- as.data.frame(gdp[[2]][[1]][3:225])

country = character(223)

for (i in 1:217)
{
  country[i] <-
    substring(coun[i, 1], first = 0, last = nchar(coun[i, 1]) -
                2)
}

gdp <- as.data.frame(gdp[[2]][5])
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



#happiness
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


#expectancy
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

#number of tests 
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


#Dataframe
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


##shiny app requirements Live Data
live_data = read_html("https://www.worldometers.info/coronavirus/")
Cases = live_data%>% html_elements(".maincounter-number")%>% html_text()
Cases = gsub("\n","",Cases)
ui <- shinyUI(

###shiny App###
  
  dashboardPage(skin="black",
                dashboardHeader(title="ANALYSIS ON COVID-19",titleWidth=320),
                dashboardSidebar(width=320,
                                 sidebarMenu( id="tabs",
                                              menuItem("Home Page", tabName = "homepage"),  
                                              menuSubItem("Geographical Distribution of COVID-19 Vaccines",tabNam="first",icon = icon("project-diagram")),
                                              menuSubItem("gdp vs total deaths",tabName="second",icon = icon("chart-line")),
                                              menuItem("Covid Death relationship with the indicators",tabName="third",icon = icon("hourglass-start"),
                                                       menuSubItem("Covid death rate vs Life Expectancy",tabName = "chart1"  ),
                                                       menuSubItem("Covid death rate vs GDP",tabName = "chart2")),
                                              menuSubItem("Does Population matters?",tabName="fourth",icon = icon("circle")),
                                              menuSubItem("Age Wise Distribution of death due to Covid 19",tabName="fifth",icon = icon("cog")),
                                              menuSubItem("Impact of GDP on number of tests",tabName="sixth",icon = icon("chart-line")),
                                               menuSubItem("Dataset",tabName="data",icon = icon("table"))
                                              
                                              
                                 )
                ),
                
                
                dashboardBody(
                  
                  tabItems(
                    
                    ## Home Page ####
                    tabItem(tabName = "homepage",
                            tags$img(src ="homepage.jpg",height = 1100,align ="center",style = "position: absolute; opacity: 0.3"),
                            fluidRow( column(
                              width = 12,
                              
                              HTML(
                                "<center><h1 style = 'color:red;font-weight: bold; border-width:5px;border-style:dotted;border-color:black;font-size: 40px'>Are indirect effects claiming more lives in COVID-19 than we realize?</h1></center>"
                              ), hr(),
                              
                              HTML("
                <center> <p style = 'font-size:15px;color:black;font-weight:bold'>
                 Welcome to our COVID-19 dashboard. We hope you it will give you a different perspective to look into!
                </p>
                  <p>
                   <span style= 'font-weight:bold;font-size:22px'>
                            <div style= 'font-size: 16px;color:green;font-weight:bold'>  What exactly does this app do? </div>
                            </span>
                   </p>
                    <p>
                   <span style= 'font-weight:bold;font-size:18px'>
                            <div style= 'font-size: 16px;color:black;font-weight:bold'>  This Shiny App presents to you the COVID-19 pandemic, analyses of top 130 Countries based on several untold factors through interactive data visualisation  </div>
                            </span>
                   </p>
                <center> <p style = 'font-size:28px;color:black;font-weight:bold'>
                <div style= 'background: orangewhite; 
                               font-size: 28px;
                               color:black;
                               font-weight:bold
                               padding: 10px; 
                               border: 1px solid black; 
                               width : 400px;
                               margin: 1px;'>
<br> Coronavirus Cases: ",{Cases[1]},
                                   "<br>Deaths: ",{Cases[2]},
                                   "<br>Recovered: ",{Cases[3]},"</p></center>"),
                              
                              
                              HTML("
            <p style = 'font-size:15px;color:black;font-weight:bold'>
                 To empower yourself with more information regarding
                 COVID-19, and the important role that the WHO is playing
                 in this pandemic : <a href = https://www.worldometers.info/coronavirus/#countries><u>Covid19-novel-coronavirus
                 </u></a></p>"),
                              HTML("
            <p style = 'font-size:15px;font-weight:bold'> 
    <div style= 'font-size: 18px;color:olive;font-weight:bold'> DATA SCIENCE WITH R :GROUP 19  </div>
      <div style= 'font-size: 15px;color:darkgreen;font-weight:bold'> Paulomi Das : Manav Reddy : Shrish shete :  Biplab Naskar   </div>
            </p></center>")
                              
                            ))),
                    tabItem( 
                      tabName="first",h3("MAP VIEW FOR NUMBER OF PEOPLE VACCINATED & NAME OF THE VACCINES DISTRIBUTED BY THE COUNTRIES AROUND THE GLOBE"),
                      fluidRow(
                        box(solidHeader = T,width = 12,background = "navy",tags$a(href="https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations/locations.csv", "COVID-19 Data Repository", target="_blank"),
                            h5("All data are aggregated by people vaccinated and people fully vaccinated.The repository contains information about vaccine update from the official sites of different country.Likewise, it also reflects name of vaccines distributed by the different countries along with their ISO Code"),
                            selectInput("name",
                                        h4("Select the Name of Country:"), 
                                        choices= unique(CountryVaccinations$country))
                        ),
                        
                        box(solidHeader = TRUE,background = "olive"  ,h4("Table Showing Vaccination Information Of Selected Country"),tableOutput("data"),width=12),
                        
                        box(background = "orange" ,width = 12,tabsetPanel(  
                          tabPanel("Geographical Distribution Of Vaccines", leafletOutput("people_vaccinated"))))
                        
                      )),
                    
                    tabItem(tabName="second",h3("Impact of Global Economy on the coronavirus pandemic"),
                            fluidRow(
                              box(solidHeader = F,background = "blue",
                                  sliderInput("obs", "Number of top countries in covid cases:",
                                              min = 0, max = 131, value = 20
                                  )),
                              
                              box(solidHeader=F,background = "green",plotlyOutput("barplot"),width = 12)
                              
                            )   
                            
                    ),
                    
                    tabItem(tabName="chart1",h3("Story of death rate and life expectancy!"),
                            fluidRow(
                              box(background = "navy", solidHeader = TRUE,width = 12,
                                   box(background = "orange" ,width = 12,tabsetPanel(  
                                    tabPanel("Covid Death Rate vs Life expectancy", plotOutput("model1plot"),width = 12)))
                              )
                              
                            )),
                    tabItem(tabName="chart2",h3("Covid Death Rate and GDP of the Country"),
                            fluidRow(
                              box(background = "navy", solidHeader = TRUE,width = 12,
                                  box(background = "orange" ,width = 12,tabsetPanel(  
                                    tabPanel("Covid Death Rate vs GDP", plotOutput("model2plot"),width = 12)))
                              )
                              
                            )),
                    
                    
                    tabItem(tabName="fourth",h3("Does Population category Matters?"),
                            fluidRow(
                              box(background = "black", solidHeader = TRUE,width = 12,
                                  selectInput("population",
                                              h4("Select the population category"), 
                                              choices= c("Less Population","Medium Population","High Population","All of them")),
                                  box(background = "orange" ,width = 12,tabsetPanel(  
                                    tabPanel("Covid Total No. of cases vs Total no. of deaths", plotOutput("populationplot"),width = 12)))
                              )
                              
                            )),
                    tabItem(tabName = "fifth", h3("Which Age group has been affected the most"),
                            fluidRow(
                              box(background = "navy" , solidHeader = TRUE , width = 12,
                                  box(background = "red" ,width = 12 , tabsetPanel(
                                    tabPanel("Age Wise Distribution of death due to Covid 19",plotOutput("pie"),width = 12)))
                              )
                            )),
                                 
                    tabItem(tabName = "sixth", h3("Impact of GDP on number of tests"),
                            fluidRow(
                              box(background = "green" , solidHeader = TRUE , width = 12,
                                  box(background = "blue" ,width = 12 , tabsetPanel(
                                    tabPanel("Does Population Size and GDP affected Covid tests",plotOutput("bubble"),width = 12)))
                              )
                            )),
                    
                    
                    tabItem(tabName ="data", 
                            fluidRow( 
                              DT::dataTableOutput("covidtable") 
                            ) 
                    )
                  ))))


server <- function(input,output){
  
  output$data <- renderTable({
    countryFilter <- subset(CountryVaccinations,CountryVaccinations$country==input$name)
   
    })
  
     output$people_vaccinated <- renderLeaflet({
       CountryVaccinations<-CountryVaccinations%>%mutate(popup_info=paste("Name of Country:",country,"<br/>","ISO_Code:",iso_code,"<br/>","Name of the Vaccines Distributed:",vaccines,"<br/>","Amount of People Vaccinated:",people_vaccinated,"<br/>","Amount of People Fully Vaccinated:",people_fully_vaccinated))
       CountryVaccinations$popup_info
       colors<-c("red","navy")
       pal<-colorBin(palette = colors,CountryVaccinations$people_vaccinated)
       leaflet()%>%addProviderTiles(provider = "Esri.NatGeoWorldMap")%>%addCircleMarkers(data=CountryVaccinations,lat = ~lat,lng = ~long,radius = ~6,popup = ~popup_info,color = ~pal(people_vaccinated),stroke = FALSE,fillOpacity = 1.5)%>% addLegend(position = "bottomright",pal = pal,values = CountryVaccinations$people_vaccinated, title = "AMOUNT OF PEOPLE VACCINATED" , opacity = 1)
     })
    
  output$barplot <- renderPlotly({
    ggp <- ggplot(covid_report[c(1:input$obs),]) +
      geom_bar(aes(x=Country_name, y=GDP_per_capita_income_in_US_dollars),stat="identity", fill="cyan",colour="#006000")+
      geom_point(aes(x=Country_name, y=((Total_deaths/Total_number_of_cases)*1000000)),stat="identity",color="red",size=2)+
      labs(
        title = "Plot of Gdp vs Total Deaths Country wise
    <span style='color:#008080;'>  GDP  </span>  ~ 
    <span style='color:red;'>Death Rates</span>
    </span>",
        x="country",y="gdp"  ) +  theme_minimal() +
      theme(
        plot.title = element_markdown(lineheight = 1.1))+      theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks = element_blank())
      scale_y_continuous(sec.axis=sec_axis(~.*1000,name="death rates"))
    ggplotly(ggp)  
  })
  
  
  #death rate vs life expectancy
  output$model1plot<-renderPlot({
    ggplot(covid_report, aes(x = Life_expectancy, y = death_rate)) + geom_point() +
      geom_vline(xintercept = mean(covid_report$Life_expectancy)) +geom_smooth(method = "lm" , se = FALSE)
    })

  #following graph represents that as death rate increases gdp decreases implies
  #country with better gdp has better medical facilites
  output$model2plot<-renderPlot({
  ggplot(covid_report, aes(x = eco_vec, y = death_rate)) + geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  })

  
  
  
  output$populationplot <- renderPlot({
    #popultion dependent code
    Q = quantile(covid_report$Population_of_the_country)
    Q1= Q[2]
    Q2 = Q[4]
    Q3 = Q[5]
    
    pop_category<-numeric(105)
    for(i in 1:105)
    {
      if(covid_report$Population_of_the_country [i]<= Q1)
      {
        pop_category[i] = 1
      }else if(covid_report$Population_of_the_country[i]>Q1&  covid_report$Population_of_the_country[i]<=Q2)
      {
        pop_category[i] = 2
      } else if(covid_report$Population_of_the_country [i]>= Q1)
      {
        pop_category[i] = 3
      }
    }
    covid_report = cbind(covid_report,pop_category)
    
    
    if(input$population == "Less Population")
     {
      i=1
      foo <- subset(covid_report, covid_report$pop_category == i)
      plot(foo$Total_number_of_cases,foo$Total_deaths, pch = 16, xlim = c(0,5000000),ylim = c(0,70000),col = i, xlab = "Total No. of Cases", ylab = "Total No. of Deaths" )
      abline(lm(foo$Total_deaths ~ foo$Total_number_of_cases), col = i,lwd =3)
    }
    else if(input$population == "Medium Population")
    {
      i=2
      foo <- subset(covid_report, covid_report$pop_category == i)
      plot(foo$Total_number_of_cases,foo$Total_deaths, pch = 16, xlim = c(0,5000000),ylim = c(0,70000),col = i, xlab = "Total No. of Cases", ylab = "Total No. of Deaths" )
      abline(lm(foo$Total_deaths ~ foo$Total_number_of_cases), col = i,lwd = 3)
    }
    else if(input$population == "High Population")
    {
      i=3
      foo <- subset(covid_report, covid_report$pop_category == i)
      plot(foo$Total_number_of_cases,foo$Total_deaths, pch = 16, xlim = c(0,5000000),ylim = c(0,70000),col = i, xlab = "Total No. of Cases", ylab = "Total No. of Deaths" )
      abline(lm(foo$Total_deaths ~ foo$Total_number_of_cases), col = i,lwd =3)
    }
    else if(input$population == "All of them")
    {
      plot(covid_report$Total_number_of_cases,covid_report$Total_deaths, pch = 16, xlim = c(0,5000000),ylim = c(0,70000),
           col = unique(pop_category),
           xlab = "Total No. of Cases", ylab = "Total No. of Deaths" )
      
      for(i in unique(pop_category)) #1:black #2:pink #3:green
   
         {
         foo <- subset(covid_report, covid_report$pop_category == i)
         abline(lm(foo$Total_deaths ~ foo$Total_number_of_cases), col = i,lwd =3)
    
      }
      legend("topright", col = unique(covid_report$pop_category), 
             legend = c("High","Medium","Low"),
             title = "Populations" ,cex=1,pch =16,pt.cex = 1.4)
      
    }
  })
  
  output$pie <- renderPlot({
  # age
  age65 <- 6938871
  age18 <- 3511354
  age0 <- 97330
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
    geom_col(color = "black") + geom_text(aes(label = no_of_people),
                                          position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y")
  
  })
  
  output$bubble<- renderPlot({
    ggplot(covid_report,aes(x=GDP_per_capita_income_in_US_dollars,y=(Number_of_tests/Population_of_the_country) ,size=(Population_of_the_country/1e7)))+geom_point(show.legend = TRUE, alpha = 0.7,colour = "orange")+ylim(0,12)+scale_x_log10()+scale_size(range=c(0.1,20),name = "POPULATION") +labs(y = "No. of tests per population" , x = "GDP per capita income in US dollars" , title = "No. of tests per population vs GDP of the country with size of the bubble depicting the population of the country")
  }) 
  #plot starts
  
  output$covidtable <- DT::renderDataTable({covid_report})
 
}

# Run the application 
shinyApp(ui = ui, server = server)


