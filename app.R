#libraries used in the app.
library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
library(dygraphs)
library(scales)
library(shinydashboard)
library(shinythemes)
library(zoo)
library(lubridate)
library(MASS)
library(tseries)
library(forecast)
library(xts)
library(dplyr)
library(tidyr)
library(gapminder)
library(gganimate)
library(leaflet)
library(reshape2)
theme_set(theme_bw())

#Reading the data into shiny App.
  data<-fread("final_data.csv",stringsAsFactors = T)

#changing the Date format to correct format.
  data$Date=as.yearmon(data$Date)
  data$year<-year(data$Date)
#Adding the year and month column to the data.
  data1 <- data
  data1$month<-as.factor(month.abb[lubridate::month(data1$Date)])
  data1$year<-year(data1$Date)

#creating data for comparing over rainfall and waterlevel
  aa=data1[,list(avg_rain=mean(rain_level),avg_water_level=mean(water_level)),by=c("Date")]
  a=ts(aa[,c("avg_rain","avg_water_level")],frequency = 12,start = c(2004, 1))

#creating the data for comparasion of average rain fall and average water level.

  Poondi <-ts( data[Name_of_Reservoir=="POONDI",list(average_rain_fall_poondi=mean(rain_level)),by=c("Date")]$average_rain_fall_poondi,frequency = 12,start = c(2004,1))

  CHOLAVARAM <-ts(data[Name_of_Reservoir=="CHOLAVARAM",list(average_rain_fall_poondi=mean(rain_level)),by=c("Date")]$average_rain_fall_poondi,frequency = 12,start=c(2004,1))

  REDHILLS<- ts(data[Name_of_Reservoir=="REDHILLS",list(average_rain_fall_poondi=mean(rain_level)),by=c("Date")]$average_rain_fall_poondi,frequency = 12,start = c(2004,1))

  CHEMBARAMBAKKAM<-ts(data[Name_of_Reservoir=="CHEMBARAMBAKKAM",list(average_rain_fall_poondi=mean(rain_level)),by=c("Date")]$average_rain_fall_poondi,frequency = 12,start = c(2004,1))

  comp<-cbind(Poondi,CHOLAVARAM,REDHILLS,CHEMBARAMBAKKAM)
  
  
  #data for graph
  ma=data1[,list(avg_water_level=mean(water_level),avg_rain_fall=mean(rain_level)),by=c("Date","longitude","latitudes","Name_of_Reservoir","year","month")]
  
  
  

# Define UI for application.
ui <- dashboardPage(skin = "blue",
        dashboardHeader(title = "Water Resources",disable = FALSE),
            dashboardSidebar(width = 180,collapsed = F,
                sidebarMenu(
                    #Home icone 
                    menuItem("Home", tabName = "Intro", icon = icon("home")),
                    #Inference icone
                    menuItem("Inferences", icon = icon("chart-line"), tabName = "inference"),
                    #Forecasting icone
                    menuItem("RainFall Forecasting", icon = icon("umbrella"), tabName = "rain_forecast"),
                    #Map icone
                    menuItem("Map", icon = icon("globe"), tabName = "map"),
                    #Conclusion icone.
                    menuItem("conclusion", icon = icon("angellist"), tabName = "conclusion")
                )
                
            ),
        dashboardBody(
            tabItems(
                #Home Page
                tabItem(tabName = "Intro",
                        tags$b(h1("Chennai Water Resource")),
                        tags$b(h3("About the city")),
                        tags$div(
                            tags$p("Chennai is the capital of the Indian state of Tamil Nadu. 
                            Located on the Coromandel Coast off the Bay of Bengal, it is the biggest cultural, economic and educational centre of south India. 
                            According to the 2011 Indian census, it is the sixth-most populous city and fourth-most populous urban agglomeration in India.
                            - Wikipedia")
                            
                        ),
                        tags$b(h3("The Problem")),
                        tags$div(
                            tags$p("This city is currently experiencing its worst water crisis.The city draws most of its water from four freshwater lakes (Poondi, Sholavaram, Puzhal and Chembarambakkam) that are usually filled by the annual monsoon. The monsoon brought less than half the usual volume of rain in 2018 and, as the city continued to draw water from the lakes at the usual rate, they are at less than one per cent of their normal capacity.-futuredirections.org.The capacitys of the Reservoirs shown below")
                        ),
                        #Graph1
                        plotlyOutput('maxCap',height='250px',width='100%'),
                        tags$b(h3("Data used in Analysis")),
                        tags$div(tags$p("The source of the data is given below.")),
                        tags$a(href="https://www.kaggle.com/sudalairajkumar/chennai-water-management", "Click here!"),
                        tags$div(tags$p("This dataset has details about the water level(mcft) and rainfall(mm) in the four main reservoirs over the last 15 years.Poondi ,Cholavaram ,Redhills,Chembarambakkam.The data is available on a daily basis and the unit is million cubic feet.")),
                        tags$b(h3("Solution")),
                        tags$div(tags$p("we  infered intresting sights from the data and also forecasted the average monthly rain fall(mm) by which concerned authority can plan the useage of resource from these four reservoir."))
                ),
                #Inferences Page   
                tabItem(tabName = "inference",
                        tags$b(h3("Total Resource analysis by all the four reservoir")),
                        #Graph1
                        dygraphOutput('rain_water_level',height='300px',width='100%'),
                        tags$div(
                          tags$p("By the above graph, these are the following inferences :- "),
                          tags$p("By adding up all the average water availability from four reservoirs, we can see that the average water levels reached almost zero three times (2004, 2017 and 2019) in this particular time period (2014-2019)."),
                          tags$p("Except in 2004, the average water levels in the time period (2004-2012) were mostly above 2000mcft. But after 2012 the average water levels have decreased drastically, except Only during the (in)famous Chennai floods of 2015."),
                          tags$p("2017 is similar to 2019 in terms of depletion of water availability but the levels reached close to 0 during the end of august unlike now where the levels reached at the beginning of June itself."),
                          tags$p("During the initial years, rain from north-east monsoon is much higher than south-west monsoon. But seems like last few years, they both are similar (reduction in rains from north-east monsoon)."),
                          tags$p("Except in 2004, the average rainfall in the time period (2004-2010) was mostly above 12mm. But after 2010 the average Rainfall has decreased, except Only during the (in)famous Chennai floods of 2015.")
                          ),
                        #Graph2
                        plotlyOutput("heatmapwater"),
                        tags$div(
                          tags$p("In the period 2004 to 2012, the initial one and a half years(2004-2005(Oct)) the water level in these reservoirs is below 1000 mcft. After the initial period(2004-2005(Oct))  the water level was well maintained (above 1000mcft), there were some seasonal fluctuations but the water level was maintained on a good amount till 2012. But from 2013 till 2015 the water level in these reservoirs went below 1000mcft except 2015 when the famous food took place. The water levels were above 2000mcft at time period(2015 Dec - 2016 Feb) of the flood. But after that period the water level went on depreciated and went under 1000mcft. Especially in April,May, and June of 2019 the water level went down below 150 mcft.")
                        ),
                        #Graph3
                        plotlyOutput("heatmaprain"),
                        tags$div(
                          tags$p("We infer that the city gets some rains in the month of June, July, August, and September due to the southwest monsoon."),
                          tags$p("Major rainfall happens during October and November of every year which is due to North-east monsoon."),
                          tags$p("During the initial years, rain from north-east monsoon is much higher than south-west monsoon. But seems like last few years, they both are similar (reduction in rains from north-east monsoon)."),
                          tags$p("So by the above two heat maps, we observe the hydrological cycle. Generally, the water in the reservoir keeps on deprecating from June till September/October,It cloudburst from September/October till November, so due to cloudburst at previous months, the water level in these reservoir keeps climbing up till December, the water level remains similar till June.")
                        ),
                        tags$b(h3("Reservoir comparasion")),
                        fluidRow(
                          box(title = "Select start Year",status = "primary", solidHeader = FALSE,
                              collapsible = TRUE,selectInput("min",
                                                             label = "year",
                                                             choices = unique(data$year),
                                                             selected = 2004)
                          ),
                          box(title = "Select end Year",status = "primary", solidHeader = FALSE,
                              collapsible = TRUE,selectInput("max",
                                                             label = "year",
                                                             choices = unique(data$year),
                                                             selected = 2019) 
                        )),
                        plotlyOutput('comp1'),
                        tags$div(
                          tags$p("       "),
                          tags$p("Note :- select the range of year till which you want to see the average water level by year in these reservoir")
                        ),
                        tags$div(
                          tags$p("On an average from 2004 - 2019, 4140 mcft of water is contributed by these four reservoirs to Chennai."),
                          tags$p("Note :- All the below inference is explained on bases of  average water level from 2004 to 2019 of june.")
                        ),
                        fluidRow(
                          box(title = "Redhill",status = "warning", solidHeader = FALSE,
                              collapsible = TRUE,
                              tags$div(
                                tags$p("Puzhal aeri, or Puzhal lake, also known as the Red Hills Lake, is located in Red Hills, Chennai, India. It lies in Thiruvallur district of Tamil Nadu state. It is one of the two rain-fed reservoirs from where water is drawn for supply to Chennai City."),
                                tags$p("The Redhill reservoir has the highest average water level from 2004 to 2019 as compared to the other three reservoirs, where the reservoir total capacity is 3300mcft in which on an average only 46%(1528mcft) of its total capacity has got filled. In the total contribution of these four reservoirs (4140mcft), on average, this reservoir contributes 36%(1528mcft) of water to Chennai.")
                              )
                              
                          ),
                          box(title = "Chembarambakkam",status = "warning", solidHeader = FALSE,
                              collapsible = TRUE,
                              tags$div(
                                tags$p("Chembarambakkam lake is a lake located in Chennai, Tamil Nadu, India, about 25 km from Chennai. It is one of the two rain-fed reservoirs from where water is drawn for supply to Chennai City"),
                                tags$p("The Chembarambakkam reservoir has the second-highest average water level from 2004 to 2019 as compared to the other reservoirs, where the reservoir total capacity is 3645mcft in which on an average only 35%(1277mcft) of its total capacity has got filled. In the total contribution of these four reservoirs (4140 Mcft), on average, this reservoir contributes 30%(1277mcft) of water to Chennai.")
                              )
                              
                          )
                      ),
                      fluidRow(
                        box(title = "Poondi",status = "warning", solidHeader = FALSE,
                            collapsible = TRUE,
                            tags$div(
                              tags$p("Poondi Lake or Sathyamoorthy reservoir is the reservoir across Kotralai River in Tiruvallur district of Tamil Nadu State. It acts as the important water source for Chennai city which is 60 km away."),
                              tags$p("The Poondi reservoir has the Third-highest average water level from 2004 to 2019 as compared to the other reservoirs, where the reservoir total capacity is 3231mcft in which on an average only 34%(1103mcft) of its total capacity has got filled. In the total contribution of these four reservoirs (4140mcft), on average, this reservoir contributes 26%(1103mcft) of water to Chennai.")
                            )
                            
                        ),
                        box(title = "Cholavarm",status = "warning", solidHeader = FALSE,
                            collapsible = TRUE,
                            tags$div(
                              tags$p("Sholavaram aeri, or Sholavaram lake, is located in Ponneri taluk of Thiruvallur district, Tamil Nadu, India. It is one of the rain-fed reservoirs from where water is drawn for supply Chennai city from this lake to Puzhal lake through canals."),
                              tags$p("The Cholavarm reservoir has the least average water level from 2004 to 2019 as compared to the other four reservoirs, where the reservoir total capacity is 1081mcft in which on an average only 21%(232mcft) of its total capacity has got filled. In the total contribution of these four reservoirs (4140mcft), on average, this reservoir contributes 5%(232mcft) of water to Chennai.")
                            )
                            
                        )
                      )
                ),
                #Forecasting
                tabItem(tabName = "rain_forecast",
                        #Text
                        tags$div(
                          tags$b("Note we have data of rainfall(mm) from 2014 Jan till 2019 June,so number of month you choose below will starts from July 2019")
                        ),
                        #Input
                        fluidRow(
                          box(title = "Select",status = "primary", solidHeader = FALSE,
                              collapsible = TRUE,
                              selectInput(
                                "S3",
                                label = "Number months to forecast",
                                choices = seq(1,36) ,selected = 6)),
                          #Input
                          box(title = "Inferences",status = "warning", solidHeader = FALSE,
                              collapsible = TRUE,
                              tags$div(
                                tags$p("By the above forecasting values, we infer that in 2019  Chennai will experience  rainfall in duration (July 2019 -  December 2019) in these regions were average monthly rainfall will reach up to 9.65mm on November.")
                              )
                              
                          )
                        ),
                        #over all forecasting
                        dygraphOutput("overForecast",height = 250),
                        tags$div(
                          tags$p("        "),
                          tags$p("        "),
                          tags$p("        ")
                        ),
                        tags$div(
                          tags$p("        "),
                          tags$p("        "),
                          tags$p("        ")
                        ),
                        #Inputs
                        fluidRow(
                          box(title = "Select", status = "primary", solidHeader = FALSE,
                              collapsible = TRUE,
                              selectInput("S1",
                                          label = "Select the Reservoir.",
                                          choices = unique(data$Name_of_Reservoir),
                                          selected = "CHEMBARAMBAKKAM")
                          ),
                          #Inputs
                          box(title = "Select",status = "primary", solidHeader = FALSE,
                              collapsible = TRUE,
                              selectInput(
                                "S2",
                                label = "Number months to forecast",
                                choices = seq(1,36) ,selected = 6)
                          )
                        ),
                        #Graph for forecasting by resvoir.
                        dygraphOutput("forecast",height = 250),
                        tags$div(
                          tags$p("      "),
                          tags$p("      "),
                          tags$p("      "),
                          tags$p("      "),
                          tags$p("      ")
                          
                        )
                ),
                #Map
                tabItem(tabName = "map",
                        fluidRow(
                          box(title = "Select", status = "primary", solidHeader = FALSE,
                              collapsible = TRUE,
                              selectInput("yy",
                                          label = "Year",
                                          choices = unique(ma$year),
                                          selected = 2004)
                          ),
                          #Inputs
                          box(title = "Select",status = "primary", solidHeader = FALSE,
                              collapsible = TRUE,
                              selectInput(
                                "mm",
                                label = "Months",
                                choices = unique(ma$month) ,selected = "Jan")
                        )),
                        leafletOutput("mymap",width="100%",height="350"),
                        tags$div(
                          tags$p("Note :- Larger the circle greater the level of water and denser the color greater the amount of rainfall")
                        )
                ),
                #conclusion page
                tabItem(tabName = "conclusion",
                        tags$div(
                          tags$p("Chennai is familiar with water shortages and is in a constant state of water deficit, mainly because of poorly maintained or non-existent water infrastructure. There is a daily water demand of 1,300 million liters, but Metrowater, the local water supplier, can only provide up to 830 million liters. That shortfall is only expected to grow, as water demand is expected to increase year by year.-futuredirections.org"),
                          tags$p("The city has experienced severe water shortages before. In 2003-04, the four reservoirs that supply the city went dry. Groundwater reserves also declined as thirsty citizens drew water from the aquifers below the city.-futuredirections.org"),
                          tags$p("As Tamil Nadu has experienced more volatile climatic conditions, with floods in 2015 and drought in 2016-17, the province has turned to alternative water sources. Since 2009, three desalination plants have been built in the province.-futuredirections.org"),
                          tags$p("Desalination is not a risk-free solution, however, and there are concerns about the operation of some of the plants that supply water to Chennai. There are allegations that the pipes that carry the effluent from the Nemmeli plant to the ocean were either built incorrectly or have broken, leading to the discharge of hypersaline water on a nearby beach.
                          Given the state of the city’s water infrastructure (estimates suggest that 30-40 per cent of the water supply is lost to leaky pipes), building more desalination plants is not necessarily the best way to improve Chennai’s water security. Building rainwater harvesting infrastructure that stores the monsoon rains for use during the year and repairing leaky pipes could be two better options.-futuredirections.org"),
                          tags$p("As of 2019 forecasting, there will be a good amount of rainfall in the month of October and November, so by this information, the authorities can plan well in advance, how to efficiently preserve this monsoons waters for the entire year.")
                          
                        )
                        
                        
                    
                )
      )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Plotting the max capacity
    output$maxCap <- renderPlotly(
        ggplotly(ggplot(data_frame(Reservoir=c("POONDI","CHOLAVARAM","REDHILLS","CHEMBARAMBAKKAM"),Capacity=c(3231,1081,3300,3645)), aes(Reservoir,Capacity))+ geom_bar(stat="identity", width = 0.3, fill="tomato2")+ scale_y_continuous(labels = comma) +
                     labs(title="The Full Storage capacity of the Reservoirs", 
                          subtitle="Unit of measure is :- McFt", 
                          caption="Source of information: The Hindu")+xlab("Names of Reservoirs")+ylab("Water Storage Capacity(McFt)")))
    #Inferences
    #plotting the comparation of water level and rainfall
    output$rain_water_level<-renderDygraph(
      
      dygraph(a,main = "RainFall and Water level") %>%
        dySeries("avg_rain", axis = 'y2') %>% dyHighlight(highlightCircleSize = 5, 
                                                          highlightSeriesBackgroundAlpha = 0.2,
                                                          hideOnMouseOut = FALSE) %>%  dyRangeSelector()
    )
    
    #Plotting the Heatmap for water level and rain fall.
    output$heatmapwater<-renderPlotly(
      ggplotly (ggplot(data=data1[,list(avg_water_level=mean(water_level)),by=c("month","year")]) +
                  geom_tile(aes(x =month, y = year,fill = avg_water_level))+ scale_x_discrete(limits = month.abb)+scale_y_continuous(breaks =c(2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+labs(title="Combined Avg water level(McFt) at four reservoirs",
                                                                                                                                                                                                                                      x ="Months", y = "Years",fill="Average water level"))
    )
    
    #plotting the Heatmap for Rain fall.
    output$heatmaprain<-renderPlotly(
      ggplotly(ggplot(data=data1[,list(avg_Rain_fall=mean(rain_level)),by=c("month","year")]) +
                 geom_tile(aes(x =month, y = year, fill = avg_Rain_fall)) + scale_x_discrete(limits = month.abb)+scale_y_continuous(breaks =c(2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+labs(title="Combined Avg monthly rainFall(mm) at four reservoirs",x ="Months", y = "Years",fill="Average water level"))
    )
    
    #plotting the comparasion graph for all the four.
    output$comp1<-renderPlotly(
      ggplotly(ggplot(melt(data[(year>=input$min & year<=input$max),list(average_water_level=mean(water_level)),by=c("year","Name_of_Reservoir","max_limit")][,list(avg_water_level=mean(average_water_level)),by=c("Name_of_Reservoir","max_limit")],id.vars = 1),aes(x = Name_of_Reservoir,y = value)) + 
                 geom_bar(aes(fill = variable),stat = "identity",position = "dodge")+labs(title = "capacity vs average water level",x="Name of the  reservoir",y="water level(mcft)")+theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "black")))
    )
    
    #Forecasting part.
    
    #extracting the forecast values
    get_values<-reactive({
      
      #extracting the data from data Frame.
      data_time_series<-xts(x = data[Name_of_Reservoir == input$S1,list(avg_rain_fall=mean(rain_level)),by=c("Date")]$avg_rain_fall ,order.by = data[Name_of_Reservoir == input$S1,list(avg_rain_fall=mean(rain_level)),by=c("Date")]$Date)
      
      #converting the data into time series format.
      data_time_series<-ts(data_time_series, frequency = 12, start = c(2004,1))
      names(data_time_series)[1] <- "Monthly Avg Rain Fall"
      
      #Building the model.
      modelHolt <- HoltWinters(data_time_series)
      modelHolt
      
      #Forecasting the monthly average values of rainfall.
      forecast_obj <- forecast(modelHolt, h=input$S2)
      
      #return the data of correct format
      
      actuals <- forecast_obj$x
      x <- ifelse(as.numeric(forecast_obj$lower[,2])<0, 0, as.numeric(forecast_obj$lower[,2]))
      lower <- ts(x,start=c(2019,7), frequency=12)
      #lower <- ts(forecast_obj$lower[,2],start=c(2019,7), frequency=12)
      upper <- ts(forecast_obj$upper[,2],start=c(2019,7), frequency=12)
      point_forecast <-ifelse(as.numeric(forecast_obj$mean)<0, 0, as.numeric(forecast_obj$mean))
      point_forecast<-ts(point_forecast,start = c(2019,7),frequency = 12)
      val=cbind(actuals, lower, upper, point_forecast)
      
    })
    #extracting the trend graph.
    get_data<-reactive({
      aa=data[Name_of_Reservoir==input$S1,list(avg_rain=mean(rain_level)),by=c("Date")]
      ts(aa[,c("avg_rain")],frequency = 12,start = c(2004, 1))
      })
    #Plotting the graph for forecasting
    output$forecast<-renderDygraph(
      dygraph(get_values(), main = paste("Forecasting of RainFall(mm) for",input$S1)) %>% 
        dyRangeSelector() %>% 
        dyRangeSelector(height = 40,
                        dateWindow = c("2004-01-01", "2021-4-01")) %>%
        dySeries(name = "actuals", label = "actual") %>%
        dySeries(c("lower","point_forecast","upper"), label = "Predicted") %>%
        dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
        dyHighlight(highlightCircleSize = 5,
                    highlightSeriesOpts = list(strokeWidth = 2)) %>%
        dyOptions(axisLineColor = "navy", gridLineColor = "grey")
    )
    
    #Over all forecasting
    
    getValues_overall<-reactive({
      
      #extracting the data from data Frame.
      data_time_series<-xts(x = data[,list(avg_rain_fall=mean(rain_level)),by=c("Date")]$avg_rain_fall ,order.by = data[Name_of_Reservoir == input$S1,list(avg_rain_fall=mean(rain_level)),by=c("Date")]$Date)
      
      #converting the data into time series format.
      data_time_series<-ts(data_time_series, frequency = 12, start = c(2004,1))
      names(data_time_series)[1] <- "Monthly Avg Rain Fall"
      
      #Building the model.
      modelHolt <- HoltWinters(data_time_series)
      modelHolt
      
      #Forecasting the monthly average values of rainfall.
      forecast_obj <- forecast(modelHolt, h=input$S3)
      
      #return the data of correct format
      
      actuals <- forecast_obj$x
      x <- ifelse(as.numeric(forecast_obj$lower[,2])<0, 0, as.numeric(forecast_obj$lower[,2]))
      lower <- ts(x,start=c(2019,7), frequency=12)
      #lower <- ts(forecast_obj$lower[,2],start=c(2019,7), frequency=12)
      upper <- ts(forecast_obj$upper[,2],start=c(2019,7), frequency=12)
      point_forecast <-ifelse(as.numeric(forecast_obj$mean)<0, 0, as.numeric(forecast_obj$mean))
      point_forecast<-ts(point_forecast,start = c(2019,7),frequency = 12)
      cbind(actuals, lower, upper, point_forecast)
    })
    #Forecasting overall rainfall in all.
    output$overForecast<-renderDygraph(
      dygraph(getValues_overall(), main = "combined rainfall Forecasting of RainFall(mm)") %>% 
        dyRangeSelector() %>% 
        dyRangeSelector(height = 40,
                        dateWindow = c("2004-01-01", "2021-4-01")) %>%
        dySeries(name = "actuals", label = "actual") %>%
        dySeries(c("lower","point_forecast","upper"), label = "Predicted") %>%
        dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
        dyHighlight(highlightCircleSize = 5,
                    highlightSeriesOpts = list(strokeWidth = 2)) %>%
        dyOptions(axisLineColor = "navy", gridLineColor = "grey")
    )
    
    
    #Map
    
    output$mymap<-renderLeaflet(
      leaflet(ma[year==input$yy & month==input$mm]) %>% addTiles()%>%setView(lng =80.2707 , lat = 13.0827,zoom = 10)%>%
        addCircles(lng = ~longitude, lat = ~latitudes,weight = 3,radius = ~avg_water_level,color = blues9,fill = ~avg_rain_fall)%>% addMarkers(lng = ~longitude, lat = ~latitudes,popup = paste("Name :-", ma[year==input$yy & month==input$mm]$Name_of_Reservoir, "<br>",
                                                                                                                                                                                          "Avg_water_level :- ",ma[year==input$yy & month==input$mm]$avg_water_level,"<br>" ,
                                                                                                                                                                                                "Avg_rain_fall :- ",ma[year==input$yy & month==input$mm]$avg_rain_fall)) 
      %>%addProviderTiles("Esri.WorldImagery", group="background 1") %>%
        addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
        addLayersControl(baseGroups = c("background 1","background 2"), options = layersControlOptions(collapsed = FALSE))
        )
}
  

# Run the application 
shinyApp(ui = ui, server = server)
