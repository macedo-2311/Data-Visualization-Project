#install.packages(c("leaflet","billboarder","randgeo", "ggiraph" ,"tidyverse","TTR","pals", "shiny","dplyr", "htmltools", "highcharter", "rgdal", "raster", "tigris", "shinythemes", "raster", "ggpolt2", "gganimate", "transfromr", "sp", "shinyWidgets","ggiraph", "randgeo", "tidyverse" ))
library(leaflet)
library(shiny)
library(htmltools)
library(ggplot2)
library(gganimate)
library(transformr)
library(sp)
library(rgdal)
library(raster)
library(shinythemes)
library(raster)
library(pals)
library(tigris)
library(shinyWidgets)
library(highcharter)
library(dplyr)
library(billboarder)
require(htmltools)
require(html)
require(shiny)
require(leaflet)
require(htmltools)
require(ggplot2)
library(highcharter)
library(billboarder)
library(lubridate)
library(tidyverse)
library(ggiraph)
library(randgeo)



#******************MAPA******************************
# Read Africa Data Set
mapafrica<- readOGR(".", "Africa")
projeto_2015r<-  read.csv("DataSetProject2015.csv", sep = ",", header = TRUE)
projeto_2015<- geo_join(mapafrica, projeto_2015r, "COUNTRY", "Entity", how="left")
projeto_2015$GPD[ which( is.na(projeto_2015$GPD))] = 0


#******************LOLIPOP******************************

lollipop_data<- data_set[- c(1275:1404),c(1,2,3,7,6,10)]
lollipop_data$MalariaDeaths= as.double(as.character(lollipop_data$MalariaDeaths))
lollipop_data$HIVDeaths= as.double(as.character(lollipop_data$HIVDeaths))
lollipop_data$MalariaDeaths= round(lollipop_data$MalariaDeaths,2)
lollipop_data$HIVDeaths= round(lollipop_data$HIVDeaths,2)
lollipop_data=transform(lollipop_data,minimo =pmin(HIVDeaths, MalariaDeaths))
lollipop_data=transform(lollipop_data, maximo= pmax(HIVDeaths, MalariaDeaths))

#******************BARPLOT******************************

data_barplot<-as.data.frame(data_set)
data_barplot=data_barplot[, c(1,3,4,10)]
data_barplot=data_barplot[- c(1275:1404),]


#******************TIME SERIES******************************
timeseries_data=read.csv('DataSetMGD.csv')
timeseries_data$val=round(timeseries_data$val,2)
timeseries_data$year=as.character((timeseries_data$year))
timeseries_data$year=as.Date((timeseries_data$year), "%Y")


###PERSONALSAR######


titulo <- tags$a(href = 'https://www.youtube.com/watch?v=L7m61Em4A5k',
                 'Evolution of diseases in Africa',style = "font-family: 'verdana', cursive;font-weight: 1000; line-height: 1.1;color: #262626;")



css_codes <- tags$style(type = "text/css",".irs-bar {background: #ff9900; border-top: 1px #ff9900 ; border-bottom: 1px #ff9900;}
                           .irs-bar-edge {background: #ff9900; border: 1px #ff9900; width: 20px;}
                        .irs-line {border: 1px #ff9900;}
                        .irs-from, .irs-to, .irs-single {background: #ff9900}
                        .irs-grid-text {color: #ff9900; font-weight: bold;}
                        .label-default {background: #ff9900;}
                        }
                        ")
css_panels <- tags$style(HTML(".tabbable > .nav > li[class=active]    > a {background-color: #ff9900; color:white;}"),
                         HTML(".tabbable > .nav > li[class=desactive]    > a {background-color: #ffa31a ; color:#ffa31a}"))

css_slider_back <- tags$head(tags$style(HTML('
                                             #sidebar {
                                             background-color: #ffebcc;
                                             border: 1px #ffebcc;
                                             }')))

### UI ######

ui <- fluidPage( theme=shinytheme("united"),css_codes, css_panels, css_slider_back, 
                 setBackgroundColor("#ffebcc"),
                 
                 titlePanel(h1(titulo)),
                 tabsetPanel(
                   tabPanel("Home", 
                            sidebarLayout(
                              sidebarPanel(id="sidebar",
                                           h4(div(HTML('<P align="center", style= "position:relative;top5px;color: gray15"><b>Context</b></p>'))),
                                           p(div(HTML("<p align='justify';style='color:gray10; font-size:15px;' > Africa as one of the largest continents worldwide is characterized by the disparity of values on global statistics.</p>"))),
                                           p(div(HTML("<p align='justify';style='color:gray10; font-size:15px;' > By the year 2015 the Gross Domestic Product (GDP) of the African Countries was set on 5,7%, being the lowest in the world.</p>"))),
                                           p(div(HTML("<p align='justify';style='color:gray10; font-size:15px;' > Comparing with other factors, such as health data, a direct relationship is observed Africa has the highest number of preventable diseases such as Malaria and HIV. </p>"))),
                                           p(div(HTML("<p align='justify';style='color:gray10; font-size:15px;' > </p>"))),
                                           br(),
                                           br(),
                                           h5(div(HTML('<P align="left", style= "position:relative;top3px;color: gray15"><b>Presentation @ NOVA IMS</b></p>'))),
                                           p(div(HTML("<p align='justify';style='color:gray10; font-size:15px;' > António Macedo (m20181271) </p>"))),
                                           p(div(HTML("<p align='justify';style='color:gray10; font-size:15px;' > Filipe Lopes (m20180937)</p>"))),
                                           p(div(HTML("<p align='justify';style='color:gray10; font-size:15px;' > Helena Vilela (m20180361)</p>")))
                                           
                              ),
                              mainPanel(leafletOutput("map", height = 500, width = 800)))
                   ),
                   tabPanel('GDP by Country',
                            sidebarPanel(
                              sliderInput("barplot_year",
                                          "Select Year Range", 
                                          min=1990,
                                          max=2015,
                                          value= format(1990,big.mark = " ")),
                              
                              checkboxGroupInput(
                                inputId = "focus",
                                label = "Region",
                                choices = c("Northern Africa" , "Middle Africa", "Western Africa","Southern Africa", "Eastern Africa"),
                                inline = TRUE
                              ),
                              h4(div(HTML('<P align="center", style= "position:relative;top5px;color: gray15"><b>Context</b></p>'))),
                              p(div(HTML("<p align='justify';style='color:gray10; font-size:15px;' > Despite owning the richest natural resources, the African continent continuous to be the poorest. Between 1990 to 2015, Africa was a stage to multiple civil wars, dictators and tyranians governments, climate catastrophes which were among the causes to increase the distance between wealth and poverty.</p>"))),
                              p(div(HTML("<p align='justify';style='color:gray10; font-size:15px;' > As seen on the plot, through the years the major income region is the Southern Africa region, contradicted by the poorest regions being the Northern, Middle and Western Africa.</p>")))
                            ),
                            
                            
                            mainPanel(h4(HTML('<p align = "center"; style="color:coral"><b>Gross Domestic Product (GDP) per capita</b></p>')),
                                      billboarderOutput("barplot", width = "100%", height = "450px"))
                   ),
                   tabPanel("HIV and Malaria",
                            sidebarPanel(
                              
                              sliderInput("lolli_year",
                                          "Select Year Range:", 
                                          min=(1990),
                                          max=(2015), 
                                          value= 1990
                              ),
                              selectInput("lolli_region", 
                                          "Select Region:",
                                          choices = list("Northern Africa" ="Northern Africa",
                                                         "Middle Africa"= "Middle Africa",
                                                         "Western Africa"="Western Africa",
                                                         "Southern Africa"="Southern Africa",
                                                         "Eastern Africa"="Eastern Africa"),
                                          selected = "Western Africa"),
                              h4(div(HTML('<P align="center", style= "position:relative;top5px;color: gray15"><b>Context</b></p>'))),
                              p(div(HTML("<p align='justify';style='color:gray10; font-size:15px;' > HIV and Malaria have been on top of the biggest causes of death in the last 30 years in the African continent.</p>"))),
                              p(div(HTML("<p align='justify';style='color:gray10; font-size:15px;' > Over the period of 1990 to 2015, both HIV and Malaria deaths rose steadily peaking between 2004 and 2006, entering in a decreasing trend until 2015.</p>")))
                              
                              
                            ),
                            mainPanel(h4(HTML('<p align = "center"; style="color:coral"><b>Comparation between HIV and Malaria</b></p>')),
                                      ggiraphOutput("Lolli"))
                   ),
                   tabPanel('Malaria Deaths',
                            sidebarPanel(
                              
                              checkboxGroupInput("timeseries_location", 
                                                 "Select Region:",
                                                 choices = list("African Region" ="African Region",
                                                                "Eastern Mediterranean Region"= "Eastern Mediterranean Region",
                                                                "European Region"="European Region",
                                                                "Region of the Americas"="Region of the Americas",
                                                                "South-East Asia Region"="South-East Asia Region",
                                                                "Western Pacific Region"="Western Pacific Region",
                                                                "WHO region"="WHO region"),
                                                 selected = c("WHO region","European Region", "African Region","Eastern Mediterranean Region", "Eastern Mediterranean Region","Region of the Americas", "South-East Asia Region" ,"Western Pacific Region" )),
                              h4(div(HTML('<P align="center", style= "position:relative;top5px;color: gray15"><b>Context</b></p>'))),
                              p(div(HTML("<p align='justify';style='color:gray10; font-size:15px;' > Deaths by Malaria saw a clear rise-peak-fall trend, increasing from around 670,000 deaths in 1990; peaking at around 930,000 in 2004; and then declining (although at varying rates) to around 620,000 in 2017 (Roser & Ritchie, 2017).</p>"))),
                              p(div(HTML("<p align='justify';style='color:gray10; font-size:15px;' > More than 90% of the estimated 300–500 million malaria cases that occur worldwide every year are in Africa. (WHO, 2014).</p>")))
                              
                              
                            ),
                            
                            
                            mainPanel(h4(HTML('<p align = "center"; style="color:coral"><b>Global Malaria Deaths</b></p>')),
                                      plotOutput("my_MGD"))
                   )
                 )
)






server <- function(input, output) {
  
  #################################################################################################
  ###################################### MAPA #####################################################
  ################################################################################################
  
  output$map <- renderLeaflet({
    
    
    mytext<- paste("<strong>","Country:", "</strong>", projeto_2015$COUNTRY,"<br/>", "<strong>", "GDP per capita: ","</strong>", format(as.numeric(projeto_2015$GPD),nsmall=0, big.mark = "."),"$", "<br/>") %>%
      lapply(htmltools::HTML)
    
    mybins=c(0,500,1000,2000,3000,5000,10000,50000)
    mypalette = colorBin( palette="Oranges", domain=projeto_2015$GPD, na.color="transparent", bins=mybins)
    
    leaflet(projeto_2015) %>%
      setView(lng = 8.032837, 
              lat = 8.997194, 
              zoom = 3.47) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons( 
        fillColor = ~mypalette(GPD),
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", weight=0.3,
        highlightOptions = highlightOptions(color = '#800000', weight=4, bringToFront = TRUE, opacity = 1),
        label = mytext,
        labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>%
      
      addLegend(pal=mypalette,
                values = ~projeto_2015$GDP,
                title = 'GDP per Capita',
                position = 'bottomright',
                labels = c("No value", "1$ - 1000$", "1000$ - 2000$", "2000$ - 3000$", "3000$ - 5000$", "5000$ - 10000$", "10000$ - 20000$"))
  })
  
  #################################################################################################
  ###################################### LOLLIPOP #################################################
  #################################################################################################
  
  observe({
    
    by_duration <- 
      lollipop_data[(lollipop_data$Year==input$lolli_year) & (lollipop_data$Region==input$lolli_region),] %>%
      arrange(maximo) %>%
      mutate(Entity=factor(Entity, levels=Entity))%>%
      na.omit(by_duration$Entity)
    
    
    output$Lolli <-  renderggiraph({
      lolli <- ggplot(by_duration, aes(x = Entity, y = maximo))  +
        geom_segment( aes(x=Entity, xend=Entity, y=minimo, yend=maximo), color="grey", size= 1) +
        geom_point_interactive( aes(x=Entity, y=minimo, tooltip = minimo, color='chocolate1'), size=2.7) +
        geom_point_interactive( aes(x=Entity, y=maximo, tooltip = maximo, color='firebrick2'), size=2.7) +
        scale_x_discrete() +
        scale_color_manual(name = NULL, labels = c("Malaria","HIV"), values = c("chocolate1","firebrick2"))+
        theme_light() + 
        theme(legend.position = c(0.85,0.2),
              plot.title = element_text(hjust = 0.5),
              legend.title = element_text("Legend"),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.background = element_rect(fill = "transparent"),
              legend.box.background = element_rect(fill = "transparent"),
              axis.title.x = element_text(colour = "gray15", face="bold"),
              axis.title.y = element_text(colour = "gray15", face="bold"),
              axis.text.x = element_text(colour="gray15"),
              axis.text.y = element_text(colour="gray15"),
              legend.key = element_rect(fill = "transparent"),
              legend.text = element_text(colour="gray15",face="bold"),
              panel.border = element_blank(),
              axis.line.x = element_line(colour = "gray30", size = 0.6),
              axis.line.y = element_line(colour = "gray30", size= 0.6)) +
        xlab("Countries") +
        ylab("Death Rate per 100 mil persons")+
        coord_flip() 
      girafe(ggobj = lolli)
      
    }) 
  })
  
  #################################################################################################
  ###################################### BAR GRAFIC GPD ############################################
  #################################################################################################
  
  
  observe({
    
    bar_arrange <- data_barplot[(data_barplot$Year==input$barplot_year) ,] %>%
      #ifelse((data_barplot$Region == input$focus), 1,0)%>%
      group_by(Region)%>%
      arrange(desc(GPD)) %>%
      mutate(Entity=factor(Entity, levels=Entity))%>%
      na.omit(by_duration$Entity)
    
    output$barplot <-renderBillboarder({ 
      billboarder() %>%
        bb_barchart(data = bar_arrange, mapping=bbaes(x= Entity, y= GPD, group=Region), rotated = TRUE, color = "#ff9900")%>%
        #bb_y_grid(show = TRUE) %>%
        bb_bar(width= list(ratio= 3))%>%
        bb_y_axis(tick = list(format = suffix("$"), fit= TRUE),
                  label = list(text = "GDP", position = "outer-top")) %>%
        bb_x_axis( tick = list(
          values = c(" ", ""),
          outer = FALSE)) %>%
        bb_color(palette = c("#331400", "#662900", "#993d00", "#cc5200", "#ff751a"))%>%
        bb_legend(show = FALSE)# %>% 
    })
    
    
  })
  #Highlight
  observeEvent(input$focus,
               {
                 billboarderProxy("barplot") %>%
                   bb_proxy_focus(input$focus)
               }, ignoreNULL = FALSE)
  
  
  
  #################################################################################################
  ###################################### TIME SERIES MALARIA ############################################
  #################################################################################################
  
  observe({
    
    by_timeseries <- 
      timeseries_data[(timeseries_data$location==input$timeseries_location) ,]
    
    
    #LINE CHART
    output$my_MGD <- renderPlot({
      ggplot(by_timeseries, aes(x=year, y=val, group=location))+
        geom_line(aes(x=year, y=val, color= location), size=1.3)+
        scale_color_manual(values = c("#331400", "#662900", "#993d00", "#cc5200", "#ff751a", "#ffa366", "#ffffff"))+
        theme_light() + 
        theme(legend.position = "right",
              plot.title = element_text(hjust = 0.5),
              legend.title = element_blank(),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "#ffebcc", color = NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.background = element_rect(fill = "transparent"),
              legend.box.background = element_rect(fill = "transparent"),
              # legend.box.margin = element_rect(fill= "transparent"),
              axis.title.x = element_text(colour = "gray15", face="bold"),
              axis.title.y = element_text(colour = "gray15", face="bold"),
              axis.text.x = element_text(colour="gray15"),
              axis.text.y = element_text(colour="gray15"),
              legend.key = element_rect(fill = "transparent"),
              legend.text = element_text(colour="gray15",face="bold"),
              panel.border = element_blank(),
              axis.line.x = element_line(colour = "gray30", size = 0.6),
              axis.line.y = element_line(colour = "gray30", size= 0.6)) +
        xlab("Year") +
        ylab("Number of Malaria Deaths")
      
      
      
    })
  })
  
  
}
shinyApp(ui, server)





