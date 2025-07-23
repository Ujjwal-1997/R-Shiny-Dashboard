library(shiny)
library(tidyverse)
library(plotly)
library(shinyalert)
library(shinyBS)

# Importing and preprocessing data
gdp_df <- read.csv('GDPPerCapita.csv', check.names = FALSE)
hdi_df <- read.csv('HDI.csv', check.names = FALSE)

gdp_df <- gdp_df %>% pivot_longer(names_to = 'year', values_to = 'gdp', cols = 3:10)
hdi_df <- hdi_df %>% pivot_longer(names_to = 'year', values_to = 'hdi', cols = 2:9)

final_df <- gdp_df %>% inner_join(hdi_df)

final_df$Country <- as.factor(final_df$Country)

final_reg_df <- final_df %>% filter(Country == 'Arab States' |
                                    Country == 'East Asia and the Pacific' |
                                    Country == 'Europe and Central Asia' |
                                    Country == 'Latin America and the Caribbean' |
                                    Country == 'South Asia' |
                                    Country == 'Sub-Saharan Africa' |
                                    Country == 'World')
final_reg_df$Country <- factor(final_reg_df$Country)
final_df <- final_df %>% filter(Country != 'Arab States' &
                                Country != 'East Asia and the Pacific' &
                                Country != 'Europe and Central Asia' &
                                Country != 'Latin America and the Caribbean' &
                                Country != 'South Asia' &
                                Country != 'Sub-Saharan Africa' &
                                Country != 'World')
final_df$Country <- factor(final_df$Country)
# Messages/texts
wlcm <- "<b>You can use this app to track the progress of countries/regions in terms of their Human Development Index
and GDP Per Capita and any relationship between these two."
opt1_help <- "Select any one of these options to view trends by geographic regions or countries."
str1 = "References"
str2 = "1. UNDP Human Development Reports"
str3 = "https://hdr.undp.org/data-center/documentation-and-downloads"
str4 = "2. The World Bank"
str5 = "https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.PCAP.CD&country="
str6 = "3. Plotly - R Figure Reference: layout"
str7 = "https://plotly.com/r/reference/layout/#layout-margin"
str8 = "4. Stackoverflow - dynamically adjust height and/or width of shiny-plotly output based on window size"
str9 = "https://stackoverflow.com/questions/44324783/dynamically-adjust-height-and-or-width-of-shiny-plotly-output-based-on-window-si/77238246#77238246"
str10 = "5. Stackoverflow - R Shiny - Display static text outside sidebar panel"
str11 = "https://stackoverflow.com/questions/52544228/r-shiny-display-static-text-outside-sidebar-panel"

sidebarPanel2 <- function (..., out1 = NULL, out2 = NULL, out3 = NULL, out4 = NULL, out5 = NULL, out6 = NULL, width = 4) 
{
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      out1, out2, out3, out4, out5, out6
  )
}

# Define UI for application that draws a histogram
ui <- fixedPage(

    titlePanel("Trends of Human Development Index and GDP Per Capita (2015-2022)"),
    h4("Following chart also depicts any relationship between HDI and GDP Per Capita for a country"),

    # Sidebar with a dropdown/text input for regions/countries 
    sidebarPanel2(
        radioButtons("opt1",
                        "View stats by:",
                        choices = c("Region", "Country"),
                        inline = TRUE),
        
        bsTooltip(id = "opt1",
                  title = opt1_help,
                  placement = "left",
                  trigger = "hover"),
        
        conditionalPanel(
          condition = "input.opt1 == 'Region'",
          selectizeInput("selectregion",
                         label = "Select Regions (maximum 5):",
                         choices = c(levels(final_reg_df$Country)),
                         multiple = TRUE,
                         selected = c("Arab States", "World", "South Asia", "Europe and Central Asia", "East Asia and the Pacific"),
                         options = list(maxItems = 5)
                         )
        ),
        
        conditionalPanel(
          condition = "input.opt1 == 'Country'",
          selectizeInput("selectcountry",
                         label = "Select countries (maximum 5):",
                         choices = c(levels(final_df$Country)),
                         multiple = TRUE,
                         selected = c("China", "Australia", "United States", "Brazil", "United Kingdom"),
                         options = list(maxItems = 5)
          )
        ),
        
        out1 = h4(str1),
        out2 = p(a(str2,href=str3)),
        out3 = p(a(str4,href=str5)),
        out4 = p(a(str6,href=str7)),
        out5 = p(a(str8,href=str9)),
        out6 = p(a(str10,href=str11))
    ),

    # Show a plot of the generated distribution
    mainPanel(
        plotlyOutput("outPlot1", width = 'auto', height = '45vh'),
        plotlyOutput("outPlot2", width = 'auto', height = '45vh')
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    shinyalert("Welcome", 
               wlcm,
               html = TRUE)
  
    reg_filt_df <- reactive({
      reg_filt_df <- final_reg_df %>% filter(Country %in% input$selectregion)
      reg_filt_df$Country <- droplevels(reg_filt_df$Country)
      reg_filt_df$Country <- factor(reg_filt_df$Country, levels = input$selectregion)
      reg_filt_df
    })
    
    con_filt_df <- reactive({
      con_filt_df <- final_df %>% filter(Country %in% input$selectcountry)
      con_filt_df$Country <- droplevels(con_filt_df$Country)
      con_filt_df$Country <- factor(con_filt_df$Country, levels = input$selectcountry)
      con_filt_df
    })

    output$outPlot1 <- renderPlotly({
        if(input$opt1 == 'Region'){
          plot_ly(data = final_reg_df, name ="") %>%
            add_lines(inherit = FALSE,
                      data = reg_filt_df(),
                      x = ~year, 
                      y = ~hdi, 
                      color = ~Country,
                      hovertemplate = paste("<b>Region:", reg_filt_df()$Country, "<br>",
                                   "<b>HDI:", reg_filt_df()$hdi)) %>%
            layout( title = "Human Development Index by years for countries/regions",
                    yaxis = list(zeroline = FALSE, title = "Human Development Index", range = c(0.3,1)),
                    xaxis = list(zeroline = FALSE, title = ""),
                    margin=list(pad=8)) %>%
            config(displayModeBar = F)
        }
      else{
        plot_ly(data = final_df, name ="") %>%
          add_lines(inherit = FALSE,
                    data = con_filt_df(),
                    x = ~year, 
                    y = ~hdi, 
                    color = ~Country,
                    hovertemplate = paste("<b>Country:", con_filt_df()$Country, "<br>",
                                 "<b>HDI:", con_filt_df()$hdi)) %>%
          layout( title = "Human Development Index by years for countries/regions",
                  yaxis = list(zeroline = FALSE, title = "Human Development Index", range = c(0.3,1)),
                  xaxis = list(zeroline = FALSE, title = ""),
                  margin=list(pad=8)) %>%
          config(displayModeBar = F)
      }
    })
    output$outPlot2 <- renderPlotly({
      if(input$opt1 == 'Region'){
        plot_ly(data = final_reg_df, name ="") %>%
          add_lines(inherit = FALSE,
                    data = reg_filt_df(),
                    x = ~year, 
                    y = ~gdp, 
                    color = ~Country,
                    hovertemplate = paste("<b>Region:", reg_filt_df()$Country, "<br>",
                                 "<b>GDP:", reg_filt_df()$gdp)) %>%
          layout( title = "GDP Per Capita by years for countries/regions",
                  yaxis = list(zeroline = FALSE, title = "GDP Per Capita (in USD)"),
                  xaxis = list(zeroline = FALSE, title = ""),
                  margin=list(pad=8)) %>%
          config(displayModeBar = F)
      }
      else{
        plot_ly(data = final_df, name ="") %>%
          add_lines(inherit = FALSE,
                    data = con_filt_df(),
                    x = ~year, 
                    y = ~gdp, 
                    color = ~Country,
                    hovertemplate = paste("<b>Country:", con_filt_df()$Country, "<br>",
                                 "<b>GDP:", con_filt_df()$gdp)) %>%
          layout( title = "GDP Per Capita by years for countries/regions",
                  yaxis = list(zeroline = FALSE, title = "GDP Per Capita (in USD)"),
                  xaxis = list(zeroline = FALSE, title = ""),
                  margin=list(pad=8)) %>%
          config(displayModeBar = F)
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
