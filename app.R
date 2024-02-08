#Shiny interface for Timber forest products

library(dplyr, warn.conflicts = F)#data manipulation and selection
library(magrittr)
library(ggplot2)
library(vroom) #data input
library(shiny)


#Data tidying
timberData <- vroom("timberForestProducts.csv")  # read in the data
#tidy names 
#timberData <- timberData %>% select_all(~gsub("\\s+|\\.", "_", .)) 
profits <- timberData %>% select(ends_with("USD")) #select profit data
richness <- timberData %>% select(ends_with(c("richness", "abundance", "cover"))) # select richness, abundance and cover data


# Define UI for application that chooses which variables to plot
ui <- fluidPage(

    # Application title
    titlePanel("Timber Forest Products and Profits"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(6,
          strong("1. Which product profits you would like to plot (horizontal axes)?"),
          selectInput(inputId = "profits", label= "", choices = c("",names(profits)))),
        column(6,
          strong("2. Which species richness you would like to plot (vertical axes)?"),
          selectInput(inputId = "richness", label= "", choices = c("",names(richness))))
          
        ),
      fluidPage(
      column(2,
             actionButton("plotBtn", "3. Run Analysis"))  
      ),
        # Show a plot of the generated distribution
      fluidRow(
      column(12, plotOutput("profit_richness"))
      )
    #textOutput(outputId = "outResult")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
   timbdat<- reactive(timberData)
  
  # output$outResult <- renderText({
     # This is how we access input elements
  #  paste(strsplit(input$richness, "_")[[1]][1], strsplit(input$richness, "_")[[1]][2])
  # })
    
    observeEvent(input$plotBtn,{
      output$profit_richness <- renderPlot({
        
        #ylabel = paste(strsplit(input$richness, "_")[[1]][1], strsplit(input$richness, "_")[[1]][2])
        #xlabel = paste(strsplit(input$profits, "_")[[1]][1], strsplit(input$profits, "_")[[1]][2])
        isolate({
          
          req(input$profits, input$richness)
          
          timbdat() %>%
          ggplot(aes(.data[[input$profits]], .data[[input$richness]], col=Habitat)) +
          geom_point(size=2, alpha=0.7)+
          scale_color_manual(values=c("#003f5c", "#ffa600"))+
          scale_y_continuous(limits = c(0, NA)) + 
          #ylab(ylabel) + 
          #xlab(xlabel) +
          geom_smooth(method=lm , se=F) +
          theme_bw()
        }) # end of isolate  
    }, res=120)
    }) #end of button click observeEvent
}

# Run the application 
shinyApp(ui = ui, server = server)
