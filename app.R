#Shiny interface for Timber forest products

library(dplyr, warn.conflicts = F)#data manipulation and selection
library(magrittr)
library(ggplot2)
library(vroom) #data input
library(shiny)
library(png)
library(patchwork)


#### Helper functions

pickImage <- function (imageName) {
  #uses the animal input column to pick the appropriate image to display
  img = switch(
    imageName,
    "Spider" = readPNG("./images/spider.png", native = TRUE),
    "Birds" = readPNG("./images/bird.png", native = TRUE),
    "Insect" = readPNG("./images/insect.png", native = TRUE),
    "Ants" = readPNG("./images/ant.png", native = TRUE),
    "Tree" = readPNG("./images/tree.png", native = TRUE)
  )
}


#Data tidying
timberData <- vroom("timberForestProducts.csv")  # read in the data
#tidy names 
#timberData <- timberData %>% select_all(~gsub("\\s+|\\.", "_", .)) 
profits <- timberData %>% select(ends_with("USD")) #select profit data
richness <- timberData %>% select(ends_with(c("richness", "abundance", "cover"))) # select richness, abundance and cover data


# Define UI for application that chooses which variables to plot
ui <- fluidPage(

    # Application title
    titlePanel("Non-Timber Forest Products and Profits"),

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
        isolate({   #produce plot only on click
          
          #find the appropriate image
          img = pickImage(strsplit(input$richness, " ")[[1]][1])
          
          req(input$profits, input$richness)
          
          plt <- timbdat() %>%
          ggplot(aes(.data[[input$profits]], .data[[input$richness]], col=Habitat)) +
 
          geom_point(size=2, alpha=0.7)+
          scale_color_manual(values=c("#003f5c", "#ffa600"))+
          scale_y_continuous(limits = c(0, NA)) + 
          #ylab(ylabel) + 
          #xlab(xlabel) +
          geom_smooth(method=lm , se=F) +
          #theme_bw() + 
          theme(panel.background = element_rect(fill='transparent'),
                #plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
                panel.grid.major = element_blank(), #remove major gridlines
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.background = element_rect(fill='transparent'), #transparent legend bg
                legend.box.background = element_rect(fill='transparent') #transparent legend panel
          )
          
          #add image to plot
          plt + inset_element(p = img, left = 0.8, bottom = 0.8, right = 1, 
                          top = 0.95, clip=TRUE, on_top=FALSE) +
          theme(rect  = element_rect(fill="transparent"))
        }) # end of isolate  
    }, res=120)
    }) #end of button click observeEvent
}

# Run the application 
shinyApp(ui = ui, server = server)
