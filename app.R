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
timberData <- vroom("NonTimberForestProducts.csv")  # read in the data
#tidy names 
#timberData <- timberData %>% select_all(~gsub("\\s+|\\.", "_", .)) %>% %>% rename_all(tolower)
profits <- timberData %>% select(ends_with("USD")) #select profit data
richness <- timberData %>% select(ends_with(c("richness", "abundance", "cover", "Carbon"))) # select richness, abundance and cover data


# Define UI for application that chooses which variables to plot
ui <- fluidPage(
    #change theme?
    theme = bslib::bs_global_theme(bootswatch = "lumen"),
    # Application title
    titlePanel("Relationship between Ecosystem Health and Non-Timber Forest Product Profits"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(6,
          strong("1. Which product profits you would like to plot (horizontal axes)?"),
          selectInput(inputId = "profits", label= "", choices = c("",names(profits)))),
        column(6,
          strong("2. Which indicator of biodiversity would like to plot (vertical axes)?"),
          selectInput(inputId = "richness", label= "", choices = c("",names(richness))))
          
        ),
      fluidPage(
      column(2,
             actionButton("plotBtn", "3. Analyze!"))  
      ),
        # Show a plot of the generated distribution
      fluidRow(
      column(12, plotOutput("profit_richness"))
      ),
     h2("About the data:"),
     p("1. The data were collected from the West Bali National Park in 25x25 m plots by researchers from Oxford Brookes University and Universitas Warmadewa."),
     p("2. or invertebrates we used pitfall and pan traps, for birds we used point sampling."),
     p("3. Abundance indicates the total number of individuals found. Richness indicates the total number of species found.") ,
     p("4. 0s for profits indicate that the crop is present but not productive."),
    
    hr(),
    fluidRow(
    column(12, align="center", print("copyright: M. Campera and S. Arif. Taxon images are from Phylopic.org")))
     
    #textOutput(outputId = "outResult"),
    
)


# Define server logic required to draw a scatterplot
server <- function(input, output) {
  thematic::thematic_shiny()
    
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
          geom_point(size=2, alpha=0.7, na.rm=T)+
          scale_color_manual(values=c("#003f5c", "#ffa600"))+
          scale_y_continuous(limits = c(0, NA)) + 
          #ylab(ylabel) + 
          #xlab(xlabel) +
          geom_smooth(method=lm , se=F, na.rm=T) +
          #theme_bw() + 
          theme(panel.background = element_rect(fill='transparent'),
                #plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
                panel.grid.major = element_blank(), #remove major gridlines
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.background = element_rect(fill='transparent'), #transparent legend bg
                legend.box.background = element_rect(fill='transparent') #transparent legend panel
          )
          
          #plt
          #add image to plot
          plt + inset_element(p = img, left = 0.8, bottom = 0.8, right = 1, 
                          top = 0.95, clip=TRUE, on_top=FALSE) +
          theme(rect  = element_rect(fill="transparent"))
        }) # end of isolate  
    }, res=110)
    }) #end of button click observeEvent
}

# Run the application 
shinyApp(ui = ui, server = server)
