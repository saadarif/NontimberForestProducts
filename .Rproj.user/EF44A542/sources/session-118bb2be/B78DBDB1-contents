library(shiny)
library(shinyFeedback)
library(readxl)
library(openxlsx)

#Input for 
#1) Banner template file, 1 per component
#2) User's marksheet, could have multiple columns but requires
#student ID and at least one column of scores under the same header as the course component as listed under Banner
#3) Course component id as it appears on banner

ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  titlePanel("Generate Autofilled Banner Import Templates for Mark Upload"),
  strong("1. Upload the template file for the course component as exported from Banner. This file should be in the .xlsx format."),
  fileInput(width="500px", "bannerTemplate", NULL, buttonLabel = "Banner Template Sheet...", accept = c(".xlsx", ".xls")),
  strong("2. Upload your marksheet. Download this sheet from Moodle, make sure to save as .xlsx file or use that as a template to enter your marks manually"),
  p("Importantly, your marksheet should have the following attributes:"),
  p("a. The headers should be in the first row and values should be in the preceeding rows, i.e. a PIP style marksheet won't work!"),
  p("b. Student IDs should be under a columns called 'Username', this is the moodle default"),
  p("c. Any columns with component scores should be either numeric or formulas or percentage format (e.g. '75 %') only. Any formulas should evaluate to a number."),
  p("d. A 0 in your marks column is interpreted as an attempt where the student got 0 marks. For Not Attemped, leave the cell blank or with  '-', the moodle default for no submission"),
  p("e. I assume the marks are out of 100 for each component, otherwise use Percentage only when downoloading from Moodle. This is what Banner expects."),
  p("f. Make sure you don't save it in the same folder as the empty template otherwise your computer will rename it!"),
  br(),
  fileInput(width="500px", "scoreSheet", NULL, buttonLabel = "Your Marksheet...", accept = c(".xlsx", ".xls")),
  br(),
  uiOutput("u_selector"),
  br(),
  strong("4. Download the file using the button below. This file should be able to be imported into Banner as is!!!"),
  br(),
  downloadButton("download1", label = "4. Download your .xlxs file")
)


server <- function(input, output, session) {
  
  #function to check whether input column is real/numeric or % e.g. "75 %"
get_col_info <- function(col) {
    #check if the column is numeric and use this an expression
    if (is.numeric(col)) { 
      return(col)
    }
    else if (is.character(col)) {  #percentage values
      return(as.numeric(sub("%", "", col)))
    }#switch based on results
    else {
      shinyFeedback::feedbackWarning("user_selected", FALSE, "Make sure the scores column in your Marksheet is numeric or formulas or percentage only!", color="red")
    }
    
  }
  
  #read in marksheet
  marksheet <- reactive({
    req(input$scoreSheet)
    ss <- read_excel(input$scoreSheet$datapath, 1, na=c("-",""))
    #check if Username column exists, this should be the name of the column containing student IDS
    id_col= "Username" %in% colnames(ss)
    shinyFeedback::feedbackWarning("scoreSheet", !id_col, "Make sure your marksheet has a column called 'Username' containing student IDS!")
    req(id_col)
    ss
  })
  
  #selecting score columns from Marksheet 
  output$u_selector <- renderUI({
    ms_local <- req(marksheet())
    selectInput("user_selected","3. Select the column (from your marksheet) containing scores for this component:",
                choices=names(ms_local),
                selected = names(ms_local)[[1]])
  })
  
  marks <- reactive({
    CWComp = req(input$user_selected)
    #Check if CWComp is read correctly as numeric
    #if (is.numeric(marksheet()[[CWComp]]))
    #chek if the score column is numeric or percentage only
    score = get_col_info(marksheet()[[CWComp]])
    print(score)
    req(score)
    CWComp
  })
  
  #read in and update banner template
  dataset <- reactive({
    mark_col=req(marks())
    req(input$bannerTemplate)
    bt <-read_excel(input$bannerTemplate$datapath, 1 )
    for(id in marksheet()$Username) {
      if(!(is.na(marksheet()[[mark_col]][marksheet()["Username"] == id]))) {
          #is mark column numeric?
          if (is.numeric(marksheet()[[mark_col]])){
             bt$Score[bt["Student ID"] == id] = round(marksheet()[[mark_col]][marksheet()["Username"] == id],digits=1)
          }
          #if it is a percentage
          else if (is.character(marksheet()[[mark_col]])) {
             bt$Score[bt["Student ID"] == id] = round(as.numeric(sub("%", "", marksheet()[[mark_col]][marksheet()["Username"] == id])),digits=1)
          }
      }
      else{
      bt$Score[bt["Student ID"] == id] = 0
      bt$Comment[bt["Student ID"] == id] = "Not Attempted"
      }
    }
    bt$`Grade Change Reason` = "OE"
    bt
  })
  
  #Download file for upload to banner
  output$download1 <- downloadHandler(
    filename = function() {
      paste0(basename(input$bannerTemplate$name))
    },
    content = function(file) {
      write.xlsx(dataset(), file)
    })

}
shinyApp(ui, server)