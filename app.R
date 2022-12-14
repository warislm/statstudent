library(shiny)
library(readxl)
library(DT)

datafinalexam65 <- read_excel("datafinalexam65.xlsx")
View(datafinalexam65)
data <- read_excel("/Users/lookmaizz/Downloads/shinyR/test/datafinalexam65.xlsx")

head(data)
df <- datafinalexam65
# Define UI for application that draws a histogram
ui <- fluidPage(
  h3("ระบบสารสนเทศเพื่อรายงานข้อมูลพฤติกรรมและผลการเรียนรู้ของนักเรียน รายวิชาสถิติ ภาคปลาย ปีการศึกษา 2565"),
  textInput("ID", 
            label="ระบุรหัสประจำตัวนิสิต", value="654xxxxx27"),
  actionButton("submit", "Submit", icon("Submit")),
  DT::DTOutput("Table")
)

table_data <- data.frame(ID = as.numeric(), Name = as.character(), Gender = as.character(), check.names = FALSE)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  tableValues <- reactiveValues(df = data.frame(ID = as.numeric(), Name = as.character(), Gender = as.character(), check.names = FALSE))
  
  observeEvent(input$submit, {
    temp <- tableValues$m
    newRow <- df
    
    if(!is.null(temp)) {
      
      if(isolate(input$ID) < temp[nrow(temp), 1]) {
        temp <- rbind(temp, newRow)
        temp <- dplyr::arrange(temp, ID)
      } 
    } else {
      temp <- rbind(temp, newRow)
    }
    
    tableValues$m <- temp
    
    
  })
  
  
  output$Table <- DT::renderDataTable({
    
    if(!is.null(tableValues$m)){
      
      table_data <- tableValues$m
      

      
    }
    
  })
  
  observe({print(colnames(tableValues$m))})
  observe({print(!is.null(tableValues$m))})    
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)