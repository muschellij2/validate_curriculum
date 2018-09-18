#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(transcriptr)
library(dplyr)

scm = read.csv("scm.csv", header = TRUE, as.is = TRUE)
scm = scm %>% 
    filter(!course_number %in% "")
scm$year = scm$term = NULL
scm$program = "SCM: Biostatistics"

phd = read.csv("phd.csv", header = TRUE, as.is = TRUE)
phd = phd %>% 
    filter(!course_number %in% "")
phd$year = phd$term = NULL
phd$program = "PHD: Biostatistics"

curr = rbind(scm, phd)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Parse Transcript"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("pdf", "Choose Transcript File(s)",
                      accept = c("application/pdf",
                                 ".pdf"),
                      multiple = TRUE
            ),
            checkboxInput("grade", "Include Grades?", 
                          value = TRUE),
            downloadButton("downloadData", "Download")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(
                type = "tabs",
                tabPanel("Transcript", dataTableOutput("tab")),
                tabPanel("Report", dataTableOutput("report"))
            )            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    get_df = function() {
        req(input$pdf)
        
        # when reading pdf, may error
        tryCatch(
            {
                print(input$pdf)
                # print(dput(input$pdf))
                paths = input$pdf$datapath
                df <- lapply(
                    paths, 
                    read_transcript,
                    type = "jhu")
                df = mapply(function(x, name) {
                    x$file = name
                    id = attributes(x)$student_info["student_id"]
                    x$student_id = id
                    x
                }, df, input$pdf$name, SIMPLIFY = FALSE)
                
                df = do.call(rbind, df)
                if (!input$grade) {
                    df$grade = NULL
                }
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        return(df)  
    }
    output$tab <- renderDataTable({
        get_df()
    }, options = list(pageLength = 10))
    
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            "output.csv"
        },
        content = function(file) {
            write.csv(get_df(), file, row.names = FALSE)
        }
    )   
    
    output$report <- renderDataTable({
        df = get_df()
        uprog = unique(df$program)
        xcurr = curr %>% 
            filter(program %in% uprog)
        if (nrow(xcurr) == 0) {
            return(NULL)
        }
        df = full_join(xcurr, df, 
                       by = c("course_number", "program"))
        df = df %>% 
            rename(course_title = course_title.x)
        
        not_req = is.na(df$status)
        df$status[not_req] = "elective"
        no_title = is.na(df$course_title)
        df$course_title[no_title] = df$course_title.y[no_title]
        df$public_health = grepl("PH", df$course_number)
        df$taken = !is.na(df$year)
        grades = df$grade
        df = df %>% 
            group_by(student_id) %>% 
            select(course_number, course_title, status,
                   program, student_id, 
                   taken, year, public_health)
        df$grade = grades
        df
    }, options = list(pageLength = 10))
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
