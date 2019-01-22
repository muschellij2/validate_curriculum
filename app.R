#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(transcriptr)
library(dplyr)
library(tidyr)

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

pass_grades = c("A", "B", "C", "P")

requirements = data_frame(
    inside_department = c(FALSE, FALSE),
    public_health = c(FALSE, TRUE),
    min_credits = c(6, 6),
    is_800 = FALSE
)
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
                tabPanel("Report", dataTableOutput("report")),
                tabPanel("Summary", 
                         dataTableOutput("ph_summary"),
                         dataTableOutput("summary"),
                         dataTableOutput("full_ph_summary")
                )
            ),
            helpText(HTML(
              paste0('The source code of this app is ',
                     '<a href="https://github.com/muschellij2/validate_curriculum">', 
                     'on Github</a>.')
            ))            
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
                
                df = transcriptr::combine_transcript(paths)
                
                if (!is.null(input$pdf$name) & FALSE)  {
                    df = df %>% 
                        rename(datapath = file)
                    dd = input$pdf %>% 
                        select(name, datapath) %>% 
                        rename(file = datapath)
                    df = left_join(df, dd)
                }
                
                if (!input$grade) {
                    df$grade = NULL
                }
                df
                
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        return(df)  
    }
    output$tab <- renderDataTable({
        df = get_df()
        df = df %>% 
            select(-pass_credits, -is_800, -public_health, -school, 
                   -department, -sub_course_number)
        df
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
    
    sum_df = reactive({
        df = get_df()
        
        sdf = df %>% 
            group_by(student_id, program, department, 
                     public_health, is_800) %>% 
            summarize(credits = sum(pass_credits)) %>% 
            ungroup()
        sdf
    })
    output$summary <- renderDataTable({
        df = sum_df()
        df
    })
    
    output$ph_summary <- renderDataTable({
        sdf = sum_df()
        most_dep = sdf %>% 
            group_by(student_id, program) %>% 
            filter(credits == max(credits)) %>% 
            select(department) %>% 
            rename(program_department = department)
        ssdf = left_join(sdf, most_dep)
        # ssdf = ssdf %>% 
        #     mutate(department = as.numeric(department)) %>% 
        #     filter( !(department >= 550 & department <= 559))
        ssdf = ssdf %>% 
            mutate(
                inside_department = department %in% program_department) %>% 
            group_by(student_id, program, 
                     inside_department, public_health, is_800) %>% 
            summarize(credits = sum(credits))
        ssdf = left_join(ssdf, requirements)
        ssdf = ssdf %>% 
            filter(!is_800) 
        ssdf %>% 
            mutate(met_requirement = credits >= min_credits) %>% 
            select(-min_credits)
    
      
    })
    
    
    output$full_ph_summary <- renderDataTable({
        sdf = sum_df()
        d = sdf %>% 
            group_by(student_id, program, public_health, is_800) %>% 
            summarize(credits = sum(credits))
        return(d)
    })    
    
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
