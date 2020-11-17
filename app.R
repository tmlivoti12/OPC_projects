
library(shiny)
library(RSQLite)
library(shinyFiles)
library(shinyalert)
library(shinydashboard)
library(htmlwidgets)
library(kableExtra)
library(htmltools)
library(lubridate)
library(dplyr)
library(Hmisc)
library(knitr)
library(tidyr)
library(readr)
library(RODBC)
library(shinyjs) #look into this to make more functional and user friendly
library(squr) # adds smooth SQL integration, use devtools::install_github("smbache/squr")
library(shinymanager) # install: devtools::install_github("datastorm-open/shinymanager")


# need to be connected to the database
conn <- dbConnect(RSQLite::SQLite(), "./Dev Files/ReportGeneratorAcessDB.db")

security <- read_db_decrypt(conn, name = "ds2", passphrase = "OPC")
my_server <- "192.168.1.11"
my_db <- "reports"

db <- RODBC::odbcDriverConnect(paste0("DRIVER={SQL Server};
                           server=",my_server,";
                           database=",my_db,";
                           uid=",security$user,";
                           pwd=",security$password))


# functions
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling 
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000000000000000000000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

# sets the usernames and passwords for authentication

credentials <- read_db_decrypt(conn, name = "credentials", passphrase = "OPC")

nameCheck <- function(x){
    if (!isValidPractice(x)){
        stop("This is an invalid practice ID. Please enter a valid one.")
    } else {print("Valid Practice ID")}
}

dateCheck <- function(x, d, g){
    if (!is.null(x) & isValidPractice(x)){
        if (!isValidDate(x,d)){
            stop("The selected date occurs after the latest extraction for this practice ID. Please enter a valid one.")
        } else {print("Valid Report Date")}
    }
}
monthlyCheck <- function(g, d, x){
    if (g == 2 & day(d) == 1 & isValidDate(x,d)){
        print(paste0("-- Monthly report will be generated for the month of ", 
                     format(seq(d, length=2, by="-1 months")[2], "%b %Y")))
    } else {print('')}
}


isValidPractice <- function(x){
    if (!is.null(x)){
        pracCheck <- paste0("SELECT id FROM opcdw.dbo.d_practice WHERE id ='", x, "';")
        pracCheck_valid <- sqlQuery(db, pracCheck)
        return(!is.na(pracCheck_valid[1,1]))
    } else(return(FALSE))
}

isValidDate <- function(x,d){
    dateCheck <- paste0("SELECT id FROM opcdw.dbo.d_extraction WHERE practice_id ='", x, "' AND extraction_date >= '", d, "';")
    dateCheck_valid <- sqlQuery(db, dateCheck)
    return(!is.na(dateCheck_valid[1,1]))
}
isImpart <- function(x){
        imp2artCheck_monthly <- paste0("SELECT practice_id FROM reports.dbo.imp2art_monthlyReport_log WHERE practice_id ='", x, "';")
        imp2artCheck_annual <- paste0("SELECT practice_id FROM reports.dbo.imp2art_yearlyReport_log WHERE practice_id ='", x, "';")
        imp2artCheck_monthly_out <- sqlQuery(db, imp2artCheck_monthly)
        imp2artCheck_annual_out <- sqlQuery(db, imp2artCheck_annual)
        return(!is.na(imp2artCheck_monthly_out[1,1]) | !is.na(imp2artCheck_annual_out[1,1]))
}


# sets date to last date of import
extDate <- function(x){
    if (!is.null(x) & isValidPractice(x)){
        dateCheck <- paste0("SELECT MAX(extraction_date) FROM opcdw.dbo.d_extraction WHERE practice_id = '", x, "';")
        dateCheck_lastExtraction <- ifelse(isValidPractice(x), as.character(sqlQuery(db, dateCheck)[1,1]), as.character(Sys.Date()))
        return(as.Date(as.character(dateCheck_lastExtraction), "%Y-%m-%d"))
    } 
}

isLoaded <- function(x, d){
    if (!is.null(x) & isValidPractice(x)){
        loaded <- FALSE
        loadCheck <- paste0("SELECT DISTINCT practice_id FROM reports.dbo.opc_audit_patients;")
        loadCheck_out <- sqlQuery(db, loadCheck)
        if (loadCheck_out[1,1] == x){
            filesRun <- read.csv("./Dev Files/SQLruntime_log.csv", row.names = NULL, stringsAsFactors = F)
            names(filesRun) <- c("file", "id", "time")
            filesRun <- filesRun %>% filter(id == paste0(x, "_", d))
            if (length(unique(filesRun$file)) >= 9){
                loaded <- TRUE
            }
        } 
        return(loaded)
    } 
}

monthlyDateChange <- function(g,d){
    if (!is.null(g)){
        if (g == 2){
            day(d) <- 1
        }
        return(d)
    }
}

# runs SQL scripts - set up loop to run all of them
rodbc <- function(query){
    connection <- RODBC::odbcDriverConnect(paste0("DRIVER={SQL Server};
                              server=",my_server,";
                              database=",my_db,";
                              uid=",security$user,";
                              pwd=",security$password))
    on.exit(RODBC::odbcClose(connection))
    RODBC::sqlQuery(connection, query)
} 
 
sqlFiles <- data.frame(fileLoc = list.files("../SQL Scripts/"), name = '')
sqlFiles <- sqlFiles[grep('SQLServer', sqlFiles$fileLoc),]
sqlFiles$fileLoc <- gsub(".sql", "", sqlFiles$fileLoc, fixed = T)
sqlFiles$name = c('0.1 - Create Base Tables', '0.2 - Diagnosis & Comorbidities', '0.3 - Pharmacotherapy',
                  '0.4 - Spirometry & Clinical Therapy', '0.5 - Risk Assessment', '0.6 - Recommendations',
                  '1.1 - Child Asthma Aggregation', '1.2 - Adult Asthma Aggregation', '1.3 - COPD Aggregation')
 
runtime_log <- read.csv('./Dev Files/SQLruntime_log.csv')
runtime_log <- runtime_log %>% filter(!is.na(file)) %>% group_by(file) %>% summarise(avg_mins = mean(time))
sqlFiles$avg_mins <- as.numeric(runtime_log$avg_mins)
 
progressNum <- c(0.3, 0.1, 0.15, 0.1, 0.09, 0.05, 0.07, 0.07, 0.07)
run_data <- read.csv("./Dev Files/SQLruntime_log.csv", row.names = NULL, stringsAsFactors = F, col.names = c('file', 'id', 'time'))
sql_runFiles <- function(x, y){ # x - practice_id, y - report date
    runDataStore <- data.frame(file = c(), id = c(), time = c())
    run_id <- paste0(x, '_', y)
    update_proxy$completionText <- ''
    for (sql in 1:nrow(sqlFiles)){
        incProgress(progressNum[sql], message = "Executing Script: " , detail = paste0(sqlFiles$name[sql], " (Avg. Run Time ", floor(sqlFiles$avg_mins[sql]),
                                                                                       ' Mins. ', round((sqlFiles$avg_mins[sql] - floor(sqlFiles$avg_mins[sql]))*60),
                                                                                       ' Secs.)'))
        fileLocation <- paste0("../SQL Scripts/", sqlFiles$fileLoc[sql])
        timeStartSQL <- Sys.time()
        sqlResult <- sq_file(fileLocation) %>% 
            sq_set(audit_date = as.Date(y),
                   practice_id = paste0(x)) %>%
            sq_send(.with = rodbc)
        if (length(sqlResult) > 0){
            stop(paste0("Unable to run ", sqlFiles$name[sql], ": ", 
                        gsub('[Microsoft][ODBC SQL Server Driver][SQL Server]', '', sqlResult[1], fixed = T)))
            }
        runDataStore <- rbind(runDataStore, c(sqlFiles$fileLoc[sql], run_id, finish <- difftime(Sys.time(),timeStartSQL, units = 'mins')))
        }
    write.table(runDataStore, file = './Dev Files/SQLruntime_log.csv', sep = ',', append = T, row.names = F, col.names = F)
}

createPracticeDir <- function(x, y){
    dir.create(paste0("../OPC QI Reports/Reports - Ready for Review/", x, "_", y, " - In Progress"), showWarnings = FALSE)
}

today <- Sys.Date()


ui <- secure_app(head_auth = tags$script(inactivity),
    fluidPage(useShinyjs(),useShinyalert(),
              tags$head(tags$style(".shiny-notification{position: fixed;top: 33%;left: 33%;right: 2%;}")),
    #includeCSS("../Asthma-Report-Files/custom.css"),
    titlePanel("Report Generator"),
    sidebarLayout(
        sidebarPanel(
            helpText("Input desired Practice ID. The date of the last data import is automatically generated."),
            textInput("text", h4("Practice ID"),
                      value = "Enter Practice ID"), 
            dateInput("date", h4("Date Input")),
            radioButtons("selectGroup", label = h4("Report Scope"), 
                               choices = c("Annual" = 1, "Monthly (IMP2ART)" = 2),
                               selected = 1),
            conditionalPanel(
                condition = "input.selectGroup == 1",
                checkboxGroupInput("type", h4("Report Type"), 
                             choices = list("Child Asthma" = 1, 
                                              "Adult Asthma" = 2, 
                                              "COPD" = 3),
                               selected = c(1,2,3))),
            conditionalPanel(
               condition = "input.selectGroup == 1",
               checkboxGroupInput("option", h4("Report Options"),
                            choices = list("Patient level" = 4, 
                                           "IMP2ART" = 5), 
                            selected = 4)),
            conditionalPanel(
                condition = "output.loadCheck == 1",
                checkboxInput("reload", "Reload Data?",
                                   value = FALSE)),
            actionButton("report", "Generate report")
        ),
        mainPanel(
           textOutput("nameValid"),
           textOutput("dateValid"),
           textOutput("monthlyDate"),
           htmlOutput("completionText"),
           fluidRow(
               column(6, htmlOutput("updateText_left")),
               column(5, htmlOutput("updateText_right"))
           )
        )
    )
))


server <- function(input, output, session) {
    secure_server(
     check_credentials = check_credentials(credentials))
    
    output$auth_output <- renderPrint({
         reactiveValuesToList(res_auth)
    })
    
     reportDate <- reactive(as.character(input$date))
     output$nameValid <- reactive(nameCheck(toupper(input$text))) 
     output$dateValid <- reactive(dateCheck(toupper(input$text), input$date, input$selectGroup))
     output$monthlyDate <- reactive(monthlyCheck(input$selectGroup, input$date, toupper(input$text)))
     output$loadCheck <- reactive(as.numeric(isLoaded(toupper(input$text), input$date)))
     
     update_proxy <<- reactiveValues(completionText = '', updateText_left = '', updateText_right = '')
     observe(output$completionText <- renderText(HTML(update_proxy$completionText)))
     observe(output$updateText_left <- renderText(HTML(update_proxy$updateText_left)))
     observe(output$updateText_right <- renderText(HTML(update_proxy$updateText_right)))
     
     outputOptions(output, "loadCheck", suspendWhenHidden = FALSE)
     observeEvent(input$report,{
         prac_noquotes <- gsub("'", "", toupper(input$text))
         prac_noquotes <- gsub('"', '', prac_noquotes)
         updateDateInput(session, "text", value = prac_noquotes)
     })
     observe({
         if (isValidPractice(toupper(input$text))){
            date <- extDate(toupper(input$text))
            updateDateInput(session, "date", value = date)
            imp2artUpdate <- isImpart(toupper(input$text))
            if (imp2artUpdate & !(5 %in% input$options)){
                updateCheckboxGroupInput(session, "option", selected = c(4,5))
            } else (updateCheckboxGroupInput(session, "option", selected = c(4)))
         }
     })
     observe({
         if (!is.null(input$selectGroup)){
            if (input$selectGroup == 2){
                date_m <- monthlyDateChange(input$selectGroup, input$date)
                updateDateInput(session, "date", value = date_m)
            }
         }
     })
     observe({
         shinyjs::toggleState("report", condition = (isValidPractice(toupper(input$text)) & isValidDate(toupper(input$text), input$date)))
     })
     observeEvent(input$report, {
         disable(id = 'report')
         disable(id = 'type')
         disable(id = 'option')
         disable(id = 'date')
         disable(id = 'text')
         disable(id = 'selectGroup')

         
         
         genReport <-  
         if(input$selectGroup == 1){
             genDir <- createPracticeDir(toupper(input$text), input$date)
             # if (isImpart(toupper(input$text)) & !(5 %in% input$option)){
             #     shinyalert(title = "IMP<sup2></sup>ART Option Not Selected", 
             #                text = "Reports for this practice have previously been done using the IMP2ART option, but IMP2ART is not currently selected. 
             #                Select <b>Update Option</b> to run the IMP2ART version of the asthma reports, or select <b>Continue</b> to continue with the regular report version",
             #                html = T, confirmButtonText = 'Update Option', cancelButtonText = 'Continue', type = 'error', showCancelButton = T,
             #                callbackR = function(x) {if (x) {print(x)
             #                    updateCheckboxGroupInput(session, "option", selected = 5)}})
             #}
             genPatientFolder <-  dir.create(paste0("../OPC QI Reports/Reports - Ready for Review/", toupper(input$text), "_", input$date, " - In Progress", "/Patient Reports"), showWarnings = FALSE)
             genPatientFolder <-  dir.create(paste0("../OPC QI Reports/Reports - Ready for Review/", toupper(input$text), "_", input$date, " - In Progress", "/Patient Reports/Possible Asthma or COPD"), showWarnings = FALSE)
             if (!isLoaded(toupper(input$text), input$date) | input$reload){
                 withProgress(sql_runFiles(toupper(input$text), reportDate()))
                 practice_log_query <- paste0("SELECT * FROM reports.dbo.opc_audit_log WHERE practice_id = '", input$practice_id, "' AND audit_date = '", input$date, "';")
                 practice_log <- sqlQuery(db, practice_log_query)
                 update_proxy$updateText_left <- paste(paste0('<b>', c('Total Patients: ', 'Excluded Patients: ', 'Clinical Event Count: ',
                                                                       'Therapy Event Count: ', 'Percent of Therapy in SNOMED: '), '</b>',
                                                              format(practice_log[1,5:9], big.mark = ',')), collapse = "<br/>")
                 child_log_query <- paste0("SELECT child_asthma_active FROM reports.dbo.asthma_report_data_child WHERE practice_id = '", 
                                           input$practice_id, "' AND audit_date = '", input$date, "';")
                 adult_log_query <- paste0("SELECT adult_asthma_active FROM reports.dbo.asthma_report_data_adult WHERE practice_id = '", 
                                           input$practice_id, "' AND audit_date = '", input$date, "';")
                 copd_log_query <- paste0("SELECT adult_copd_35plus FROM reports.dbo.copd_report_data WHERE practice_id = '", 
                                           input$practice_id, "' AND audit_date = '", input$date, "';")
                 update_proxy$updateText_right <- paste(paste0('<b>', c('Child Asthma Patients: ', 'Adult Asthma Patients: ', 'COPD Patients: '), '</b>',
                                                              format(practice_log[1,5:9], big.mark = ',')), collapse = "<br/>")
             } else (
                 shinyalert(title = "Data Already loaded", 
                                            text = "Data for this practice is already loaded into the report space, and the data re-load option has not been checked. SQL Files will not be re-ran.",
                                            html = T, confirmButtonText = 'Okay',type = 'info')
             )
             if(length(input$type) < 1){
                 "Pick"
             }
             withProgress(message = "Generating Practice Level Reports:", {
                 print(input$option)
                          if(1 %in% input$type & !(5 %in% input$option)){
                              incProgress(.1, message = 'Generating Child Asthma Report')
                              rmarkdown::render("../Asthma Report Files/ukReport_ChildAsthma_v2.Rmd",
                                   output_file = paste0("../OPC QI Reports/Reports - Ready for Review/", toupper(input$text), "_", input$date, " - In Progress", "/OPC Child Asthma Annual Audit"),
                                   params = list(practice_id = toupper(input$text), audit_date = reportDate()))
             }
                          if(2 %in% input$type & !(5 %in% input$option)){
                              incProgress(.1, message = 'Generating Adult Asthma Report')
                              rmarkdown::render("../Asthma Report Files/ukReport_AdultAsthma_v2.Rmd",
                                       output_file = paste0("../OPC QI Reports/Reports - Ready for Review/", toupper(input$text), "_", input$date, " - In Progress", "/OPC Adult Asthma Annual Audit"),
                                       params = list(practice_id = toupper(input$text), audit_date = reportDate()))
             }
                          if(3 %in% input$type){
                              incProgress(.1, message = 'Generating COPD Report')
                              rmarkdown::render("../COPD Report Files/ukReport_COPD_v2.Rmd", 
                                           output_file = paste0("../OPC QI Reports/Reports - Ready for Review/", toupper(input$text), "_", input$date, " - In Progress", "/OPC COPD Annual Audit") , 
                                           params = list(practice_id = toupper(input$text), audit_date = reportDate()))
             }
                          if((5 %in% input$option) & (1 %in% input$type)){
                              incProgress((1/3), message = 'Generating IMP2ART Child Asthma Report')
                              rmarkdown::render("../IMP2ART Asthma Report Files/ukReport_ChildAsthma_impart_v2.Rmd",
                                   output_file = paste0("../OPC QI Reports/Reports - Ready for Review/", toupper(input$text), "_", input$date, " - In Progress", "/OPC Child IMP2ART Asthma Annual Audit"),
                                   params = list(practice_id = toupper(input$text), audit_date = reportDate()))
             }
                          if((5 %in% input$option) & (2 %in% input$type)){
                              print('It worked')
                              incProgress((1/3), message = 'Generating IMP2ART Adult Asthma Report')
                              rmarkdown::render("../IMP2ART Asthma Report Files/ukReport_AdultAsthma_impart_v2.Rmd",
                                   output_file = paste0("../OPC QI Reports/Reports - Ready for Review/", toupper(input$text), "_", input$date, " - In Progress", "/OPC Adult IMP2ART Asthma Annual Audit"),
                                   params = list(practice_id = toupper(input$text), audit_date = input$date))
             }})
             if(4 %in% input$option){ # pulls patient list
                 asthma_copd <- sqlQuery(db, 'select * from reports.dbo.opc_audit_patients where ((asthma_med_qof = 1 AND asthma_qof = 1 AND asthma_resolved IS NULL) OR (copd_qof = 1 AND copd_resolved IS NULL)) AND hashed_nhs IS NOT NULL;') # pulls patient list
                 possible_asthma_copd <- sqlQuery(db, 'select * from reports.dbo.opc_audit_patients where ((asthma_possible = 1 AND asthma_resolved IS NULL) OR (copd_possible = 1 AND copd_resolved IS NULL)) AND hashed_nhs IS NOT NULL;') # pulls patient list
                 asthma_copd$hashed_nhs <- trimws(asthma_copd$hashed_nhs)
                 possible_asthma_copd$hashed_nhs <- trimws(possible_asthma_copd$hashed_nhs)
                 asthma_copd$hashed_nhs <- gsub('/', '\\', asthma_copd$hashed_nhs)
                 possible_asthma_copd$hashed_nhs <- gsub('/', '\\', possible_asthma_copd$hashed_nhs)
                 withProgress(message = "Generating Asthma/COPD Patient Level Reports:", value = 0, {
                              log_time <- c()
                              for(i in 1:nrow(asthma_copd)){ # generates patient reports diagnosed asthma or copd
                                  time_start <- Sys.time()
                                  # if (!isLoaded(toupper(input$text), input$date)){
                                  #     stop("Underlying data has been overwritten. Data will need to be reloaded to proceeed.")
                                  # }
                                  incProgress(1/nrow(asthma_copd), detail = paste0(round(100*i/nrow(asthma_copd), 1), "% - (Est. ",
                                                                                   ifelse(is.null(log_time), '', round((nrow(asthma_copd)-i)*mean(log_time)/60)), ' Mins. Remaining)'))
                                  print(reportDate())
                                  purrr::walk(asthma_copd[i, 1],rmarkdown::render("../Patient Level Reports/patient_level_v3.Rmd",
                                                                                  params = list(patientList = asthma_copd, audit_date = reportDate(), impart = isImpart(toupper(input$text))),
                                                                                  output_file = paste0("../OPC QI Reports/Reports - Ready for Review/", toupper(input$text),"_",
                                                                                                       reportDate(), " - In Progress", "/Patient Reports/patientReport_OPC-",  asthma_copd[i, 2])))
                                  log_time <- c(log_time, Sys.time()-time_start)

                 }})
                 withProgress(message = "Generating Possible Asthma/COPD Patient Level Reports:", value = 0, {
                              log_time <- c()
                              for(i in 1:length(possible_asthma_copd$patient_id)){ # generates patient reports for possible asthma or possible COPD
                                  time_start <- Sys.time()
                                  # if (!isLoaded(toupper(input$text), input$date)){
                                  #     stop("Underlying data has been overwritten. Data will need to be reloaded to proceeed.")
                                  # }
                                  incProgress(1/nrow(possible_asthma_copd), detail = paste0(round(100*i/nrow(possible_asthma_copd), 1), "% - (Est. ",
                                                                                            ifelse(is.null(log_time), '', round((nrow(possible_asthma_copd)-i)*mean(log_time)/60)), ' Mins. Remaining)'))
                                  purrr::walk(possible_asthma_copd[i, 1],rmarkdown::render("../Patient Level Reports/patient_level_possible_v2.Rmd",
                                                                                           params = list(patientList = possible_asthma_copd, audit_date = reportDate()),
                                                                                           output_file = paste0("../OPC QI Reports/Reports - Ready for Review/", toupper(input$text),"_",
                                                                                                                reportDate(), " - In Progress", "/Patient Reports/Possible Asthma or COPD/patientReport_OPC-",  possible_asthma_copd$hashed_nhs[i])))
                                  log_time <- c(log_time, Sys.time()-time_start)
                 }})
                 file.rename(paste0("../OPC QI Reports/Reports - Ready for Review/", input$text, "_", input$date, " - In Progress"),
                             paste0("../OPC QI Reports/Reports - Ready for Review/", input$text, "_", input$date, ""))
             }
             }else if(input$selectGroup == 2){
                 withProgress(message = "Monthly IMP2ART Report is being generated...", rmarkdown::render("../IMP2ART Asthma Report Files/monthlyReport_v3.Rmd", 
                                   output_file = paste0("../OPC QI Reports/Reports - Ready for Review/", "IMP2ART Monthly Asthma Report - ", toupper(input$text), " - ", 
                                                        format(seq(input$date, length=2, by="-1 months")[2], "%b%Y")),
                                   params = list(practice_id = toupper(input$text), audit_date = reportDate())))
                 shinyalert(title = "IMP2ART Monthly Report Complete", 
                            text = paste0("The monthly report was successfully generated and stored at ", 
                                          "../OPC QI Reports/Reports - Ready for Review/", "<b>IMP2ART Monthly Asthma Report - ", 
                                                 toupper(input$text), " - ", format(seq(input$date, length=2, by="-1 months")[2], "%b%Y"), "</b>"),
                            html = T, confirmButtonText = 'Okay',type = 'info')
             }else{"Pick"}
         enable(id = 'report')
         enable(id = 'type')
         enable(id = 'option')
         enable(id = 'date')
         enable(id = 'text')
         enable(id = 'selectGroup')
     }
)
}

# Run the application 
shinyApp(ui = ui, server = server)


