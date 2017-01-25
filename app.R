require(shiny)
require(shinysky)
require(shinythemes)
require(tidyverse)
require(shinyBS)
require(RISmed)

# Find the next ID of a new record
# (in mysql, this could be done by an incremental index)
GetNextId <- function(i) {
    if (exists("articles") && nrow(articles) > 0) {
        a <- gsub("([A-Z ]{7,25}:)", "$\\1", articles$abstract[i], perl = TRUE)
        a <- unlist(strsplit(a, "\\$"))
        a <- a[a != ""]
        return(a)
    } else {
        return ("No more!")
    }
}

get.pubmed.data = function(search.elements){
    res = EUtilsSummary(search.elements)
    res_records = EUtilsGet(res)
    res = data.frame(PMID(res_records),
                     YearPubmed(res_records),          
                     Title(res_records),
                     ArticleTitle(res_records),
                     AbstractText(res_records),
                     Affiliation(res_records), 
                     stringsAsFactors = FALSE)
    
    colnames(res) = c("PMID",
                      "Year",
                      "Title",
                      "ArticleTitle",
                      "AbstractText",
                      "Affiliation")
    return(res)
}

# IterateArticles <- function() {
#     iterator <- iterator + 1
#     return(iterator)
# }

# CSS to use in the app
appCSS <-
    ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
.select2-container {
    width: 100% !important;
}
#submit_msg { margin-left: 15px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
"

shinyApp(
    ui = fluidPage(
        theme = shinytheme("simplex"),
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        title = "Subset Clinical Trials",
        div(id = "header",
            h1("Subset Clinical Trials"),
            strong( 
                span("Source"),
                a("on GitHub", href = "https://github.com/tempuslabs/compbio/tree/martin/patient_pubmed_search"))
        ),
        
        fluidRow(
            column(3,
                   div(
                       id = "form",
                       textInput(inputId = "disease", 
                                 label = "Disease Search Terms:", 
                                 placeholder = "e.g. breast stage III",
                                 width = 300),
                       textInput(inputId = "gene", 
                                 label = "Gene:",
                                 placeholder = "e.g. BRAF:",
                                 width = 300),
                       textInput(inputId = "variant", 
                                 label = "Gene Variant:",
                                 placeholder = "e.g. V600E, amplification",
                                 width = 300),
                       actionButton("submit", "Search", class = "btn-primary"),
                       br(),br(),
                       actionButton("next_art", "Next Article", class = "btn-primary"),
                       actionButton("prev_art", "Previous Article", class = "btn-primary")
                   )
                   ),
                   
            column(9,
                   uiOutput("results")
            )
        )
    ),
    
    
    server = function(input, output, session) {
        values <- reactiveValues(i = 1)
        # Update the PUBMED search on submit
        updateTable <- eventReactive(input$submit, {
            patient.data <- c(input$disease, 
                              input$gene,
                              input$variant)
            results <- get.pubmed.data(paste(patient.data, collapse = " "))
            arrange(results, desc(Year))
        })
        
        # Iterate articles if necessary
        # IterateArticles <- reactive({
        #     if(input$next_art){
        #         iterator <- iterator + 1
        #     }else if (input$prev_art){
        #         print("YES")
        #         iterator <<- iterator - 1
        #     } 
        # })
        observe({
            input$next_art
            isolate({
                values$i <- values$i + 1
            })
        })
        
        observe({
            input$prev_art
            isolate(values$i <- values$i - 1)
        })
        # render the clinical trials panel
        output$results <- renderUI({
            res <- updateTable()
            div(
                id = "resultsPanel",
                h2(renderText(paste(nrow(updateTable()), 
                                    " Search Results (", 
                                    values$i, 
                                    "of ",
                                    nrow(updateTable()),
                                    ")"))),
                h3("Journal (Year)"),
                renderText(paste(res$Title[values$i], "(", res$Year[values$i], ")")),
                h3("Article Title"),
                renderText(res$ArticleTitle[values$i]),
                h3("Article Abstract"),
                renderText(res$Abstract[values$i]),
                h3("Author Affiliations"),
                renderText(res$Affiliation[values$i])
                # DT::renderDataTable(updateTable(), options = list(
                #     autoWidth = TRUE,
                #     columnDefs = list(list(width = '200px', targets = "_all"))
                )
        })
    }
)
