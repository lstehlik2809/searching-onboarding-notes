# uploading libraries
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinycssloaders)
library(rclipboard)
library(openai)
library(DT)

# OpenAI API key
readRenviron("./.Renviron")
openaiKey <- Sys.getenv("OPENAI_API_KEY")


# function for getting text embeddings
get_embedding <- function(text){
  embedding <- openai::create_embedding(
    model = "text-embedding-ada-002", 
    input = text, 
    openai_api_key=openaiKey
  )
  
  embedding_extracted <- embedding$data$embedding
  
  return(embedding_extracted)
}


# function for computing cosine similarity
cosine_similarity <- function(vec1, vec2) {
  return(sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2))))
}


# function for getting onboarding materials and computing their respective embeddings
get_materials_embeddings <- function(path){
  files <- list.files(path)
  
  materials_embeddings_df <- data.frame() 
  
  for(f in files){
    lines <- readLines(paste0(path, "/", f), warn = FALSE)
    content <- paste(lines, collapse = "\n")
    
    emb <- get_embedding(content)
    
    df_supp <- data.frame(
      file = f,
      text = content,
      embedding = paste(unlist(emb), collapse = " ")
    )
    
    materials_embeddings_df <- dplyr::bind_rows(materials_embeddings_df, df_supp)
  }
  
  return(materials_embeddings_df)
}


# function for finding the most relevant documents using cosine similarity and answering the question
get_relevant_materials_answer <- function(df, question, top_n=3){
  
  cosine_similarity_vector <- c()
  
  for(i in 1:nrow(df)){
    document_embedding <- as.numeric(unlist(strsplit(df$embedding[i], " ")))
    question_embedding <- as.numeric(unlist(get_embedding(question)))
    cs <- cosine_similarity(document_embedding, question_embedding)
    cosine_similarity_vector <- c(cosine_similarity_vector, cs)
  }
  
  document_question_sim <- df %>%
    dplyr::mutate(cos_sim = cosine_similarity_vector) %>%
    dplyr::select(file, text, cos_sim) %>%
    dplyr::mutate(cos_sim = round(cos_sim,3)) %>%
    dplyr::arrange(desc(cos_sim)) %>%
    dplyr::top_n(n=top_n, wt=cos_sim)
  
  concatenated_string <- paste(document_question_sim["text"], collapse="\n")
  
  chat <- create_chat_completion(
    model = "gpt-4", # gpt-3.5-turbo-16k, gpt-4
    messages = list( 
      list( "role" = "system", "content" = stringr::str_glue("Answer the question based only on the provided information from onboarding notes. Here are the relevant onboarding notes: {concatenated_string} And here is the question you should answer based on the information provided: {question} If you won't be able to answer the question based on information provided, then write that you are not able to answer the question given the information you were provided with."))
    ), 
    temperature = 0, 
    top_p = 1, 
    n = 1, 
    stream = FALSE, 
    stop = NULL, 
    max_tokens = NULL, 
    presence_penalty = 0, 
    frequency_penalty = 0, 
    logit_bias = NULL, 
    user = NULL, 
    openai_api_key = openaiKey, 
    openai_organization = NULL
  )
  
  answer <- chat$choices$message.content
  
  return(list(document_question_sim = document_question_sim, answer = answer))
  
}


# ui part
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "Searching Onboarding Notes",
    titleWidth = 300,
    tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/ludekstehlik/", icon("linkedin"), "Profile", target="_blank")),
    tags$li(class="dropdown",tags$a(href="https://blog-about-people-analytics.netlify.app/", icon("rss"), "Blog", target="_blank"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("SEARCH", tabName = "search", icon = icon("search")),
      menuItem("README", tabName = "readme", icon = icon("eye"))
    )
  ),
  dashboardBody(
    rclipboardSetup(),
    tabItems(
      tabItem(tabName = "search",
              fluidRow(
                column(width = 6,
                       HTML("<font size='4'><br><strong>1. Select a folder with your notes (in .txt format)</strong></font>"),
                       HTML("<br>"),
                       shinyFiles::shinyDirButton(id = 'folder', label = "Select a folder", title = "Select a folder", style="background-color: #fff; border-color: #d2d6de"),
                       htmlOutput("selected_path"),
                       HTML("<br>"),
                       HTML("<font size='4'><br><strong>2. Compute the embeddings for your notes</strong></font>"),
                       HTML("<br>"),
                       actionButton("emb", label = "Compute embeddings", width = '75%', style="background-color: #fff; border-color: #d2d6de"),
                       uiOutput("embedding_process") %>% withSpinner(color="#605ca8", size = 0.4, proxy.height = "20px"),
                       HTML("<br>"),
                       HTML("<font size='4'><br><strong>3. Set the maximum number of best-fit documents to be used</strong></font>"),
                       HTML("<br>"),
                       numericInput(inputId = "num_docs", label = "Set a number between 1-6", value =  3, step = 1, width = '75%'),
                       HTML("<font size='4'><br><strong>4. Write your question</strong></font>"),
                       textAreaInput("question", label = "It can be a keyword(s) or whole sentence(s)", "", width = "75%", resize = "vertical"),
                       HTML("<font size='4'><br><strong>5. Get an answer to your question</strong></font>"),
                       HTML("<br>"),
                       actionButton("sim", label = "Send your question", width = "75%", style="background-color: #fff; border-color: #d2d6de")
                ),
                column(width = 6,
                       uiOutput("answer_to_question")  %>% withSpinner(color="#605ca8", size = 1),
                       HTML("<br>"),
                       uiOutput("answer_to_question_copy"),
                       HTML("<br>"),
                       HTML("<br>"),
                       dataTableOutput('table') %>% withSpinner(color="#605ca8", size = 1)
                )
              )
      ),
      tabItem(tabName = "readme",
              HTML("<font size='4'><strong>Overview</strong><br>This R Shiny application aids in searching and retrieving relevant information from onboarding notes. It computes embeddings for the onboarding notes, which can then be used to match and answer user's questions based on the provided information in the notes.</br></br>
              <strong>How to use:</strong>
              <ul>
              <li>Navigate to the SEARCH tab.</li>
              <li>Click on the 'Select a folder' button to choose a directory containing your onboarding notes in .txt format.</li>
              <li>Compute embeddings for your notes by clicking the 'Compute embeddings' button.</li>
              <li>Set the maximum number of best matching documents you want the app to consider for answering.</li>
              <li>Type your question into the provided text area.</li>
              <li>Click on 'Send your question' to get an answer based on the onboarding notes.</li>
              <li>View the answer in the right-hand panel. You can also copy the answer to your clipboard.</li>
              <li>Below the answer, a table displays the relevant documents that matched your question along with their relevance scores.</li>
              </ul>
              <strong>Features:</strong>
              <ul>
              <li>Select Onboarding Notes Folder: Users can select a folder containing onboarding notes in .txt format.</li>
              <li>Compute Embeddings: Once the folder is selected, embeddings for the notes can be computed.</li>
              <li>Set Number of Documents: Users can set the maximum number of best matching documents to be considered for answering the question.</li>
              <li>Ask a Question: Users can input their question, which can be a keyword or a complete sentence.</li>
              <li>Get Answer: Based on the embeddings of the notes and the question, the application will fetch and display the most relevant answer.</li>
              <li>Copy Answer: Users have an option to copy the answer to their clipboard.</li>
              <li>Relevance Score Table: Displays a table of relevant documents matched for the question along with their relevance scores.</li>
              </ul>
              <strong>Dependencies:</strong>
              <ul>
              <li>Libraries: tidyverse, shiny, shinydashboard, shinyFiles, shinycssloaders, rclipboard, openai, DT</li>
              <li>API: OpenAI ('text-embedding-ada-002' model used to get text embeddings; 'gpt-4' or 'gpt-3.5-turbo-16k' models used to answer questions based on information provided in the most relevant documents; requires API key)</li>
              </ul>
              </font>"),
      )
    )
  )
)


# server part
server <- function(input, output, session) {
  
  # selecting the folder with notes in .txt format
  volumes <- c(shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(
    input, "folder", 
    roots = volumes, 
    session = session, 
    restrictions = system.file(package = "base"), 
    allowDirCreate = FALSE
    )
  
  output$selected_path <- renderUI({
    dirPath <- shinyFiles::parseDirPath(volumes, input$folder)
    HTML(dirPath)
  })
  
  # getting onboarding materials and computing their respective embeddings
  materials_embeddings_df <- eventReactive(input$emb, {
    dirPath <- shinyFiles::parseDirPath(volumes, input$folder)
    get_materials_embeddings(path=dirPath)
  })

  output$embedding_process <- renderPrint({
    if(nrow(materials_embeddings_df()) > 0) {
      cat("Embeddings are prepared.")
    }
  })
  
  
  # finding the most relevant documents using cosine similarity and answering the question
  output_df <- eventReactive(input$sim, {
    get_relevant_materials_answer(df=materials_embeddings_df(), question=input$question, top_n=input$num_docs)
  })
  
  # answering the question
  output$answer_to_question <- renderUI({
    HTML(paste0("<br>", stringr::str_replace_all(output_df()$answer, "\n", "<br>")))
  })
  
  output$answer_to_question_copy <- renderUI({
    rclipButton(
      "copy_text", 
      label= "Copy text", 
      clipText = output_df()$answer, 
      icon = icon("clipboard"),
      style="color: #fff; background-color: #605ca8; border-color: #605ca8"
    )
  })
  
  # listing the most relevant notes
  output$table <- DT::renderDataTable(server=FALSE,{
    
    output_df()$document_question_sim %>%
      dplyr::select(file, cos_sim) %>%
      dplyr::rename('relevance score'=cos_sim) %>%
      datatable(
        escape = FALSE,
        selection = 'multiple', # single, multiple
        class = 'cell-border stripe', 
        extensions = 'Buttons',
        rownames= FALSE,
        filter = 'top',
        options = list(
          pageLength = 10, 
          autoWidth = FALSE,
          dom = 'Blfrtip',
          buttons = c('copy'), 
          scrollX = FALSE,
          scrollY = '425px'
        )
      ) %>%
      formatStyle(
        'relevance score',
        background = styleColorBar(output_df()$document_question_sim %>% dplyr::select(file, cos_sim) %>% dplyr::rename('relevance score'=cos_sim) %>% dplyr::select('relevance score'), '#605ca8'),
        backgroundSize = '98% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
}

shinyApp(ui, server)
