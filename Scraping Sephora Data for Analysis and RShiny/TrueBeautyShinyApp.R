library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(readxl)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(leaflet)

dir_reviews=paste0(getwd(),"/Data Files/Reviews")
dir_summary=paste0(getwd(),"/Data Files/Summary")
list_reviews<-list.files(dir_reviews)
list_summary<-list.files(dir_summary)

ui=shinyUI(fluidPage(
  
  dashboardPage(skin = "black", title = "TrueBeauty",
                
  dashboardHeader(title = tags$img(src="W.png")),

  
  dashboardSidebar(
    
    sidebarMenu(
      selectInput("Category", "Category:",gsub(pattern = "\\.xlsx$", "", list_summary)),
      
      sidebarMenu(
        selectInput("Skin Type", "Skin Type:", "NA")),
    
    conditionalPanel(
      condition = "input.Category == 'Cleanser'|input.Category=='Eye Treatment'|input.Category=='Face Masks'|input.Category=='Face Treatment'|input.Category=='Moisturiser'",
      numericRangeInput("Price", "Price Range:", value=c(0,100), width = NULL, separator = " to ")
    ),
    
    br(),
    br(),
    mainPanel(
      img(src='TRUE BEAUTY.png'),
    ))
  ),
  
  dashboardBody(
    fluidRow(width = 12, height = NULL,

    tabsetPanel(
      tabPanel("Products",
               
               column(12,box(height =500,width = 800, solidHeader = FALSE, status = "success",
                             DT::dataTableOutput("table1",height = 300))),
               tabsetPanel(
                
               tabPanel("Description",
               column(12,box(height =700,width = 800, solidHeader = FALSE, status = "success",
                             uiOutput("text1")))
               ),
               tabPanel("Reviews",
                        column(12,box(height =400,width = 800, solidHeader = FALSE, status = "success",
                              DT::dataTableOutput("table2",height = 300)))
               ),
               tabPanel("Word Cloud",
                        column(12,box(height =400,width = 800, solidHeader = FALSE, status = "success",
                                      plotOutput("wc")))
               )
               )
               
      ),
      tabPanel("Store Locator",
               leafletOutput("mymap"),
               tabsetPanel(
                 tabPanel(strong("Please click on markers to view Store Address & Name"))
               )
               )
      )
    )
    ))
      
)
)
   

server=shinyServer(function(input, output) {
  
  calcul<-reactive({
    summary<-read_excel(paste0("Data Files/Summary/",input$Category,".xlsx"),sheet = 1)
    summary$Price<-as.integer(summary$Price)
    if(all(is.na(summary$Price))==FALSE){
      w=which(summary$Price>=input$Price[1]&summary$Price<=input$Price[2])
    }else{
      w=c(1:nrow(summary))
    }
    
    d<-data.frame("Product"=summary$`Product Name`[w],
                  "Brand" = summary$`Brand Name`[w],
                  "Price"=summary$Price[w],
                  "Rating"=summary$`True Score`[w])
    order.scores <- order(d$Rating, decreasing = TRUE) 
    
    dat <- d[order.scores,]
    dat
  })
  output$table1 <- DT::renderDataTable({
    dat<-calcul()
    datatable(dat, selection=list(mode="single", target="cell"),rownames = F)
    
  })

  output$mymap <- renderLeaflet({
    stores <- read_excel("Data Files/Sephora Stores.xlsx")
    leaflet() %>% addTiles() %>% 
      addMarkers(lng = stores$Longitude, lat = stores$Latitude, popup = paste("<center>","<b>",stores$`Store Name`,"</center>","</b>", "<br>",stores$Location))
    
  })

  output$img1 <-renderUI({
    if(input$Category == "Cleanser"){
      img(width = 175, height = 175, src = "Cleansers.png", style = "margin-left:15px; margin-top:10px")
    }
    else if(input$Category == "Eye Treatment"){
      img(width = 175, height = 175,src = "Eye Treatments.png", style = "margin-left:15px; margin-top:10px")
    }
    else if(input$Category == "Face Masks"){
      img(width = 175, height = 175,src = "FaceMasks1.png", style = "margin-left:15px; margin-top:10px")
    }
    else if(input$Category == "Face Treatment"){
      img(width = 175, height = 175,src = "Facial Treatments.png", style = "margin-left:15px; margin-top:10px")
    }
    else if(input$Category == "Moisturiser"){
      img(width = 175, height = 175,src = "Moisturiser.png", style = "margin-left:15px; margin-top:10px")
    }
    else if(input$Category == "Eye Make Up Remover"){
      img(width = 175, height = 175,src = "EMUR.png", style = "margin-left:15px; margin-top:10px")
    }
    else if(input$Category == "Scrub"){
      img(width = 175, height = 175,src = "Scrubs.png", style = "margin-left:15px; margin-top:10px")
    }
    else if(input$Category == "Toner"){
      img(width = 175, height = 175,src = "Toners.png", style = "margin-left:15px; margin-top:10px")
    }
    else if(input$Category == "Neck Cream"){
      img(width = 175, height = 175,src = "Neck Cream.png", style = "margin-left:15px; margin-top:10px")
    }
  })
  
  reviews_data<-reactive({
    data<-read_excel(paste0("Data Files/Reviews/",input$Category,".xlsx"),sheet = 1)
    data
  })
  
  
  ########################"
  output$text1<-renderUI({
    summary<-read_excel(paste0("Data Files/Summary/",input$Category,".xlsx"),sheet = 1)
    summary$Price<-as.integer(summary$Price)
    dat<-calcul()
    product=paste(dat[input$table1_cell_clicked$row,1])
    w1=which(summary$`Product Name`==product)
    ###Print Description"

    fluidRow(tags$head(tags$style(
      "body { word-wrap: break-word; }"
    )),
      column(2,uiOutput("img1",inline = T)),br(),br(),
    column(10,h3(product),h4(summary$`Brand Name`[w1])),
    
    fluidRow(column(11,h3("Description",style = 'margin-left:30px; margin-top :40px'),
      p(summary$Description[w1], style = "margin-left:30px"))
      
    ))
    
    
  })

    
  output$wc <- renderPlot({
    reviews<-reviews_data()
    dat<-calcul()
    product=dat[input$table1_cell_clicked$row,1]
    w2<-which(reviews$`Product Name`==product)
    text<-reviews$Reviews[w2]
    
    docs <- Corpus(VectorSource(text))
    inspect(docs)
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove english common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove your own stop word
    # specify your stopwords as a character vector
    docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    d<-head(d,1000)
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  ################
  output$table2 <- DT::renderDataTable({
    reviews<-reviews_data()
    dat<-calcul()
    product=dat[input$table1_cell_clicked$row,1]
    w3<-which(reviews$`Product Name`==product)
    
    rv=reviews$Reviews[w3]
    d2<-data.frame("Reviews"=rv,
                   "Score"=reviews$`Sentiment Score`[w3]
    )
    order.scores <- order(d2$Score, decreasing = TRUE) 
    
    v <- d2[order.scores,]
    datatable(v, selection=list(mode="single", target="cell"),rownames = F)
    
  })
  })


shinyApp(ui = ui, server = server)
