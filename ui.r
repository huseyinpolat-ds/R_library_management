# Load required libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(shinyjs)


setwd("C:\\Users\\Ankara\\Desktop\\1stclass-2ndsemester\\Adv Pr R\\Project")


# Define UI
ui <- fluidPage(tags$head(
  # Note the wrapping of the string in HTML()
  tags$style(HTML('
        h3{
    font-size: 50px;
    color: white;
    font-weight: bold;
    
  }
  .navbar-brand {
  font-weight: bold;

  }
table {
    font-size: 15px;
}
  .form-control {
color: black;
background-color: #52e2fe;
display: block;
    width: 100%;
    height: 38px;
    padding: 8px 12px;
    font-size: 14px;}

body {
    color: #ffebe1;
    font-family: "Segoe UI", Frutiger, "Frutiger Linotype", "Dejavu Sans", "Helvetica Neue", Arial, sans-serif;

}


.bg-black {
    background-color: ##060606!important;
}

.small-box .icon-large {
  position: absolute;
  top: auto;
  bottom: 15px;
  right: 70px;
  font-size: 70px;
  color: rgba(255, 255, 255, 0.7);
}

.navbar-default .navbar-brand {
    color: white;
}

.input-group .form-control {
color: black;
background-color: #52e2fe;
display: block;
    width: 100%;
    height: 38px;
    padding: 8px 12px;
    font-size: 14px;
}

  pre.shiny-text-output {
    white-space: pre-wrap;
}
  
  '
))),

  
  useShinyjs(),
  theme = shinytheme("slate"),
  navbarPage(
    id = "navbar",
    "LIBRARY MANAGEMENT SYSTEM",
    # Input values
    tabPanel(
      "Books",
      value = 1,
      sidebarLayout(
      sidebarPanel(
        id = "sidebar-books",
        HTML("<h4>Add book</h4>"),
        width = 3,
        textInput("book_title", label = "Title", value = ""),
        textInput("book_author", label = "Author", value = ""),
        textInput("book_isbn", label = "ISBN", value = ""),
        textInput("book_publisher", label = "Publisher", value = ""),

        verbatimTextOutput("add_book_err"),
        
        actionButton("add_book", label = "Add Book", icon = icon("plus")),
        
        HTML("<h4>Remove book</h4>"),
        textInput("book_isbn_remove", label = "ISBN", value = ""),
        
        verbatimTextOutput("remove_book_err"),
        
        actionButton("remove_book", label = 
                       "Remove Book", icon = icon("trash")),
        
        HTML("<h4>Search a book</h4>"),
        
        textInput("search_books", label = "Search by any term:", value = "")
        
      ),
      #sidebarpanel
      
      mainPanel(
        fluidRow(valueBoxOutput("value1_books"),
                 valueBoxOutput("value2_books"),
                 valueBoxOutput("value3_books")),
        
        
        HTML("<h2><strong>10 Last added books</strong></h2>"),
        
        fluidRow(column(12,
                        tableOutput('table_books'))),
        
        
        uiOutput("dynamic_header_books"),
        
        verbatimTextOutput("dynamic_books_search_word_out"),
        
        fluidRow(column(12,
                        tableOutput('filtered_books_table')))
        
      ) #mainpanel
      ) #sidebarlayout
    ),
    #tabpanel
    tabPanel(
      "Transactions",
      value = 2,
      sidebarLayout(
      sidebarPanel(
        id = "sidebar-transactions",
        
        HTML("<h4>Checkout book</h4>"),
        width = 3,
        textInput("book_isbn_checkout", label = "ISBN", value = ""),
        textInput("member_card_checkout", label = "Member Card Number", value = ""),

        verbatimTextOutput("checkout_book_err"),
        
        actionButton("checkout", label = "Checkout Book", icon = icon("arrow-right")),
        HTML("<h4>Return book</h4>"),
        textInput("book_isbn_return", label = "ISBN", value = ""),
        textInput("member_card_return", label = "Member Card Number", value = ""),

        verbatimTextOutput("return_book_err"),
        
        actionButton("return", label = "Return Book", 
                     icon = icon("arrow-left")),
        
        HTML("<h4>Search for transaction</h4>"),
        
        textInput("search_transactions", 
                  label = "Search by any term:", value = "")
        
      )
      , #sidebarpanel
         
        mainPanel(
          fluidRow(valueBoxOutput("value1_transactions"),
                   valueBoxOutput("value2_transactions"),
                   valueBoxOutput("value3_transactions")),
          
          HTML("<h2><strong>Last 10 Transactions</strong></h2>"),
        
                  fluidRow(column(12,
                                  tableOutput(
                                    'table_transactions'
                                  ))),
          
        uiOutput("dynamic_header_transactions"),
        
        verbatimTextOutput("dynamic_transactions_search_word_out"),
        
        fluidRow(column(12,
                        tableOutput('filtered_transactions_table')))

         ) #mainpanel
      
      ) #sidebarlayout
    ),
    #tabpanel
    tabPanel(
      "Members",
      value = 3,
      sidebarLayout(
      sidebarPanel(
        id = "sidebar-members",
        HTML("<h4>Add member</h4>"),
        width = 3,
        textInput("member_name", label = "Full Name", value = ""),
        textInput("member_address", label = "Address", value = ""),
        textInput("member_phone", label = "Mobile number", value = ""),
        textInput(
          "member_card_registration",
          label = "Member Card Number",
          value = ""
        ),

        verbatimTextOutput("add_member_err"),
        
        
        actionButton("add_member", label = "Add Member", icon = icon("plus")),
        HTML("<h4>Remove member</h4>"),
        textInput(
          "member_card_registration_remove",
          label = "Member Card Number",
          value = ""
        ),

        verbatimTextOutput("remove_member_err"),
        
        actionButton("remove_member", 
                     label = "Remove Member", icon = icon("trash")),
        
        HTML("<h4>Search for member</h4>"),
        
        textInput("search_members", 
                  label = "Search by any term:", value = "")
        
        
      ), #sidebarpanel
      
      
      mainPanel(
        fluidRow(valueBoxOutput("value1_members"),
                 valueBoxOutput("value2_members"),
                 valueBoxOutput("value3_members")),
        
        HTML("<h2><strong>10 Last added members</strong></h2>"),
        
        fluidRow(column(12,
                        tableOutput(
                          'table_members'
                        ))),
        
        
        uiOutput("dynamic_header_members"),
        
        verbatimTextOutput("dynamic_members_search_word_out"),
        
        fluidRow(column(12,
                        tableOutput('filtered_members_table')))

      ) #mainpanel
      ) #sidebarlayout
    ) #tabpanel
  ) #navbarpage
  #) #dashboardpage
) #ui


