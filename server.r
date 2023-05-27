library(tidyverse)

setwd("C:\\Users\\Ankara\\Desktop\\1stclass-2ndsemester\\Adv Pr R\\Project")

source("lib_sys.r")

library1 <- readRDS(file = "library1.rds") #library1



# Other functions

save_lib <- function () {
  
  saveRDS(library1, "library1.rds")
  
}

n_of_books <- function () {
  dim(library1$show_books() %>% filter(Is.Removed == FALSE))[1]
}

n_of_publishers <- function () {
  dim(library1$show_books() %>% 
        filter(Is.Removed == FALSE) %>% 
        select(Publisher) %>% 
        unique())[1]
} 

n_of_authors <- function () {
  dim(library1$show_books() %>% 
        filter(Is.Removed == FALSE) %>% 
        select(Author) %>% 
        unique())[1]
} 

n_of_transactions <- function () {
  dim(library1$show_transactions())[1]
}

n_of_books_borrowed <- function () {
  borrowed_books <- library1$show_transactions() %>% filter(is.na(Return.Date))
  all_books <- library1$show_books() %>% filter(Is.Removed == FALSE)
  
  joined <- merge(all_books, borrowed_books, by = "ISBN", all.x = TRUE)
  
  dim(joined %>% filter(!is.na(Member.Card.Number)))[1]
}

n_of_books_available <- function () {
  borrowed_books <- library1$show_transactions() %>% filter(is.na(Return.Date))
  all_books <- library1$show_books() %>% filter(Is.Removed == FALSE)
  
  joined <- merge(all_books, borrowed_books, by = "ISBN", all.x = TRUE)
  
  dim(joined %>% filter(is.na(Member.Card.Number)))[1]
}

n_of_members <- function() {
  
  dim(library1$show_members() %>% 
        filter(Is.Removed == FALSE))[1]
  
}

n_of_countries <- function() {
  
  dim(unique(library1$show_members() %>% 
               filter(Is.Removed == FALSE) %>% select(Country)))[1]
  
}

n_of_registrations_30_days <- function() {
  
  dim(library1$show_members() %>% 
        filter(Is.Removed == FALSE, Registration.Date >= (Sys.Date() - 30)))[1]
  
}



######### Server


server <- function (input, output) {
  ######## Books
  
  
  dynamic_books_search_df <- reactive({
    if (input$search_books == "") {
      
    } else {
      library1$show_books()[rowSums(sapply(library1$show_books(),
                                           function(x)
                                             grepl(paste0(input$search_books),
                                                   x,
                                                   ignore.case = TRUE))) > 0,]
    }
  })
  
  dynamic_books_search_word <- reactive({
    if (input$search_books == "") {
      ""
    }
    else {
      paste("Search results for the term: '",
            input$search_books,
            "'",
            sep = "")
    }
  })
  
  output$dynamic_books_search_word_out <-
    renderText({
      dynamic_books_search_word()
    })
  
  output$dynamic_header_books <- reactive({
    if (input$search_books == "") {
      ""
    }
    else {
      HTML("<h2><strong>Search results</strong></h2>")
    }
  })
  
  output$filtered_books_table <-
    renderTable(dynamic_books_search_df(),
                width = "200%")
  
  
  initial_items_books <- function() {
    output$value1_books <- renderValueBox({
      valueBox(
        formatC(n_of_books(), format = "d", big.mark = ',')
        ,
        subtitle = HTML(
          "<p style = font-size:16px><strong># of books</strong></p>"
        )
        ,
        icon = icon("book", lib = 'font-awesome')
        ,
        color = "black"
      )
    })
    
    output$value2_books <- renderValueBox({
      valueBox(
        formatC(n_of_authors(), format = "d", big.mark = ',')
        ,
        subtitle = HTML(
          "<p style = font-size:16px><strong># of authors</strong></p>"
        )
        ,
        icon = icon("pen-nib", lib = 'font-awesome')
        ,
        color = "black"
      )
    })
    
    output$value3_books <- renderValueBox({
      valueBox(
        formatC(
          n_of_publishers(),
          format = "d",
          big.mark = ','
        )
        ,
        subtitle = HTML(
          "<p style = font-size:16px><strong># of publishers</strong></p>"
        )
        ,
        icon = icon("handshake", lib = 'font-awesome')
        ,
        color = "black"
      )
    })
    
    output$table_books <-
      renderTable(head(library1$show_books() %>%
                         filter(Is.Removed == FALSE), 10)[, 1:5],
                  width = "200%")
    
  }
  
  
  initial_items_books()
  
  
  observeEvent(input$add_book, {
    
    
    tryCatch({
      book1 <-
        Book$new(
          title = input$book_title,
          author = input$book_author,
          isbn = input$book_isbn,
          publisher = input$book_publisher
        )
      
      library1$add_book(book1)
      
      save_lib()
      
      output$add_book_err <- renderText({""})
      
    },
    error = function(err) {
      output$add_book_err <- renderText({
        conditionMessage(err)
      })
    }
    
    )# trycatch
    
    initial_items_books()
    
  })
  
  observeEvent(input$remove_book, {
    tryCatch({
      library1$remove_book(input$book_isbn_remove)
      save_lib()
      output$remove_book_err <- renderText({""})
      
      
    },
    error = function(err) {
      output$remove_book_err <- renderText({
        conditionMessage(err)
      })
    })# trycatch
    
    initial_items_books()
    
  })
  
  
  ######### Transactions
  
  
  
  initial_items_transactions <- function() {
    output$value1_transactions <- renderValueBox({
      valueBox(
        formatC(
          n_of_transactions(),
          format = "d",
          big.mark = ','
        )
        ,
        subtitle = HTML(
          "<p style = font-size:16px><strong># of transactions</strong></p>"
        )
        ,
        icon = icon("right-left", lib = 'font-awesome')
        ,
        color = "black"
      )
    })
    
    output$value2_transactions <- renderValueBox({
      valueBox(
        formatC(
          n_of_books_borrowed(),
          format = "d",
          big.mark = ','
        )
        ,
        subtitle = HTML(
          "<p style = font-size:16px><strong># of books currently borrowed</strong></p>"
        )
        ,
        icon = icon("arrow-right", lib = 'font-awesome')
        ,
        color = "black"
      )
    })
    
    output$value3_transactions <- renderValueBox({
      valueBox(
        formatC(
          n_of_books_available(),
          format = "d",
          big.mark = ','
        )
        ,
        subtitle = HTML(
          "<p style = font-size:16px><strong># of books currently available</strong></p>"
        )
        ,
        icon = icon("check", lib = 'font-awesome')
        ,
        color = "black"
      )
    })
    
    transactions <- library1$show_transactions()
    all_books <- library1$show_books()
    members <- library1$show_members()

    booksandtransactions <-
      merge(transactions, all_books, by = "ISBN")
    alljoined <-
      merge(booksandtransactions,
            members,
            by.x = "Member.Card.Number",
            by.y = "Card.Number")
    
    alljoined <- alljoined[order(alljoined$max_date, decreasing = TRUE),]

    to_show_search <<-
      alljoined[, c(
        "ISBN",
        "Title",
        "Author",
        "Member.Card.Number",
        "Name",
        "Mobile",
        "Checkout.Date",
        "Return.Date"
      )]
    to_show <-
      alljoined[, c("Title",
                    "Author",
                    "Publisher",
                    "Name",
                    "Checkout.Date",
                    "Return.Date")]

    output$table_transactions <- renderTable(head(to_show, 10),
                                             width = "200%")
    
    
  }
  
  
  initial_items_transactions()
  
  
  dynamic_transactions_search_df <- reactive({
    if (input$search_transactions == "") {
      
    } else {
      to_show_search[rowSums(sapply(to_show_search,
                                    function(x)
                                      grepl(paste0(input$search_transactions),
                                            x,
                                            ignore.case = TRUE))) > 0,]
    }
  })
  
  dynamic_transactions_search_word <- reactive({
    if (input$search_transactions == "") {
      ""
    }
    else {
      paste("Search results for the term: '",
            input$search_transactions,
            "'",
            sep = "")
    }
  })
  
  output$dynamic_transactions_search_word_out <-
    renderText({
      dynamic_transactions_search_word()
    })
  
  output$dynamic_header_transactions <- reactive({
    if (input$search_transactions == "") {
      ""
    }
    else {
      HTML("<h2><strong>Search results</strong></h2>")
    }
  })
  
  output$filtered_transactions_table <-
    renderTable(dynamic_transactions_search_df(),
                width = "200px")
  
  observeEvent(input$checkout, {
    tryCatch({
      library1$checkout_book(
        isbn = input$book_isbn_checkout,
        member_card_number = input$member_card_checkout
      )
      
      save_lib()
      
      output$checkout_book_err <- renderText({""})
      
      
    },
    error = function(err) {
      output$checkout_book_err <- renderText({
        conditionMessage(err)
      })
    })# trycatch
    
    initial_items_transactions()
    
  })
  
  
  
  observeEvent(input$return, {
    tryCatch({
      library1$return_book(
        isbn = input$book_isbn_return,
        member_card_number = input$member_card_return
      )
      save_lib()
      output$return_book_err <- renderText({""})
      
    },
    error = function(err) {
      output$return_book_err <- renderText({
        conditionMessage(err)
      })
    })# trycatch
    
    initial_items_transactions()
    
  })
  
  
  
  ######### Members
  
  
  dynamic_members_search_df <- reactive({
    if (input$search_members == "") {
      
    } else {
      library1$show_members()[rowSums(sapply(library1$show_members(),
                                             function(x)
                                               grepl(paste0(input$search_members),
                                                     x,
                                                     ignore.case = TRUE))) > 0,]
    }
  })
  
  dynamic_members_search_word <- reactive({
    if (input$search_members == "") {
      ""
    }
    else {
      paste("Search results for the term: '",
            input$search_members,
            "'",
            sep = "")
    }
  })
  
  output$dynamic_members_search_word_out <-
    renderText({
      dynamic_members_search_word()
    })
  
  output$dynamic_header_members <- reactive({
    if (input$search_members == "") {
      ""
    }
    else {
      HTML("<h2><strong>Search results</strong></h2>")
    }
  })
  
  output$filtered_members_table <-
    renderTable(dynamic_members_search_df(),
                width = "200%")
  
  initial_items_members <- function() {
    output$value1_members <- renderValueBox({
      valueBox(
        formatC(n_of_members(), format = "d", big.mark = ',')
        ,
        subtitle = HTML(
          "<p style = font-size:16px><strong># of active members</strong></p>"
        )
        ,
        icon = icon("id-card", lib = 'font-awesome')
        ,
        color = "black"
      )
    })
    
    output$value2_members <- renderValueBox({
      valueBox(
        formatC(
          n_of_countries(),
          format = "d",
          big.mark = ','
        )
        ,
        subtitle = HTML(
          "<p style = font-size:16px><strong>Members from # of countries</strong></p>"
        )
        ,
        icon = icon("globe", lib = 'font-awesome')
        ,
        color = "black"
      )
    })
    
    output$value3_members <- renderValueBox({
      valueBox(
        formatC(
          n_of_registrations_30_days(),
          format = "d",
          big.mark = ','
        )
        ,
        subtitle = HTML(
          "<p style = font-size:16px><strong># of registrations in last 30 days</strong></p>"
        )
        ,
        icon = icon("user-pen", lib = 'font-awesome')
        ,
        color = "black"
      )
    })
    
    output$table_members <-
      renderTable((
        library1$show_members() %>%
          filter(Is.Removed == FALSE) %>%
          head(10)
      )[, 1:6], width = "200%")
  }
  
  
  initial_items_members()
  
  observeEvent(input$add_member, {
    tryCatch({
      member1 <-
        Member$new(
          name = input$member_name,
          address = input$member_address,
          phone = input$member_phone,
          card_number = input$member_card_registration
        )
      
      library1$add_member(member1)
      
      save_lib()
      
      output$add_member_err <- renderText({""})
      
      
    },
    error = function(err) {
      output$add_member_err <- renderText({
        conditionMessage(err)
      })
    })# trycatch
    
    initial_items_members()
    
  })
  
  
  
  observeEvent(input$remove_member, {
    tryCatch({
      library1$remove_member(member_card_number = input$member_card_registration_remove)
      
      save_lib()
      output$remove_member_err <- renderText({""})
      
    },
    error = function(err) {
      output$remove_member_err <- renderText({
        conditionMessage(err)
      })
    })# trycatch
    
    initial_items_members()
    
  })
  
  
} ## server