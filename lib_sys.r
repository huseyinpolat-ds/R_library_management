Sys.setenv(lang = "en")
library(R6)
library(tidyverse)
library(Rcpp)

################################# C++ functions #################################

####### ISBN Validate

sourceCpp("isbnvalidate.cpp")

####### Card number validate

sourceCpp("card_number_validate.cpp")

################################# API function #################################

####### Mobile validate API

# mobile_validate_via_api = function (mobile) {
# 
#   if(!is.character(mobile))
#     stop("Error: Phone number should be written as string!")
# 
#   only_digits <- gsub("[^0-9]","\\", mobile)
# 
#   if(nchar(only_digits) < 10)
#     # 10 is selected approximately, there is no concrete reasoning behind as
#     # I am not familiar with each country'mobile phone number format
#     stop("Error: Phone number should contain country code and cannot be shorter than 10 digits!")
# 
# 
#   listout = list()
# 
#   if (substr(mobile,1,3) == '+90') {
#     listout$country_name = "Turkey"
#   } else if (substr(mobile,1,3) == '+44') {
#     listout$country_name = "UK"
#   } else if (substr(mobile,1,3) == '+48') {
#     listout$country_name = "Poland"
#   } else {
#     listout$country_name = "US"
#   }
# 
# 
#   listout$is_valid = TRUE
# 
#   listout$type = "mobile"
# 
# 
#   #     if(!require(httr)){
#   #         install.packages("httr")
#   #     }
#   #     if(!require(jsonlite)){
#   #         install.packages("jsonlite")
#   #     }
# 
# 
#   #     library(httr)
#   #     library(jsonlite)
# 
# 
#   #     api_call_key = "https://phonevalidation.abstractapi.com/v1/?api_key=0e51948306fb42569606aef1df25f894&phone="
# 
#   #     res = GET(paste(api_call_key, mobile, sep = ""))
# 
#   #     data = fromJSON(rawToChar(res$content))
# 
#   #     listout = list(is_valid = data$valid, country_name = data$country$name, type = data$type)
# 
#   return(listout)
# 
# }

mobile_validate_via_api = function (mobile) {

  if(!is.character(mobile))
    stop("Error: Phone number should be written as string!")

  only_digits <- gsub("[^0-9]","\\", mobile)

  if(nchar(only_digits) < 10)
    # 10 is selected approximately, there is no concrete reasoning behind as
    # I am not familiar with each country'mobile phone number format
    stop("Error: Phone number should contain country code and cannot be shorter than 10 digits!")

  if(!require(httr)){
    install.packages("httr")
  }
  if(!require(jsonlite)){
    install.packages("jsonlite")
  }


  library(httr)
  library(jsonlite)


  api_call_key = "https://phonevalidation.abstractapi.com/v1/?api_key=0e51948306fb42569606aef1df25f894&phone="

  res = GET(paste(api_call_key, mobile, sep = ""))

  data = fromJSON(rawToChar(res$content))

  listout = list(is_valid = data$valid, country_name = data$country$name, type = data$type)

  return(listout)

}

################################# R6 classes #################################


####### "Book" class

Book <-
  R6Class(
    "Book",
    public = list(
      initialize = function(title, author, isbn, publisher) {
        if (nchar(title) < 1 | !is.character(title))
          stop("Error: Title of the book must be character and have at least one letter!")
        if (nchar(author) < 1 | !is.character(author))
          stop("Error: Author of the book must be character and have at least one letter!")
        if (nchar(publisher) < 1 | !is.character(publisher))
          stop("Error: Publisher of the book must be character and have at least one letter!")
        if (!isbnvalidate(isbn))
          stop("Error: ISBN number is not valid!")
        
        private$title <- title
        private$author <- author
        private$isbn <- toupper(gsub("[^0-9a-zA-z]*","",isbn)) ## remove anything else than numbers and alphabetical characters
        private$publisher <- publisher
        private$date_added <- Sys.time()
        private$.is_removed <- FALSE
        private$.date_removed <- as.POSIXct(NA)
        private$.is_borrowed <- FALSE
      },
      
      borrow = function() {
        private$.is_borrowed = TRUE # Will be called when a book is borrowed 
      },
      
      returned = function() {
        private$.is_borrowed = FALSE # Will be called when a book is returned
      },
      
      remove = function() {
        private$.is_removed <- TRUE
        private$.date_removed <- Sys.time() # Will be called when a book is removed
      },
      
      
      dict = function () {  # Creating a dictionary for each book 
        # so we can easily add it easily to dataframe.
        l = list(
          "Title" = private$title,
          "Author" = private$author,
          "ISBN" = private$isbn,
          "Publisher" = private$publisher,
          "Date Added" = private$date_added,
          "Is Removed" = private$.is_removed,
          "Date Removed" = private$.date_removed,
          "Is Borrowed" = private$.is_borrowed
        )
        return(data.frame(l)) # return as dataframe
      }
      
    ),
    
    private = list(
      title = NULL,
      author = NULL,
      isbn = NULL,
      publisher = NULL,
      date_added = as.POSIXct(NA),
      .is_removed = FALSE,
      .date_removed = as.POSIXct(NA),
      .is_borrowed = FALSE
    )
  )

####### "Member" class

Member <- R6Class(
  "Member",
  public = list(
    initialize = function(name, address, phone, card_number) {
      if (nchar(name) < 1 | !is.character(name))
        stop("Error: Name of the member must be character and have at least one letter!")
      if (nchar(address) < 1 | !is.character(address))
        stop("Error: Address of the member must be character and have at least one letter!")
      api_call = mobile_validate_via_api(phone) # api call for mobile validation
      if (!api_call$is_valid) # When the number is not valid
        stop("Error: Given mobile number is not valid!")
      if (api_call$type != "mobile") # When the number is not mobile
        stop("Error: Given mobile number is not a mobile number!")
      card_number_validate(card_number)
      #api_call = list(country_name = "UK") # To be romeved
      
      
      
      private$name <- name
      private$address <- address
      private$phone <- phone
      private$country <- api_call$country_name
      private$card_number <- card_number_validate(card_number)
      private$registration_date <- Sys.time()
      private$.is_removed <- FALSE
      private$.date_removed <- as.POSIXct(NA) # NA as date
    },
    
    
    remove = function() {
      private$.is_removed <- TRUE # Will be called when a member is removed
      private$.date_removed <- Sys.time()
    },
    
    
    dict = function () {  # Creating a dictionary for each book 
      # so we can easily add it easily to dataframe.
      l = list(
        "Name" = private$name,
        "Address" = private$address,
        "Mobile" = private$phone,
        "Country" = private$country,
        "Card Number" = private$card_number,
        "Registration Date" = private$registration_date,
        "Is Removed" = private$.is_removed,
        "Date Removed" = private$.date_removed
      )
      return(data.frame(l)) # return as dataframe
    }
    
  ),
  
  private = list(
    name = NULL,
    address = NULL,
    phone = NULL,
    country = NULL,
    card_number = NULL,
    registration_date = as.POSIXct(NA), # NA as date
    .is_removed = FALSE,
    .date_removed = as.POSIXct(NA) # NA as date
  )
)

####### "Transaction" class

Transaction <- R6Class(
  "Transaction",
  public = list(
    initialize = function(book, member, checkout_date, return_date) {
      private$book <- book # Book instance
      private$member <- member # Member instance
      private$checkout_date <- checkout_date
      private$.return_date <- return_date
    },
    
    dict = function () {  # Creating a dictionary for each book 
      # so we can easily add it easily to dataframe.
      l = list(
        "ISBN" = private$book$dict()$ISBN, # Get ISBN from Book instance
        "Member Card Number" =  private$member$dict()$Card.Number, # Get Card.Number from Member instance
        "Checkout Date" = private$checkout_date,
        "Return Date" = private$.return_date
      )
      return(data.frame(l)) # Return as dataframe
    },
    
    update_return_date = function(returndate) {
      private$.return_date = returndate # Update return date
    }
    
  ),
  
  private = list(
    book = NULL,
    member = NULL,
    checkout_date = as.POSIXct(NA, tz = "UTC" ),
    .return_date = as.POSIXct(NA, tz = "UTC" )
  )
)

####### "Library" class

Library <- R6Class(
  "Library",
  private = list(
    books = list(), # Book instances
    books_df = list(), # Dataframe of book instances
    members = list(), # Member instances
    members_df = list(), #Dataframe of member instances
    transactions = list(), # Transaction instances
    transactions_df = list() # Dataframe of transaction instances
  ),
  public = list(
    initialize = function() {
      private$books <- list()
      private$books_df <- list()
      private$members <- list()
      private$members_df <- list()
      private$transactions <- list()
      private$transactions_df <- list()
    },
    
    add_book = function(book) {
      if (length(private$books) > 0) { # In case of adding more books
        idx <- which(sapply(private$books, function(t) {
          identical(t$dict()$ISBN, book$dict()$ISBN) & # Compare given ISBN with the existing book ISBNs 
            identical(t$dict()$Is.Removed, FALSE)
        })) 
        if (length(idx) > 0) { # If book already exists
          stop("Error: Book already exists in the database!")
        }
        else { # If book doesn't exist
          private$books <- c(private$books, book) # Add book to Book instances
          private$books_df <- rbind(private$books_df, book$dict()) # Add book to dataframe of Book instances
        }
      } else { # In case of adding the frist book
        private$books <- c(private$books, book) # Add book to Book instances
        private$books_df <- rbind(private$books_df, book$dict()) # Add book to dataframe of Book instances
      }
    },
    
    remove_book = function(isbn) {
      isbn <- toupper(gsub("[^0-9a-zA-z]*","",isbn)) # remove anything else than numbers and alphabetical characters
      idx <- which(sapply(private$books, function(t) { # Compare given ISBN with the existing book ISBNs 
        identical(t$dict()$ISBN, isbn) &
          identical(t$dict()$Is.Removed, FALSE)
      }))
      if (length(idx) > 0) { # If book already exists
        if (private$books[[idx]]$dict()$Is.Borrowed == TRUE) { # Check if book is borrowed by someone at the moment
          stop("Error: Book is currently borrowed and it cannot be deleted unless it is returned.")
        } else { 
          
          private$books_df[private$books_df["ISBN"] == isbn&
                             private$books_df["Is.Removed"] == FALSE,
                           "Date.Removed"] = Sys.time() # Update removed time in dataframe of Book instances
          
          private$books_df[private$books_df["ISBN"] == isbn&
                             private$books_df["Is.Removed"] == FALSE,
                           "Is.Removed"] = TRUE # Mark as removed in dataframe of Book instances
          private$books[[idx]]$remove() # Update removed time and mark as removed in Book instance
        }
        
      } else { # If book doesn't exist
        stop("Error: Book not found!")
      }
      
      
    },
    
    show_books = function() { # Just to return list of books
      df <- private$books_df[order(private$books_df$Date.Added, decreasing = TRUE),] # Order by added date
      df$Date.Added <- format(as.Date(df$Date.Added, origin = .Date(0)), "%Y/%m/%d") # Format date
      df$Date.Removed <- format(as.Date(df$Date.Removed, origin = .Date(0)), "%Y/%m/%d") # Format date
      
      return(df)
    },
    
    
    add_member = function(member) {
      if (length(private$members) > 0) { # In case of adding more members
        idx <- which(sapply(private$members, function(t) { # Search if given card number is already registered
          identical(t$dict()$Card.Number, member$dict()$Card.Number)
        }))
        if (length(idx) > 0) { # If card number is already registered - there is an existing member
          stop("Error: User with this card number already exists in the database!")
        }
        else { # If card number is not registered before
          private$members <- c(private$members, member) # Add member to Member instaces
          private$members_df <-
            rbind(private$members_df, member$dict()) # Add member to dataframe of Member instaces
        }
      } else { # In case of adding the first member
        private$members <- c(private$members, member) # Add member to Member instaces
        private$members_df <-
          rbind(private$members_df, member$dict()) # Add member to dataframe of Member instaces
      }
    },
    
    
    remove_member = function(member_card_number) {
      member_card_number <- card_number_validate(member_card_number) # Check if given card number is correct and reformat it
      idx <- which(sapply(private$members, function(t) { # Search if given card number is already registered
        identical(t$dict()$Card.Number, member_card_number)
      }))
      if (length(idx) > 0) { # If card number is registered before - there is an existing member
        private$members[[idx]]$remove()
        private$members_df[private$members_df["Card.Number"] == member_card_number,
                           "Is.Removed"] = TRUE
        private$members_df[private$members_df["Card.Number"] == member_card_number,
                           "Date.Removed"] = Sys.time()
      } else { # If card number is not registered before
        stop("Error: Member not found!")
      }
      
    },
    
    show_members = function() { # Just to return list of members
      df <-
        private$members_df[order(private$members_df$Registration.Date, decreasing = TRUE),] # Order by registration date
      df$Registration.Date <-
        format(as.Date(df$Registration.Date, origin = .Date(0)), "%Y/%m/%d") # Format date
      df$Date.Removed <- format(as.Date(df$Date.Removed, origin = .Date(0)), "%Y/%m/%d") # Format date
      return(df)
    },
    
    checkout_book = function(isbn, member_card_number) {
      member_card_number <- card_number_validate(member_card_number) # Check if given card number is valid
      isbn <- toupper(gsub("[^0-9a-zA-z]*","",isbn)) # Remove unnecassary characters from ISBN
      idx <- which(sapply(private$books, function(t) { # Search if given book exists
        identical(t$dict()$ISBN, isbn) &
          identical(t$dict()$Is.Removed, FALSE)
      }))
      if (length(idx) > 0) {
        book <- private$books[[idx]]
      } else {
        stop("Error: Book not found!")
      }
      
      idx2 <- which(sapply(private$members, function(t) {
        identical(t$dict()$Card.Number, member_card_number) # Search if there's a member with given card number
      }))
      if (length(idx2) > 0) {
        member <- private$members[[idx2]]
      } else {
        stop("Error: Member not found.")
      }
      if (book$dict()$Is.Borrowed == FALSE) {
        transaction <-
          Transaction$new(book, member, Sys.time(), as.Date(NA))
        private$transactions <- c(private$transactions, transaction)
        private$transactions_df <-
          rbind(private$transactions_df, transaction$dict())
        private$books[[idx]]$borrow()
        private$books_df[private$books_df$ISBN == isbn & 
                           private$books_df$Is.Removed == FALSE, 
                         "Is.Borrowed"] = TRUE
      }
      
      else {
        stop("Error: Book is already checkedout, it is not available.")
      }
    },
    
    return_book = function(isbn, member_card_number) {
      isbn <- toupper(gsub("[^0-9a-zA-z]*","",isbn))
      member_card_number <- card_number_validate(member_card_number)
      return_date <- Sys.time()
      
      idx <- which(sapply(private$transactions, function(t) {
        identical(t$dict()$ISBN, isbn) &
          identical(t$dict()$Member.Card.Number, member_card_number) &
          is.na(t$dict()$Return.Date)
      }))
      if (length(idx) > 0) {
        private$transactions[[idx]]$update_return_date(return_date)
        private$transactions_df[private$transactions_df["ISBN"] == isbn &
                                  private$transactions_df["Member.Card.Number"] == member_card_number,
                                "Return.Date"] = return_date
        
        idx2 <- which(sapply(private$books, function(t) {
          identical(t$dict()$ISBN, isbn)
        }))
        private$books[[idx2]]$returned()
        private$books_df[private$books_df$ISBN == isbn & 
                           private$books_df$Is.Removed == FALSE, 
                         "Is.Borrowed"] = FALSE
        
        
      } else {
        stop("Error: Transaction not found.")
      }
      
    },
    
    show_transactions = function () {
      df <-
        private$transactions_df[order(private$transactions_df$Checkout.Date, decreasing = TRUE),]
      df$Checkout.Date <-
        format(as.Date(df$Checkout.Date, origin = .Date(0)), "%Y/%m/%d")
      df$Return.Date <- format(as.Date(df$Return.Date, origin = .Date(0)), "%Y/%m/%d")
      return(df)
    },
    
    check1 = function() {
      return(private$books)
    }
    
  )
)
