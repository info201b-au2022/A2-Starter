## A2: Challenge 
# Assignment 2: Challenge 
# dhendry@uw.edu 
# Autumn 2022 

## A2: Challenge: Developing data systems ----
# 
# "Count Love" and similar systems are developed by writing functions that each
# do one thing. The function, `get_purposes()`, which you wrote earlier, is an
# example. 
#
# To work on this part A2, you should carefully examine the column `Tags` in the 
# `protests` dataframe.  Also, you will find this blog post useful: 
# https://countlove.org/blog/new-protest-topics.html 
#
# Also, below, you will see the implementation of two functions, 
# which you should study:
#    get_positions() 
#    format_doc() 
# 
# In this part of A2, you will write two new functions: 
#   
# FUNCTION #1
# filter_positions() takes two arguments: 
#     purpose                A string for the purpose of the protest
#     position_taken         A string for the position taken or NULL
#                            The default value should be NULL 
#                            
# This function should return a dataframe of the protests that match on `purpose` 
# and `position_taken`. If `position_taken` is NULL then all protests that 
# match the `purpose` should be returned.
# 
# FUNCTION #2
# filter_and_report() takes three arguments: 
#     purpose               A string for the purpose of the protest
#     position_taken        A string for the position taken or NULL
#                           The default value should be NULL
#                           
# This function should do two things:(1) It should call `filter_positions`,
# to create a sub-set of protests; and (2) It should call `format_doc()`, 
# to create a simple report.
# 
# CODING PROMPT 
# (1) Write the two functions (#1 and #2), just described 
# (2) Write code to test the functions and show that they work
# (3) Document your code. 
#
# GRADING 
# Your work will be graded as complete/incomplete and tow challenge points 
# will be assigned for very high-quality work.

## Filtering positions ---- 
# This function returns a vector of "For" or "Against" positions 
# for a particular purpose of a protest. Protest purposes include 
# "Civil Rights", "Environment", etc.
#
#    purpose        A string for the purpose of the protest  
#    for_position   A boolean to show either "For" (TRUE) or "Against" (FALSE) 
#                   positions
#

protests <- read.csv("https://countlove.org/data/data.csv")

get_positions <- function (purpose, for_position=TRUE) {
  # Find all tags for a particular purpose
  matches <- str_detect(protests$Tags, purpose)
  category_tags <- protests[matches,"Tags"]
  
  # Check that the cateory name is in protests
  if(length(category_tags) == 0) {
    stop(paste0("Category not found (Name:", purpose, ")."))
  }
  
  # Use a regular expression to find all "For" or "Against" positions
  # See stringr (https://stringr.tidyverse.org/)
  if (for_position == TRUE ) {
    positions <- str_extract_all(category_tags,"For .*?;|For .*?$")
  } else{
    positions <- str_extract_all(category_tags,"Against .*?;|Against .*?$")
  }
  # Remove those cases where no positions are found
  lens <- lapply(positions,length)
  positions <- positions[lens > 0]
  
  # Turn the list of positions into a vector and tidy up the 
  # strings (remove the semi-colon)
  v <- unlist(positions)
  v <- str_replace(v, ";", "")
  
  # Remove duplicates and sort
  v <- sort(unique(v))
  return (v)
}

## Reporting ---- 
# This function produces a markdown report that summarizes a subset 
# of protests. 
# 
#    protest_df     A dataframe of the subset of protests to be summarized 
#    category       A string for the protest category name
#    position_taken A string of the position taken.  If NULL, then the data 
#                   has NOT been filtered by a position_taken 
#
# The output takes this form: 
#     # Category:  Environment 
#     ## Position taken:  For renewable energy 
#     _Summary_
#
#     * Number of Protests: 19
#     * Total Attendees: 686
#
#     _List of Protests (Attendees)_
#
#     * 2017-04-01: Indianapolis, IN [Article](http://www.tribstar.com/news/solar-proponents-rally-against-bill/article_7c7a7af8-b024-5ad8-a2de-75154083601c.html) (_160_)
#     * 2017-04-24: Cambridge, MA [Article](http://www.cambridgeday.com/2017/04/25/climate-rally-calls-for-all-renewable-energy-steps-against-anti-science-of-white-house/) (_NA_)
#     ... 
#
# To view the report use `cat()` print the string and then save in a markdown file and view the file. 
#    output <- format_doc()
#    cat(output)
#    copy output from the console and save in a file named `report.md`
#    view `report.md' in a document viewer (e.g. Atom > Packages > Markdown Previewer)

format_doc <- function(protest_df, purpose, position_taken=NULL) {
  # Heading components
  heading1 <- paste("# Protest purpose: ", purpose, "\n")
  heading2 <- "\n"
  if (!is.null(position_taken)) {
    heading2 <- paste("## Position taken: ", position_taken, "\n")
  }
  
  # Summary component
  num_protests <- nrow(protest_df)
  total_attendees <- sum(protest_df$Attendees, na.rm=TRUE)
  
  summary <- paste0("_Summary_\n\n", 
                    "* Number of Protests: ", num_protests, "\n", 
                    "* Total Attendees: ", total_attendees, "\n\n",
                    "_List of Protests (Attendees)_\n\n")
  
  # List of protests component (Note: Bullet_list is a vector)
  bullet_list <- paste0("* ", protest_df$Date, ": ", protest_df$Location,
    " [Article](", protest_df$Source, ")", " (_", protest_df$Attendees, "_)\n")
                       
  # Collapse the vector into a string
  url_str <- paste0(bullet_list, collapse="")
  
  # Paste together each of the report components
  md_doc <- paste0(heading1, heading2, summary, url_str)
  
  return(md_doc)
}


## Filtering positions ---- 
# YOUR IMPLEMENTATION OF FUNCTION #1 

## Filter and Report ---- 
# YOUR IMPLEMENTATION OF FUNCTION #2 

## Testing ----
# TEST AND DEMOSTRATE YOUR FUNCTIONS HERE 


filter_protests <- function (category, position_taken=NULL) {
  protests_filtered <- protests[str_detect(protests$Tags,category),]
  if (!is.null(position_taken)) {
    protests_filtered <- protests_filtered[str_detect(protests_filtered$Tags,position_taken),]
  }
  return(protests_filtered)
}

filter_and_report <- function (category, position_taken=NULL) {
  p <- filter_protests(category, position_taken)
  report <- format_doc(p, category, position_taken)
  return(report)
}

count_positions <- function(position_taken) {
  t <- str_detect(protests$Tags, position_taken)
  return(sum(t))
}

print_positions <- function(e) {
  position_taken <- names(e)[1]
  num <- e[[position_taken]]
  print(paste0(position_taken, ": ", num, '.'))
}

# An example text 
purpose <- "Environment"
position <- "Against noise"
output <- filter_and_report(purpose,position)
cat(output)

t <- get_positions("Environment")
tt<- sapply(t,count_positions)
sapply(tt,print_positions)

lapply(t, paste0())

