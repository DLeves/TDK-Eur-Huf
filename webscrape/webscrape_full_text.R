library(tidyverse)
library(rvest)
library(deeplr)
library(openxlsx)
library(httr)

get_last_page_number = function(start_link) {
  last_page = read_html(start_link) %>%
    html_nodes(".page-item:nth-child(5) .page-link") %>%
    html_attr("href") %>%
    substr(., 7, nchar(.)) %>%
    as.numeric()
  
  return(last_page)
}

get_article_links = function(search_page) {
  links = search_page %>%
    html_nodes("#onesignal-slidedown-cancel-button , .card-title a") %>%
    html_attr("href") %>%
    as.data.frame() %>%
    rename(link = ".") %>%
    filter(str_detect(link, "^/gazdasag/")) %>%
    mutate(link = paste0("https://hang.hu", link))
  
  return(links)
}

scrape_article = function(link, hungarian_months) {
  # Send the HTTP request and capture the response
  response <- GET(link)
  
  # Check if the status code is 404 (Not Found) and returns a NULL
  if (status_code(response) == 404) {
    print(paste0("Page not found (404):", link))
    return(NULL)
  }
  
  article_page = read_html(link)
  
  title = article_page %>%
    html_nodes(".mb-lg-45") %>%
    html_text()
  
  text = article_page %>%
    html_nodes("p") %>%
    html_text() %>%
    paste(., collapse = " ") %>%
    str_replace(., "Minden jog fenntartva © 2018-2024 - Magyar Hang", "")
  
  date = article_page %>%
    html_nodes(".date.d-lg-inline") %>%
    html_text() %>%
    str_replace_all(., hungarian_months) %>%
    substr(., 1, 13) %>%
    as.Date(., format = "%Y. %m. %d.")
  
  article = data.frame(
    link = link,
    title = title,
    text = text,
    date = date
  )
  
  return(article)
}

scrape_magyar_hang = function(start_link) {
  # Const variable
  hungarian_months = c(
    "január" = "01.",
    "február" = "02.",
    "március" = "03.",
    "április" = "04.",
    "május" = "05.",
    "június" = "06.",
    "július" = "07.",
    "augusztus" = "08.",
    "szeptember" = "09.",
    "október" = "10.",
    "november" = "11.",
    "december" = "12."
  )
  
  # Getting the number of the last page
  last_page = get_last_page_number(start_link)
  
  # making an empty dataframe to fill with the article data
  df = data.frame(
    link = character(),
    date = Date(),
    title = character(),
    text = character()
  )
  
  # Iterating trough the available search pages
  for (i in 1:last_page) {
    # Setting the i-th search page link
    search_page_link = paste0(start_link, "?page=", i)
    
    # Reading the HTML of the search page
    print(paste0("Reading search page ", i, "/", last_page))
    search_page = read_html(search_page_link)
    
    # Getting the article links from the searchpage
    links = get_article_links(search_page)
    
    # Quitting the loop if the last page's article(s) aren't relevant
    if((nrow(links) == 0) & (i == last_page)){
      return(df)
    } else if((nrow(links) == 0)){
      next
    }
    
    # Iterating trough the article links of the i-th search page
    for (j in 1:nrow(links)) {
      article = NULL
      
      # scraping the j-th article
      article = scrape_article(links$link[j], hungarian_months)
      
      # adding the scraped data to the initially empty output dataframe
      if (!is.null(article)) {
        df = rbind(df, article)
      }
      
    }
    
    # Sleepin for 1 sec, so we won't get banned
    print(paste0(nrow(df), " articles scraped so far"))
    Sys.sleep(1)
  }
  
  # returning the scraped data
  return(df)
}

# Test:
# start_link = "https://hang.hu/kereses/cimke/varga-mihaly-660"
# df = scrape_magyar_hang(start_link)
