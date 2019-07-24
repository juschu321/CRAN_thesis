psy_packages <- ctv::available.views()[[30]]$packagelist$name
#####to get monthly download statistics (psych)####
psy_spread_monthly <- data.frame()
psy_spread_monthly <- 
  psy_download_statistics %>%
  mutate(day = format(date, "%d"), month = format(date, "%m"), 
         year = format(date, "%Y")) %>%
  group_by(year, month, package) %>%
  summarise(total = sum(count))

psy_spread_monthly <- tidyr::spread(psy_spread_monthly, key = package, value = total)

####function get_subcategory_of_psy_packages####

# takes the list of psy_packages
# returns as list of psy_packagaes with eaxch subcategory
get_subcategory_of_psy_packages <- function(psy_packages) {
  html <-
    read_html("https://cran.r-project.org/web/views/Psychometrics.html")
  
  # target data frame
  psy_package_categories <- data.frame()
  
  
  # looping over all packages
  # "psy_packages" may need to be replaced
  for (package in psy_packages) {
    # Get all a (links) with text equal to the current package name
    # then select the next higher ul (unordered list)
    # then select the p (paragraph) before that ul
    # that p should contain the name of the categoriy
    # XPATH
    query_string <-
      paste0("//a[text() = '",
             package,
             "'][1]/ancestor::ul/preceding::p[1]")
    category <-
      html %>% html_node(xpath = query_string) %>% html_text(trim = TRUE)
    
    # remove colon of category name
    category <- substr(category, 1, nchar(category) - 1)
    
    psy_package_categories <-
      rbind(psy_package_categories,
            data.frame(package = package, category = category))
  }
  
  return (psy_package_categories)
}

# takes a lot of time...
psy_download_statistics <- get_download_statistics(psy_packages)
psy_api_data <- get_api_data(psy_packages)
psy_edgelist <- get_edgelist_of_packages(psy_api_data)

#####get subcategories#####
psy_subcategories <- get_subcategory_of_psy_packages(psy_packages)


