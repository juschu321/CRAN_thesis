#contains functions for various data collection (e.g. download statistics, dependencies)

library(ctv)
library(cranlogs)
library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(magrittr)
library(xml2)
library(lubridate)
library(dplyr)

####function get_packages_per_ctv####
# Packages per CTV
# Returns a list of all packages and their ctv name and core specification
get_packages_per_ctv <- function () {
    package_ctv <- data.frame()
    for (view in ctv::available.views()) {
        packagelist <- view[["packagelist"]]
        for (i in 1:dim(packagelist)[1]) {
            core <- (packagelist[["core"]][i])
            name <- (packagelist[["name"]][i])
            package_ctv <-
                rbind(package_ctv,
                      data.frame(
                          package = name,
                          ctv = view$name,
                          core = core
                      ))
        }
    }
    return(package_ctv)
}


####funtion get_api_data#####
# Takes a string of a package
# Returns a list of the corresponding API-data
r_api <- function(package) {
    url <- modify_url("http://crandb.r-pkg.org/", path = package)
    
    resp <- GET(url)
    if (http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
    }
    parsed <-
        jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}


# Takes a list of package names as strings
# Returns the API data as list
get_api_data <- function (packages) {
    api_data <- list()
    for (package in packages) {
        package.api.data <- r_api(package)
        api_data <- c(api_data, list(package.api.data))
    }
    return(api_data)
}



####function get_edgelist_of_packages(api_data_of_packages)####
# Takes API data as list
# Returns a edgelist of "depends", "imports", "suggests"
get_edgelist_of_packages <- function (api_data_of_packages) {
    edgelist <- data.frame()
    for (package in api_data_of_packages) {
        for (dependency in names(package$Depends)) {
            edgelist <-
                rbind(
                    edgelist,
                    data.frame(
                        this = package$Package,
                        needs_this = dependency,
                        type = "depends"
                    )
                )
        }
        for (import_dependency in names(package$Imports)) {
            edgelist <-
                rbind(
                    edgelist,
                    data.frame(
                        this = package$Package,
                        needs_this = import_dependency,
                        type = "imports"
                    )
                )
        }
        
        for (import_dependency in names(package$Suggests)) {
            edgelist <-
                rbind(
                    edgelist,
                    data.frame(
                        this = package$Package,
                        needs_this = import_dependency,
                        type = "suggests"
                    )
                )
        }
    }
    
    return(edgelist)
}



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




#####function get_download_statistics (adapted from 1999)####

# Takes a list of package names as strings
# Returns the download statistics of the given packages since 01.01.1999
get_download_statistics <- function(packages) {
    download_statistics <- data.frame()
    for (package in packages) {
        current_package_stats <- cran_downloads(
            package = package,
            from    = as.Date("1999-01-01"),
            to      = Sys.Date() - 1
        )
        download_statistics <-
            rbind(download_statistics, current_package_stats)
    }
    return(download_statistics)
}







####get all ctvs in a list#####
ctvs <- list()

for (n in names(packages_per_ctv))
    if (is.factor(packages_per_ctv[[n]])) {
        print(n)
        print(levels(packages_per_ctv[[n]]))
    }


####get just the packages in a dataframe#####
just_packages <- data.frame()

just_packages <- packages_per_ctv %>%
    select(package)

print(just_packages)


#packages aus packages_per_ctv
ctvs <- packages_per_ctv %>%
    distinct(ctv)

packages <- packages_per_ctv %>%
    distinct(package)



view(pppp)
