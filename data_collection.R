#contains functions for various data collection (e.g. download statistics, dependencies)

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


####funtion get_api_data (base for dependencies etc.)#####
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



#####function get_download_statistics (adapted from 10/2012)####

# Takes a list of package names as strings
# Returns the download statistics of the given packages since 01.10.2012
get_download_statistics <- function(packages) {
    download_statistics <- data.frame()
    for (package in packages) {
        current_package_stats <- cran_downloads(
            package = package,
            from    = as.Date("2012-10-01"),
            to      = Sys.Date() - 1
        )
        download_statistics <-
            rbind(download_statistics, current_package_stats)
    }
    return(download_statistics)
}







####get just the ctvs + packages in a separate dataframe#####
#packages aus packages_per_ctv
ctvs <- packages_per_ctv %>%
    distinct(ctv)

packages <- packages_per_ctv %>%
    distinct(package)


