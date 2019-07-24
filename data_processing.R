#####filter global data#####
#alles was hier drin steht, ist der default
filter_timeseries_data <-
  function (selected_from = as.Date("2012-10-01"),
            selected_to = Sys.Date() - 1,
            selected_packages = list(),
            selected_min_count = 0,
            selected_ctv = list(),
            level = "ctv") {
    if (level == "package") {
      filtered_data <- tutti_timeseries %>%
        filter (package == selected_packages) %>%
        filter (date >= selected_from, date <= selected_to) %>%
        filter (count >= selected_min_count) %>%
        select (date, count, package)
    }
    else{
      filtered_data <- tutti_timeseries %>%
        filter (ctv == selected_ctv) %>%
        filter (date >= selected_from, date <= selected_to) %>%
        filter (count >= selected_min_count) %>%
        select (date, count, ctv)
    }
    
    return (filtered_data)
  }




####aggregation (time)#####
aggregate_timeseries_data <-
  function (timelevel = "monthly", filtered_data) {
    download_monthly <- filtered_data %>%
      mutate(month = as.Date(cut(date, breaks = "month"))) %>%
      group_by_at(vars(-c(date, count))) %>% # group by everything but date, day, count
      summarise(total = sum(count))
  }
