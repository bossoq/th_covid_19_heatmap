## load library httr,jsonlite,tidyverse,sf
library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
setwd("/workspaces/Covid") ## set your working dir

## query api
api_key <- "please input your api key"
url <- paste0("https://opend.data.go.th/get-ckan/datastore_search?",
              "resource_id=329f684b-994d-476b-91a4-62b2ea00f29f")
http_response <- GET(url, add_headers("api-key" = api_key), accept_json())
total_data <- fromJSON(content(http_response, "text"))$result$total

offset_record <- 0 ## set start
while (offset_record <= max(total_data, 0)) {
    url_new <- paste0(url, "&offset=", offset_record)
    http_response <- GET(url_new,
                         add_headers("api-key" = api_key),
                         accept_json())
    temp_data <- fromJSON(content(http_response, "text"))$result$records
    if (offset_record > 0) {
        opencases <- rbind(opencases, temp_data)
    } else {
        opencases <- temp_data
    }
    offset_record <- offset_record + 100
}

opencases <- as_tibble(opencases)

## Clean missing values
## missing province
opencases$province_of_onset <- ifelse(opencases$province_of_onset == "",
                                      opencases$province_of_isolation,
                                      opencases$province_of_onset)

## กทม. to กทม
opencases$province_of_onset <- ifelse(opencases$province_of_onset == "กทม." |
                                      opencases$province_of_onset == "กรุงเทพ" |
                                      opencases$province_of_onset == "กรุงเทพมหานคร",
                                      "กทม",
                                      opencases$province_of_onset)

## sort opencases
opencases <- opencases[order(opencases["No."]), ]

## convert to date
opencases$announce_date <- as.Date(opencases$announce_date) ## convert date

## filter out quarantine and ASQ
opencases_local <- filter(opencases,
                          !grepl("State Quarantine",
                                 risk) &
                          !grepl("ASQ",
                                 risk) &
                          !grepl("HQ",
                                 risk))

## create subset
start_date <- as.Date("2020-12-18") ## first date of second wave
end_date <- Sys.Date()

second_wave <- opencases_local[opencases_local$announce_date >= start_date &
                               opencases_local$announce_date <= end_date, ]
second_wave <- second_wave[, -c(1, 4, 8)]


## number of secondWave by date
the_date <- start_date
temp_data <- second_wave[, c(2, 6)]
temp_prov <- NULL
temp_name <- NULL
while (the_date <= end_date) {
    name <- paste("data", format(the_date, "%Y%m%d"), sep = "")
    temp <- temp_data[temp_data$announce_date == the_date, ]
    if (dim(temp)[1] > 0) {
        temp_name <- c(temp_name, name)
        assign(name, setNames(aggregate(temp,
                                        by = list(temp$province_of_onset),
                                        length)[, 1:2],
                                    c("province",
                                    paste0("d", format(the_date, "%Y%m%d")))))
        temp_prov <- c(temp_prov, get(name)$province)
    }
    the_date <- the_date + 1
}

## create new dataframe
time_series <- read.csv("province_map.csv", sep = ",")
for (i in temp_name) {
    time_series <- merge(x = time_series,
                         y = get(i), by = "province", all.x = T)
}
time_series <- time_series %>% replace(is.na(.), 0)
time_series <- time_series[, -1]
time_series[, -1] <- mutate_all(time_series[, -1],
                     function(x) as.numeric(as.character(x)))
names(time_series)[1] <- "NAME_1"

## create accumulated time series
from <- 2
num <- dim(time_series)[2] - 1
time_series_acc <- time_series
for (i in from:num) {
    time_series_acc[, i + 1] <- time_series_acc[, i] + time_series_acc[, i + 1]
    if (i == 2) {
        names(time_series_acc)[i] <- paste0("acc",
                                     substr(names(time_series_acc[i]), 2, 99))
    }
    names(time_series_acc)[i + 1] <- paste0("acc",
                            substr(names(time_series_acc[i + 1]), 2, 99))
}

## convert 0 to na
time_series[time_series == 0] <- NA
time_series_acc[time_series_acc == 0] <- NA
day_na <- time_series %>% replace(is.na(.), 0)
acc_na <- time_series_acc %>% replace(is.na(.), 0)
acc_range <- range(acc_na[, -1])

## Load ShapeFiles
thailand <- st_read("gadm36_THA_1.shp")

## merge timeseries with shape
time_series_plot <- left_join(thailand, time_series, by = "NAME_1")
time_series_acc_plot <- left_join(thailand, time_series_acc, by = "NAME_1")

## create plot
cases_plot <- list()
for (i in temp_name) {
    temp_date <- format(as.Date(substr(i, 5, 99),
                        format = "%Y%m%d"),
                        "%Y%m%d")
    temp_date2 <- format(as.Date(substr(i, 5, 99),
                        format = "%Y%m%d"),
                        "%d-%m-%Y")
    temp_aes <- paste0("d", temp_date)
    cases_plot[[temp_aes]] <- ggplot(time_series_plot) +
            ggtitle(paste0("Covid Open Cases on ", temp_date2,
            "\n total daily cases: ",
            sum(day_na[temp_aes]), " cases")) +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_sf(aes_string(fill = temp_aes)) +
            scale_fill_gradient(high = "#ff5b5b",
                low = "#cbfc90",
                na.value = "white") +
            labs(fill = "no. of cases")
}

cases_plot_acc <- list()
for (i in temp_name) {
    temp_date <- format(as.Date(substr(i, 5, 99),
                        format = "%Y%m%d"),
                        "%Y%m%d")
    temp_date2 <- format(as.Date(substr(i, 5, 99),
                        format = "%Y%m%d"),
                        "%d-%m-%Y")
    startdate <- format(start_date, "%d-%m-%Y")
    temp_aes <- paste0("acc", temp_date)
    cases_plot_acc[[temp_aes]] <- ggplot(time_series_acc_plot) +
            ggtitle(paste0("Accumulated Covid Open Cases ",
                startdate, " to ", temp_date2,
                "\n total accumulated cases: ",
                sum(acc_na[temp_aes]), " cases")) +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_sf(aes_string(fill = temp_aes)) +
            scale_fill_gradient2(high = "#ff5b5b",
                mid = "#f4f764",
                low = "#cbfc90",
                na.value = "white",
                midpoint = 50,
                limits = c(floor(acc_range[1]), ceiling(acc_range[2]))) +
            labs(fill = "no. of cases")
}

## save plot function
save_plot <- function(var, path) {
    for (i in names(get(var))) {
        if (var == "cases_plot") {
            fname <- substr(i, 2, 99)
            filename <- paste0(fname, ".png")
        } else {
            fname <- substr(i, 4, 99)
            filename <- paste0(i, ".png")
        }
        ggsave(plot = get(var)[[i]],
            path = path,
            file = filename)
    }
}

save_plot("cases_plot", "plot")
save_plot("cases_plot_acc", "acc_plot")
