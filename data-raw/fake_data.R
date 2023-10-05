n <- 500
user_ids <- sprintf("user%d", sample(1:5, n, replace = TRUE))
urls <- c(
    "https://example.com/page1",
    "https://example.com/page2",
    "https://example.com/page3",
    "https://example.com/page4",
    "https://example.com/page5",
    "https://example.net/page1",
    "https://example.net/page2",
    "https://example.org/page1",
    "https://example.org/page2",
    "https://example.org/page3"
)
visited_urls <- sample(urls, n, replace = TRUE)

generate_timestamp <- function() {
    current_time <- as.POSIXct(Sys.Date())
    random_days <- as.difftime(sample(0:0, 1), units = "days")
    random_hours <- as.difftime(sample(2:4, 1), units = "hours")
    random_minutes <- as.difftime(sample(0:59, 1), units = "mins")

    random_time <- current_time - random_days - random_hours - random_minutes
    return(as.character(random_time))
}
timestamps <- replicate(n, generate_timestamp())
fake_tracking <- data.frame(panelist_id = user_ids, url = visited_urls, timestamp = timestamps)
fake_tracking <- dplyr::arrange(fake_tracking, panelist_id, timestamp)[sample(1:n), ]
usethis::use_data(fake_tracking, overwrite = TRUE)
