test_that("add_duration", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_duration <- add_duration(wt, cutoff = 300, replace_by = NA)
    # test that variables in result
    expect_true("duration" %in% names(wt_duration))
    # test that duration variable is not NA
    expect_true(min(wt_duration$duration, na.rm = TRUE) >= 0)
    # test that no duration > cutoff
    expect_true(max(wt_duration$duration, na.rm = TRUE) < 300)
    # test that last row for panelist is NA for default
    expect_true(is.na(tail(wt_duration$duration[wt_duration$panelist_id == "AiDS4k1rQZ"], 1)))
    # test that last row for panelist is not NA
    wt_duration <- add_duration(wt, last_replace_by = 0)
    expect_true(tail(wt_duration$duration[wt_duration$panelist_id == "AiDS4k1rQZ"], 1) == 0)
    # test device_switch_na
    wt_duration <- add_duration(wt, device_switch_na = TRUE, device_var = "device")
    wt_duration[, device_next := shift(device, n = 1, type = "lead", fill = NA), by = "panelist_id"]
    expect_true(is.na(wt_duration[device_next != device][["duration"]][1]))
})

test_that("add_duration testdt_specific", {
    options(digits = 22)
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_duration <- add_duration(wt)
    # test duration for first row
    expect_true(as.numeric(wt_duration[1, "duration"]) == 2.8580000400543212890625)
    # test total duration
    expect_true(sum(wt_duration[, "duration"], na.rm = T) == 1177364.354005098342896)
})

test_that("add_duration errors", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    expect_error(add_duration(wt, replace_by = -1))
    expect_error(add_duration(wt, device_switch_na = T, device_var = NULL))
    expect_error(add_duration(wt, device_switch_na = T, device_var = "not_a_variable"))
})

test_that("add_session", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_session <- add_session(wt, cutoff = 1800)
    # test that variables in result
    expect_true("session" %in% names(wt_session))
    # test that session variable always positive
    expect_true(min(wt_session$session, na.rm = T) >= 1)
    # test that next session is only smaller than session when switch to new panelist
    wt_session[, next_session := shift(session, n = 1, type = "lead", fill = NA), by = "panelist_id"]
    expect_true(nrow(wt_session[session > next_session]) <=
        length(unique(wt_session$panelist_id)))
})

test_that("add_session errors", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    # no cutoff specified
    expect_error(add_session(wt))
})

test_that("add_session testdt_specific", {
    options(digits = 22)
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_session <- add_session(wt, cutoff = 1800)
    expect_true(wt_session[panelist_id == "AiDS4k1rQZ"][.N, "session"] == 123)
})

test_that("deduplicate", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt <- add_duration(wt, cutoff = 300, replace_by = 300)
    # test that variables in result
    wt_dedup <- deduplicate(wt, method = "flag")
    expect_true("duplicate" %in% names(wt_dedup))
    wt_dedup <- deduplicate(wt, method = "drop")
    expect_true(!"duplicate" %in% names(wt_dedup))
    wt_dedup <- deduplicate(wt, method = "aggregate", keep_nvisits = TRUE)
    expect_true("visits" %in% names(wt_dedup))
    wt <- extract_domain(wt)
    wt_dedup <- deduplicate(wt, method = "aggregate", add_grpvars = "domain")
    expect_true("domain" %in% names(wt_dedup))
    wt <- extract_host(wt)
    wt_dedup <- deduplicate(wt, method = "aggregate", add_grpvars = c("domain", "host"))
    expect_true("domain" %in% names(wt_dedup))
})

test_that("deduplicate errors", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt <- add_duration(wt, cutoff = 300, replace_by = 300)
    expect_error(deduplicate(wt, method = "aggregate", duration_var = "not_a_variable"))
    expect_error(deduplicate(wt, method = "flag", within = NULL))
    expect_error(deduplicate(wt, method = "drop", within = NULL))
})

test_that("deduplicate testdt_specific", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt <- add_duration(wt, cutoff = 300, replace_by = 300)
    wt_dedup <- deduplicate(wt, method = "drop")
    expect_true(nrow(wt_dedup) == 46574)
    wt_dedup <- deduplicate(wt, method = "flag")
    expect_true(sum(wt_dedup[, "duplicate"]) == 3038)
    wt_dedup <- deduplicate(wt, method = "aggregate")
    expect_true(nrow(wt_dedup) == 39540)
    wt_dedup <- deduplicate(wt, method = "aggregate", keep_nvisits = T)
    expect_true(max(wt_dedup[, "visits"]) == 608)
})

test_that("extract_host", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_host <- suppressWarnings(extract_host(wt))
    expect_true("host" %in% names(wt_host))
    wt[, other_url := url]
    wt_host <- suppressWarnings(extract_host(wt, varname = "other_url"))
    expect_true("other_url_host" %in% names(wt_host))
})

test_that("extract_host errors", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    expect_error(extract_host(wt, varname = "not_a_variable"))
})

test_that("extract_host testdt_specific", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_host <- suppressWarnings(extract_host(wt, drop_na = TRUE))
    expect_true(wt_host[1, "host"] == "dkr1.ssisurveys.com")
    expect_true(nrow(wt_host) == 49451)
    wt_host <- suppressWarnings(extract_host(wt, drop_na = FALSE))
    expect_true(nrow(wt_host) == nrow(wt))
})

test_that("extract_domain", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    # test existence of new columns
    wt_domain <- suppressWarnings(extract_domain(wt))
    expect_true("domain" %in% names(wt_domain))
    wt[, other_url := url]
    wt_domain <- suppressWarnings(extract_domain(wt, varname = "other_url"))
    expect_true("other_url_domain" %in% names(wt_domain))
    wt_domain <- suppressWarnings(extract_domain(wt, drop_na = FALSE))
    # test domain composition
    wt_domain[, tmp_host := urltools::domain(gsub("@", "%40", url))]
    wt_domain[, tmp_suffix := urltools::suffix_extract(tmp_host)[["suffix"]]]
    wt_domain[, tmp_domain_name := urltools::suffix_extract(tmp_host)[["domain"]]]
    expect_true(is.na(wt_domain[is.na(tmp_suffix), "domain"][1]))
    expect_true(wt_domain[!is.na(tmp_suffix) & is.na(tmp_domain_name), "domain"] ==
        wt_domain[!is.na(tmp_suffix) & is.na(tmp_domain_name), "tmp_suffix"])
})

test_that("extract_domain errors", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    expect_error(extract_domain(wt, varname = "not_a_variable"))
})

test_that("extract_domain testdt_specific", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_domain <- suppressWarnings(extract_domain(wt, drop_na = TRUE))
    expect_true(wt_domain[1, "domain"] == "ssisurveys.com")
    expect_true(nrow(wt_domain) == 49451)
    wt_domain <- suppressWarnings(extract_domain(wt, drop_na = FALSE))
    expect_true(nrow(wt_domain) == nrow(wt))
})

test_that("extract_path", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_path <- extract_path(wt)
    expect_true("path" %in% names(wt_path))
    wt[, other_url := url]
    wt_host <- extract_path(wt, varname = "other_url")
    expect_true("other_url_path" %in% names(wt_host))
})

test_that("extract_path errors", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    expect_error(extract_path(wt, varname = "not_a_variable"))
})

test_that("extract_path testdt_specific", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_path <- extract_path(wt)
    expect_true(wt_path[1, "path"] == "tzktsxomta")
    expect_true(is.na(wt_path[url == "https://www.youtube.com/", "path"][1]))
})

test_that("parse_path", {
    skip_on_cran()
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_path <- parse_path(wt)
    expect_true("path_split" %in% names(wt_path))
    # test that all path_split values have letters
    expect_true(nrow(wt_path[, test := grepl("[A-Za-z]", path_split, perl = T)]) ==
        nrow(wt_path[!is.na(path_split)]))
    # test different name for URL variable
    wt[, url2 := url]
    wt_path2 <- parse_path(wt, varname = "url2")
    expect_true("url2_path_split" %in% names(wt_path2))
})

test_that("parse_path errors", {
    skip_on_cran()
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    expect_error(extract_path(wt, varname = "not_a_variable"))
})

test_that("parse_path testdt_specific", {
    skip_on_cran()
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_path <- parse_path(wt)
    expect_true(wt_path[4672, "path_split"] == "quartzy, instagram, influencers, are, out, slackers, in")
    expect_true(is.na(wt_path[url == "https://www.youtube.com/", "path_split"][1]))
})

test_that("drop_query", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    # test existence of new colums
    wt_noquery <- drop_query(wt)
    expect_true("url_noquery" %in% names(wt_noquery))
    wt[, other_url := url]
    wt_noquery <- drop_query(wt, varname = "other_url")
    expect_true("other_url_noquery" %in% names(wt_noquery))
    # test absence of queries / fragments
    wt_noquery <- drop_query(wt)
    expect_true(nrow(wt[data.table::like(url_noquery, "\\?|#")]) == 0)
})

test_that("drop_query errors", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    expect_error(drop_query(wt, varname = "not_a_variable"))
})

test_that("drop_query testdt_specific", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_noquery <- drop_query(wt)
    expect_true(wt_noquery[1, "url_noquery"] == "https://dkr1.ssisurveys.com/tzktsxomta")
    wt_queries <- wt[data.table::like(url, "\\?")]
    wt_noquery <- drop_query(wt_queries)
    expect_true(wt_noquery[1, "url_noquery"] == "https://www.marketwatch.com/story/kelloggs-owned-veggie-burger-brand-morningstar-farms-to-go-all-vegan-by-2021-2019-03-04")
})

test_that("add_next_visit add_previous_visit", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    # test existence of new colums
    wt_next <- add_next_visit(wt)
    expect_true("url_next" %in% names(wt_next))
    wt_next <- add_next_visit(wt, level = "host")
    expect_true("host_next" %in% names(wt_next))
    wt_next <- add_next_visit(wt, level = "domain")
    expect_true("domain_next" %in% names(wt_next))
    wt_prev <- add_previous_visit(wt)
    expect_true("url_previous" %in% names(wt_prev))
    wt_prev <- add_previous_visit(wt, level = "host")
    expect_true("host_previous" %in% names(wt_prev))
    wt_prev <- add_previous_visit(wt, level = "domain")
    expect_true("domain_previous" %in% names(wt_prev))
    # test identity of second visit and first next visit
    wt_next <- add_next_visit(wt)
    expect_true(wt_next[2, "url"] == wt_next[1, "url_next"])
    # test identity of first visit and second previous visit
    wt_prev <- add_previous_visit(wt)
    expect_true(wt_prev[1, "url"] == wt_prev[2, "url_previous"])
    # test first and last row
    expect_true(is.na(wt_next[.N, "url_next"]))
    expect_true(is.na(wt_prev[1, "url_previous"]))
})

test_that("add_next_visit add_previous_visit testdt_specific", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_next <- add_next_visit(wt)
    expect_true(wt_next[1, "url_next"] == "https://roirocket.decipherinc.com/hivvocmeox")
    wt_prev <- add_previous_visit(wt)
    expect_true(wt_prev[2, "url_previous"] == "https://dkr1.ssisurveys.com/tzktsxomta")
})

test_that("add_title", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking[1])
    wt_title <- add_title(wt)
    expect_true("title" %in% names(wt_title))
})

test_that("add_title testdt_specific", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking[1])
    wt_title <- add_title(wt)
    expect_true(is.na(wt_title[, "title"]))
})

test_that("add_referral", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_ref <- add_referral(wt, platform_domains = "facebook.com", patterns = "fbclid=")
    # test existence of columns
    expect_true("referral" %in% names(wt_ref))
    expect_true(!"domain_previous" %in% names(wt_ref))
    # test value of new column
    expect_true(names(table(wt_ref$referral)) == "facebook.com")
})

test_that("add_referral errors", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    expect_error(add_referral(wt))
    expect_error(add_referral(wt, platform_domains = "facebook.com"))
    expect_error(add_referral(wt, platform_domains = c("facebook.com", "twitter.com"), pattern = "some"))
})

test_that("add_referral testdt_specific", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_ref <- add_referral(wt, platform_domains = "facebook.com", patterns = "fbclid=")
    expect_true(table(wt_ref$referral) == 57)
    expect_true(table(wt_ref$referral, exclude = NULL)[2] == 49555)
})

test_that("urldummy", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt <- suppressWarnings(extract_domain(wt))
    code_urls <- c("https://dkr1.ssisurveys.com/tzktsxomta")
    wt <- create_urldummy(wt, dummy = code_urls, name = "test_dummy")
    expect_true(wt$test_dummy[1])
})

test_that("panelist_data", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    data("testdt_survey_w")
    # test existence of columns
    wt_joined <- add_panelist_data(wt, testdt_survey_w)
    expect_true("leftright" %in% names(wt_joined))
    wt_joined <- add_panelist_data(wt, testdt_survey_w, cols = c("gender", "education"))
    expect_true(!("leftright" %in% names(wt_joined)))
    # text presence of data
    expect_true(sum(is.na(wt_joined$gender)) == 0)
})

test_that("panelist_data errors", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    data("testdt_survey_w")
    wt_joined <- add_panelist_data(wt, testdt_survey_w)
    expect_error(add_panelist_data(wt_joined, "not_a_variable"))
    expect_error(add_panelist_data(wt_joined, join_on = "not_a_variable"))
})

test_that("panelist_data testdt_specific", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    data("testdt_survey_w")
    wt_joined <- add_panelist_data(wt, testdt_survey_w)
    expect_true(round(mean(wt_joined$leftright), 2) == 4.99)
})
