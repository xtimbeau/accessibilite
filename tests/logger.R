library(logger)
library(furrr)
library(progressr)
logfile <- "test.log"
file.create(logfile)
plan(multiprocess)

with_progress(
  {
  pb <- progressor(steps=50)
  future_map(1:50,
           ~{
             pb()
             log_appender(appender_file(logfile))
             s <- stringr::str_c("hello ", future:::session_uuid()[[1]])
             log_info(" ----- {s}")
             }
           )
  },
  handlers=handler_progress(format=":bar :percent :eta", width=80))
