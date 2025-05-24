box::use(
  logger[...],
  paws[...],
)

log_group <- Sys.getenv("FD") |>
  stringr::str_replace_all("[^[:alnum:]]", "-")
log_stream <- "incident_response"

# Define a custom appender that logs to CloudWatch Logs
appender_cloudwatch <- function(log_group, log_stream) {
  
  force(log_stream)
  force(log_group)

  # 1. Initialize CloudWatch Logs client
  cwlogs <- cloudwatchlogs()
  
  # 2. Ensure log stream exists
  streams_info <- cwlogs$describe_log_streams(
    logGroupName = log_group,
    logStreamNamePrefix = log_stream
  )$logStreams
  
  # Check if the log stream already exists
  stream_names <- vapply(streams_info, function(x) x$logStreamName, character(1))
  
  if (!(log_stream %in% stream_names)) {
    cwlogs$create_log_stream(
      logGroupName  = log_group,
      logStreamName = log_stream
    )
  }
  
  # 3. Get the current uploadSequenceToken (may not exist for brand-new streams)
  streams_info <- cwlogs$describe_log_streams(
    logGroupName = log_group,
    logStreamNamePrefix = log_stream
  )$logStreams
  
  # Pull the correct stream object (in case multiple returned)
  stream_entry <- Filter(function(x) x$logStreamName == log_stream, streams_info)[[1]]
  
  # Optional chaining if no token yet (newly created stream)
  seq_token <- stream_entry$uploadSequenceToken %||% NULL

  # 4. Return the function that 'logger' will call for each log
  structure(
    function(msg, level, namespace = NA_character_,
             .logcall = NA, .topcall = NA, .topenv = NA) {
      
      timestamp_ms <- as.numeric(Sys.time()) * 1000
      
      if (!is.character(msg)) {
        msg <- as.character(msg)
      }
      
      result <- cwlogs$put_log_events(
        logGroupName  = log_group,
        logStreamName = log_stream,
        logEvents     = list(
          list(
            timestamp = timestamp_ms,
            message   = msg
          )
        ),
        sequenceToken = seq_token
      )
      
      seq_token <<- result$nextSequenceToken
    },
    class = "appender"
  )
  
}

# --- USAGE EXAMPLE ---

log_layout(
  layout_glue_generator(format = "[{level}] [{format(time, '%Y-%m-%d %H:%M:%S')}] [{namespace}] {msg}")
)


# 1. Configure the logger to use our custom CloudWatch appender and console appender
if (TRUE) { #FIXME FOr some reason, the log appender isn't working.
  log_appender(appender_stdout)
} else {
log_appender(
  appender_cloudwatch(log_group, log_stream)
)
  print(class(appender_cloudwatch(log_group, log_stream)))
  
}

  
# 2. (Optional) set the logging threshold (DEBUG, INFO, WARN, ERROR, FATAL)
log_threshold(TRACE)

# 3. Generate some log messages
log_info("Connection to AWS from {log_group} {log_stream}", namespace = 'logging')
