box::use(
  logger[...],
  paws[...],
)

log_group <- Sys.getenv("FD") |>
  stringr::str_replace_all("[^[:alnum:]]", "-")
log_stream <- "incident_response"

# Define a custom appender that logs to CloudWatch Logs
appender_cloudwatch <- function(log_group, log_stream) {

  # 1. Initialize CloudWatch Logs client
  cwlogs <- cloudwatchlogs()

  # 2. Attempt to create the log group and log stream (if they don't exist).
  #    Wrap in `try()` to ignore errors if already exists:
  try(cwlogs$create_log_group(logGroupName = log_group), silent = TRUE)
  try(cwlogs$create_log_stream(
    logGroupName  = log_group,
    logStreamName = log_stream
  ), silent = TRUE)

  # 3. Get the current sequence token (needed to append new events)
  streams_info <- cwlogs$describe_log_streams(
    logGroupName        = log_group,
    logStreamNamePrefix = log_stream
  )
  seq_token <- streams_info$logStreams[[1]]$uploadSequenceToken

  # 4. Return the function that 'logger' will call for each log
  #    Must use the exact signature: function(level, msg, namespace, .logcall, .topcall, .topenv)
  function(msg, level, namespace = NA_character_,
           .logcall = NA, .topcall = NA, .topenv = NA) {

    # CloudWatch expects timestamps as the number of milliseconds since epoch (type: 64-bit integer)
    timestamp_ms <- as.numeric(Sys.time()) * 1000  # numeric avoids 32-bit overflow

    # If msg is not a character, convert it (just in case)
    if (!is.character(msg)) {
      msg <- as.character(msg)
    }

    # Send the log event
    result <- cwlogs$put_log_events(
      logGroupName  = log_group,
      logStreamName = log_stream,
      logEvents     = list(
        list(
          timestamp = timestamp_ms,  # keep as numeric (paws will handle conversion)
          message   = msg
        )
      ),
      sequenceToken = seq_token
    )

    # Update the token for subsequent logs
    seq_token <<- result$nextSequenceToken
  }
}

# --- USAGE EXAMPLE ---

log_layout(
  layout_glue_generator(format = "[{level}] [{format(time, '%Y-%m-%d %H:%M:%S')}] [{namespace}] {msg}")
)


# 1. Configure the logger to use our custom CloudWatch appender and console appender
if (interactive()) {
  log_appender(appender_stdout)
} else {
log_appender(
  appender_cloudwatch(log_group, log_stream)
)
}

  
# 2. (Optional) set the logging threshold (DEBUG, INFO, WARN, ERROR, FATAL)
log_threshold(INFO)

# 3. Generate some log messages
log_info("Connection to AWS from {log_group} {log_stream}", namespace = 'logging')
