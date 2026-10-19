blog_refresh_requested <- function(env_var = "BLOG_REFRESH_DATA") {
  value <- tolower(trimws(Sys.getenv(env_var, unset = "false")))
  false_values <- c("", "0", "false", "no", "off")
  true_values <- c("1", "true", "yes", "on")

  if (value %in% false_values) return(FALSE)
  if (value %in% true_values) return(TRUE)

  stop(
    sprintf(
      "%s must be one of: %s",
      env_var,
      paste(c(false_values[-1], true_values), collapse = ", ")
    ),
    call. = FALSE
  )
}

read_source_registry <- function(path) {
  required <- c(
    "source_id", "publisher", "dataset", "release_date", "coverage_end",
    "landing_url", "download_url", "filename", "minimum_bytes", "sha256"
  )

  registry <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  missing_columns <- setdiff(required, names(registry))

  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Source registry is missing required columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (anyDuplicated(registry$source_id)) {
    stop("Source registry contains duplicate source_id values.", call. = FALSE)
  }

  text_columns <- setdiff(required, "minimum_bytes")
  if (any(vapply(registry[text_columns], function(x) any(is.na(x) | !nzchar(trimws(x))), logical(1)))) {
    stop("Source registry contains blank required values.", call. = FALSE)
  }

  if (
    any(is.na(as.Date(registry$release_date))) ||
    any(is.na(as.Date(registry$coverage_end)))
  ) {
    stop("Source registry dates must use valid YYYY-MM-DD values.", call. = FALSE)
  }

  minimum_bytes <- suppressWarnings(as.numeric(registry$minimum_bytes))
  if (any(!is.finite(minimum_bytes) | minimum_bytes <= 0)) {
    stop("Source registry minimum_bytes values must be positive numbers.", call. = FALSE)
  }

  if (any(!grepl("^[0-9a-fA-F]{64}$", registry$sha256))) {
    stop("Source registry SHA-256 values must contain 64 hexadecimal characters.", call. = FALSE)
  }

  if (any(basename(registry$filename) != registry$filename)) {
    stop("Source registry filenames must not contain directory paths.", call. = FALSE)
  }

  registry
}

download_registered_source <- function(source, raw_dir, refresh = blog_refresh_requested()) {
  if (!is.data.frame(source) || nrow(source) != 1) {
    stop("source must be a one-row data frame from the source registry.", call. = FALSE)
  }

  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  destination <- file.path(raw_dir, source$filename[[1]])
  minimum_bytes <- as.numeric(source$minimum_bytes[[1]])
  expected_sha256 <- tolower(trimws(source$sha256[[1]]))
  downloaded <- refresh || !file.exists(destination)

  if (downloaded) {
    extension <- tools::file_ext(destination)
    temporary <- tempfile(
      pattern = paste0(source$source_id[[1]], "-"),
      tmpdir = raw_dir,
      fileext = if (nzchar(extension)) paste0(".", extension) else ""
    )
    on.exit(unlink(temporary), add = TRUE)

    downloaded_ok <- tryCatch(
      {
        utils::download.file(
          source$download_url[[1]],
          temporary,
          mode = "wb",
          quiet = TRUE
        )
        TRUE
      },
      error = function(error) FALSE
    )

    if (!downloaded_ok && nzchar(Sys.which("curl"))) {
      curl_status <- suppressWarnings(
        system2(
          "curl",
          c(
            "-L", "--fail", "--retry", "3",
            "--connect-timeout", "20", "--max-time", "300",
            "-o", temporary,
            source$download_url[[1]]
          ),
          stdout = FALSE,
          stderr = FALSE
        )
      )
      downloaded_ok <- identical(curl_status, 0L)
    }

    if (!downloaded_ok) {
      stop(
        sprintf("Could not download registered source %s.", source$source_id[[1]]),
        call. = FALSE
      )
    }

    downloaded_bytes <- file.info(temporary)$size
    if (is.na(downloaded_bytes) || downloaded_bytes < minimum_bytes) {
      stop(
        sprintf(
          "Downloaded source %s is smaller than expected (%s bytes).",
          source$source_id[[1]],
          downloaded_bytes
        ),
        call. = FALSE
      )
    }

    downloaded_sha256 <- digest::digest(temporary, algo = "sha256", file = TRUE)
    if (!identical(downloaded_sha256, expected_sha256)) {
      stop(
        sprintf(
          "Checksum mismatch for downloaded source %s. Expected %s but found %s.",
          source$source_id[[1]],
          expected_sha256,
          downloaded_sha256
        ),
        call. = FALSE
      )
    }

    if (!file.copy(temporary, destination, overwrite = TRUE)) {
      stop(sprintf("Could not stage downloaded source at %s.", destination), call. = FALSE)
    }
  }

  file_bytes <- file.info(destination)$size
  if (is.na(file_bytes) || file_bytes < minimum_bytes) {
    stop(
      sprintf(
        "Cached source %s is smaller than expected (%s bytes).",
        source$source_id[[1]],
        file_bytes
      ),
      call. = FALSE
    )
  }

  file_sha256 <- digest::digest(destination, algo = "sha256", file = TRUE)
  if (!identical(file_sha256, expected_sha256)) {
    stop(
      sprintf(
        "Checksum mismatch for %s. Expected %s but found %s.",
        source$source_id[[1]],
        expected_sha256,
        file_sha256
      ),
      call. = FALSE
    )
  }

  list(
    path = destination,
    manifest = data.frame(
      source_id = source$source_id[[1]],
      publisher = source$publisher[[1]],
      dataset = source$dataset[[1]],
      release_date = source$release_date[[1]],
      coverage_end = source$coverage_end[[1]],
      landing_url = source$landing_url[[1]],
      download_url = source$download_url[[1]],
      local_file = source$filename[[1]],
      cached_file_mtime = format(file.info(destination)$mtime, tz = "UTC", usetz = TRUE),
      bytes = file_bytes,
      sha256 = file_sha256,
      downloaded_this_run = downloaded,
      stringsAsFactors = FALSE
    )
  )
}

write_source_manifest <- function(manifests, path) {
  manifest <- do.call(rbind, lapply(manifests, function(x) x$manifest))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(manifest, path, row.names = FALSE, na = "")
  invisible(manifest)
}
