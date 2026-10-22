# Generate the four latest news cards for the homepage.
# Run from the website root with: Rscript scripts/update-home-news.R

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("The R package 'yaml' is required. Install it with install.packages('yaml').")
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || is.na(x[1])) y else x[1]

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
news_dir <- file.path(root, "news", "posts")
output_file <- file.path(root, "_generated", "home-news.html")
count <- 4L

if (!dir.exists(news_dir)) {
  stop("News directory not found: ", news_dir)
}

files <- list.files(news_dir, pattern = "\\.qmd$", full.names = TRUE)
posts <- list()

for (file in files) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  if (length(lines) < 3L || trimws(lines[1]) != "---") next

  end <- which(trimws(lines[-1]) == "---")[1] + 1L
  if (is.na(end) || end < 3L) next

  front_matter <- paste(lines[2:(end - 1L)], collapse = "\n")
  metadata <- tryCatch(
    yaml::yaml.load(front_matter),
    error = function(e) {
      message("Skipping ", basename(file), ": invalid YAML: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(metadata)) next

  raw_date <- metadata$date
  if (is.null(raw_date) || !nzchar(as.character(raw_date))) next
  post_date <- tryCatch(as.Date(as.character(raw_date)), error = function(e) NA)
  if (is.na(post_date)) {
    message("Skipping ", basename(file), ": date should be YYYY-MM-DD")
    next
  }

  title <- as.character(metadata$title %||% tools::file_path_sans_ext(basename(file)))
  title <- gsub("\\*\\*", "", title)
  title <- gsub("[*_`]", "", title)

  description <- as.character(metadata$description %||% "")
  description <- trimws(gsub("\\s+", " ", gsub("[*_`]", "", description)))
  if (!nzchar(description) || description == " ") {
    body <- paste(lines[(end + 1L):length(lines)], collapse = " ")
    description <- trimws(gsub("\\s+", " ", gsub("!?(\\[[^]]*\\])\\([^)]*\\)|\\[[^]]*\\]\\([^)]*\\)|[*_`#]", "", body)))
  }
  if (nchar(description) > 220) {
    description <- paste0(substr(description, 1, 217), "…")
  }

  image <- as.character(metadata$image %||% "")
  image <- trimws(image)
  if (nzchar(image) && !grepl("^(https?://|/)", image)) {
    root_image <- file.path(root, image)
    if (file.exists(root_image)) {
      image <- gsub("\\\\", "/", image)
    } else {
      image <- gsub("\\\\", "/", file.path(dirname(sub(paste0("^", root, "/?"), "", file)), image))
    }
  }

  rel <- substring(normalizePath(file, winslash = "/", mustWork = TRUE), nchar(root) + 2L)
  url <- paste0("/", sub("\\.qmd$", ".html", gsub("\\\\", "/", rel)))
  posts[[length(posts) + 1L]] <- list(
    date = post_date,
    date_label = format(post_date, "%B %d, %Y"),
    title = title,
    description = description,
    image = image,
    url = url
  )
}

if (length(posts)) {
  order_idx <- order(vapply(posts, function(x) as.numeric(x$date), numeric(1)), decreasing = TRUE)
  posts <- posts[order_idx][seq_len(min(count, length(order_idx)))]
}

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}


# Generate a simple date-and-headline list
cards <- vapply(posts, function(item) {
  date_label <- format(item$date, "%b %d, %Y")
  
  paste0(
    '<div class="home-news-item">',
    '<span class="home-news-date">',
    escape_html(date_label),
    '</span>',
    '<a class="home-news-title" href="',
    escape_html(item$url),
    '">',
    escape_html(item$title),
    '</a>',
    '</div>'
  )
}, character(1))

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
writeLines(paste(cards, collapse = "\n"), output_file, useBytes = TRUE)
message("Generated ", length(posts), " homepage news cards: ", output_file)
