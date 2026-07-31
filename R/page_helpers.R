slugify_page <- function(page_name) {
  
  slug <- iconv(
    page_name,
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  slug <- tolower(slug)
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("^-+|-+$", "", slug)
  
  if (!nzchar(slug)) {
    stop("The page name must contain at least one letter or number.")
  }
  
  paste0(slug, ".html")
}


page_js_filename <- function(href) {
  paste0(
    tools::file_path_sans_ext(basename(href)),
    ".js"
  )
}


set_page_html_module <- function(html_path, js_filename) {
  
  replace_file_text(
    file_path = html_path,
    pattern = paste0(
      '<script\\s+type\\s*=\\s*["\']module["\']\\s+',
      'src\\s*=\\s*["\']src/[^"\']+\\.js["\']\\s*>',
      '\\s*</script>'
    ),
    replacement = paste0(
      '<script type = "module" src="src/',
      js_filename,
      '"></script>'
    )
  )
}


set_page_js_title <- function(js_path, page_name) {
  
  page_name_js <- jsonlite::toJSON(
    page_name,
    auto_unbox = TRUE
  )
  
  replace_file_text(
    file_path = js_path,
    pattern = 'await\\s+insertHead\\s*\\([^;]*\\)\\s*;',
    replacement = paste0(
      "await insertHead(",
      page_name_js,
      ");"
    )
  )
}