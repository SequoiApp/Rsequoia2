dg_org_id <- function(q = ""){
  q <- utils::URLencode(q)
  id_url <- sprintf("https://www.data.gouv.fr/api/1/organizations/?q=%s&page=1&badge=public-service", q)
  req <- curl::curl_fetch_memory(id_url)
  content <- jsonlite::fromJSON(rawToChar(req$content))
  id <- content$data$id
  return(id)
}

dg_dataset <- function(q = "", org_id = "", page_size = 10){

  base_url <-"https://www.data.gouv.fr/api/1/datasets/?organization=%s&q=%s&page_size=%s&page=%s"
  q <- q <- utils::URLencode(q, reserved = TRUE)

  # --- First call ---
  url1 <- sprintf(base_url, org_id, q, page_size, 1)
  req1 <- curl::curl_fetch_memory(url1)
  content1 <- jsonlite::fromJSON(rawToChar(req1$content), simplifyVector = FALSE)

  total <- content1$total
  total_pages <- ceiling(total / page_size)

  if (total_pages > 50){
    cli::cli_abort("Too many pages (>50). Narrow your query.")
  }

  all <- content1$data  # store first page immediately

  pb <- cli::cli_progress_bar(
    "Fetching dataset",
    type = "iterator",
    total = total_pages - 1
  )

  # --- Fetch remaining pages ---
  if (total_pages > 1) {
    for (page in 2:total_pages) {
      url <- sprintf(base_url, org_id, q, page_size, page)
      req <- curl::curl_fetch_memory(url)
      content <- jsonlite::fromJSON(rawToChar(req$content), simplifyVector = F)
      all <- c(all, content$data)
      cli::cli_progress_update()
    }
  }

  if (total_pages > 1){
    cli::cli_progress_done()
  }

  dataset_list <- lapply(all, function(ds){
    list(
      ds_id = ds$id,
      ds_title = ds$title,
      ds_description = ds$description
    )
  })
  dataset <- do.call(rbind, dataset_list) |> as.data.frame()

  ressource_list <- lapply(all, function(ds){
    lapply(ds$resources, function(r){
      list(
        ds_id = ds$id,
        r_id = r$id,
        r_title = r$title,
        r_description = r$description,
        r_format = r$format,
        r_url = r$url
      )
    })
  })

  resource <- do.call(rbind, unlist(ressource_list, recursive = FALSE)) |> as.data.frame()

  return(list(dataset = dataset, resource = resource))
}

dg_get_dataset <- function(id) {

  url <- sprintf("https://www.data.gouv.fr/api/1/datasets/%s/", id)
  req <- curl::curl_fetch_memory(url)
  content <- jsonlite::fromJSON(rawToChar(req$content), simplifyVector = FALSE)

  # Dataset-level metadata
  dataset <- data.frame(
    ds_id = content$id,
    title = content$title,
    description = content$description,
    tags = paste(unlist(content$tags), collapse = ", "),
    created_at = content$created_at,
    last_modified = content$last_modified,
    stringsAsFactors = FALSE
  )

  # Resource-level metadata
  resources <- do.call(
    rbind,
    lapply(content$resources, function(r) {
      list(
        ds_id = id,
        r_id = r$id,
        title = r$title,
        description = r$description,
        format = r$format,
        filesize = r$filesize,
        url = r$url
      )
    })
  ) |> as.data.frame()

  return(list(
    dataset = dataset,
    resources = resources
  ))
}
