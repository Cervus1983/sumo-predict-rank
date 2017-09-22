library(rvest)
library(tidyverse)

sumodbBanzukeSimpleQuery <- function(basho) {
  raw_html <- tryCatch(
    read_html(paste0("http://sumodb.sumogames.de/Banzuke.aspx?b=", basho, "&hl=on&c=on&simple=on")),
		error = function(e) {},
		warning = function(w) {}
  )

  table_banzuke <- tryCatch(
    raw_html %>% 
      html_node("table.banzuke") %>% 
      html_table(),
		error = function(e) {},
		warning = function(w) {}
  )
  
  ids <- tryCatch(
    raw_html %>% 
      html_node("table.banzuke") %>% 
      html_nodes("a") %>% 
      html_attr("href") %>% 
      grep("^Rikishi\\.aspx\\?r=\\d+$", ., value = TRUE) %>% 
      sub("^Rikishi\\.aspx\\?r=", "", .) %>% 
      as.integer()
  )

	if (nrow(table_banzuke) == length(ids)) cbind(
	  id = ids,
	  table_banzuke %>% setNames(tolower(names(.)))
	)
}
