get_infections <- function(pref){
  source(paste0("scraper/", pref, ".R"), local = TRUE)
  infection <- scraper()
  if(!all(c("index", "発表日", "年代", "性別", "居住地") %in% names(infection))){
    stop("ERROR: infection does not have required colmn!")
  }

  file_latest <- paste0("infections_", pref, ".csv")
  file_latest_path <- paste0("data/", file_latest) 
  if (any(dir("data") %in% file_latest)){
    old_infection <- read_csv(file_latest_path, col_types = cols(.default = "c"))
  }else{
    old_infection <- infection
  }

  out <- list(latest = infection, old = old_infection, pref = pref)
  return(out)
}

post_infection <- function(diff, pref, target, target_name = NULL, nmax = 20){
  pref_name <- switch(pref, kyoto = "京都府", osaka = "大阪府", okayama = "岡山県", kagawa = "香川県")
  icon <- switch(pref, kyoto = ":kyo:", osaka = ":han:", okayama = ":oka:")
  url_guide <- switch(pref,
		    kyoto = "http://www.pref.kyoto.jp/kentai/corona/hassei1-50.html",
		    osaka = "http://www.pref.osaka.lg.jp/hodo/index.php?site=fumin",
		    okayama = "https://www.pref.okayama.jp/page/667843.html",
		    kagawa = "https://www.pref.kagawa.lg.jp/content/etc/subsite/kansenshoujouhou/kansen/se9si9200517102553.shtml"
		    ) 
  
  diff_n <- nrow(diff)
  if(diff_n == 0){
    stop("ERROR: rows of diff seems to be zero.")
  }else if(diff_n < nmax){
    for(i in seq(to = nrow(diff))){
      text <- paste0(icon, " ", pref_name, "発表\n",
  		  "発表日: ", diff$発表日[i], ", ",
  		  "年代: ", diff$年代[i], ", ",
  		  "性別: ", diff$性別[i], ", ",
  		  "居住地: ", diff$居住地[i],
  		  "\n", url_guide
  		  ) 
      if(TEST){
        print(paste0("TEST for ", target_name, ": ", text))
      }else{
        invoke(POST, target(text))
      }
    }
  }else{
    text <- paste0(icon, " ", pref_name, " 新規感染者多数", "\n")
    diff_cnt <- diff %>% group_by(発表日, 居住地) %>% summarise(n = n())
    dates <- sort(unique(diff_cnt$発表日))
    for(i in seq(along.with = dates)){
      text <- paste0(text, "発表日: ", dates[i], "\n")
      diff_by_loc <- filter(diff_cnt, 発表日 == dates[i]) 
      for(j in seq(to = nrow(diff_by_loc))){
        text <- paste0(text, " 居住地 ", diff_by_loc$居住地[j], ": ", diff_by_loc$n[j], "人\n")
      } 
      text <- paste0(text, " ", dates[i], "計:", sum(diff_by_loc$n), "人\n")
      text <- paste0(text, "\n")
    }
    text <- paste0(text, "計: ", diff_n, "人\n", url_guide)
    if(TEST){
      print(paste0("TEST for ", target_name, ": ", text))
    }else{
      invoke(POST, target(text))
    }
  }

}

notify_infection <- function(infections, target, target_name = NULL, location = NULL, nmax = 20){
  latest <- infections$latest
  old <- infections$old
  pref <- infections$pref 

  diff <- anti_join(latest, old, by = "index") 
  if(!is.null(location)){
    diff <- filter(diff, 居住地 == location)
  }
  
  growth <- nrow(diff)
  
  check_health <- growth >= 0
  if(check_health){
    if(growth > 0){ 
      post_infection(diff, pref, target, target_name, nmax)
    }else{
      if(TEST){
        print(paste0("TEST for ", target_name, ": No infection in ", pref, location))
      }
    }
  }else{
    if(TEST){
      print(paste0("TEST for ", target_name, ": alert in ", pref))
    }else{
      invoke(POST, target(paste0("ALERT: Something happened in ", pref)))
    }
  } 
}

update_record <- function(infections){
  latest <- infections$latest
  old <- infections$old
  pref <- infections$pref

  file_latest <- paste0("infections_", pref, ".csv")
  file_latest_path <- paste0("data/", file_latest)

  growth <- nrow(latest) - nrow(old) 

  check_health <- growth >= 0
  if(check_health) write_csv(latest, file_latest_path, na = "")
}

zentohan <- function(text){
  out <- text
  zennum <- "０１２３４５６７８９"
  for(i in 0:9){
    out <- gsub(substr(zennum, i+1, i+1), i, out)
  }
  return(out)
}


post_slack_message <- function(url){
  function(text){
    list(url = url,
	 encode = "json",
	 body = list(text = text)
    )
  }
}

post_linenotify_message <- function(token){
  function(text){
    list(url = "https://notify-api.line.me/api/notify",
         encode = "form",
         config = add_headers(Authorization = paste0("Bearer ", token)),
         body = list(message = text)
         )
  }
}
