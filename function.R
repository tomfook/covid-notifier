get_infections <- function(pref){
  source(paste0("scraper/", pref, ".R"), local = TRUE)
  infection <- scraper()

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

post_infection <- function(diff, pref, target, nmax = 20){
  pref_name <- switch(pref, kyoto = "京都府", osaka = "大阪府", okayama = "岡山県")
  icon <- switch(pref, kyoto = ":kyo:", osaka = ":han:", okayama = ":oka:")
  url_guide <- switch(pref,
		    kyoto = "http://www.pref.kyoto.jp/kentai/corona/hassei1-50.html",
		    osaka = "http://www.pref.osaka.lg.jp/hodo/index.php?site=fumin",
		    okayama = "https://www.pref.okayama.jp/page/667843.html"
		    ) 
  
  diff_n <- nrow(diff)
  if(diff_n == 0){
    stop("rows of diff seems to be zero.")
  }else if(diff_n < nmax){
    for(i in seq(to = nrow(diff))){
      text <- paste0(icon, " ", pref_name, "発表\n",
  		  "発表日: ", diff$発表日[i], ", ",
  		  "年代: ", diff$年代[i], ", ",
  		  "性別: ", diff$性別[i], ", ",
  		  "居住地: ", diff$居住地[i],
  		  "\n", url_guide
  		  ) 
    }
  }else{
    text <- paste0(icon, " ", pref_name, " 新規感染者多数", "\n")
    diff_cnt <- diff %>% group_by(発表日, 居住地) %>% summarise(n = n())
    dates <- sort(unique(diff_cnt$発表日))
    for(i in seq(along.with = dates)){
      text <- paste0(text, "発表日: ", dates[i], "\n")
      diff_by_loc <- filter(diff_cnt, 発表日 == dates[i]) 
      for(j in seq(to = nrow(diff_by_loc))){
        text <- paste0(text, "居住地 ", diff_by_loc[j,][["居住地"]], ": ", diff_by_loc[j,][["n"]], "人\n")
      } 
      text <- paste0(text, "\n")
    }
    text <- paste0(text, "計: ", diff_n, "人\n", url_guide)
    }

  if(TEST){
    print(paste0("TEST for ", target, ": ", text))
  }else{
    POST(url = target, encode = "json", body = list(text = text))
  }
}

notify_infection <- function(infections, target, location = NULL, nmax = 20){
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
      post_infection(diff, pref, target, nmax)
    }else{
      if(TEST){
        print(paste0("TEST for ", target, ": No infection in ", pref, location))
      }
    }
  }else{
    if(TEST){
      print(paste0("TEST for ", target, ": alert in ", pref))
    }else{
      POST(url = slack_webhookurl, encode = "json", body = list(text = paste0("ALERT: Something happened in ", pref)))
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

