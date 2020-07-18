get_infections <- function(pref){
  source(paste0("scraper/", pref, ".R"), local = TRUE)
  infection <- scraper()
  out <- list(latest = infection, pref = pref)
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
  
  if(nrow(diff) < nmax){
    for(i in seq(to = nrow(diff))){
      text <- paste0(icon, " ", pref_name, "発表\n",
  		  "発表日: ", diff[i,][["発表日"]], ", ",
  		  "年代: ", diff[i,][["年代"]], ", ",
  		  "性別: ", diff[i,][["性別"]], ", ",
  		  "居住地: ", diff[i,][["居住地"]],
  		  "\n", url_guide
  		  ) 
      if(TEST){
        print(paste0("TEST for ", target, ": ", text))
      }else{
        POST(url = target, encode = "json", body = list(text = text))
      }
    }
  }else{
    text <- paste0(icon, " ", pref_name, " 新規感染者多数", "\n")
    diff_n <- diff %>% group_by(発表日, 居住地) %>% summarise(n = n())
    dates <- sort(diff_n[["発表日"]]) 
    for(i in seq(along.with = dates)){
      text <- paste0(text, "発表日: ", dates[i], "\n")
      diff_by_loc <- filter(diff_n, 発表日 == dates[i]) 
      for(i in seq(to = nrow(diff_by_loc))){
        text <- paste0(text, "居住地 ", diff_by_loc[i,][["居住地"]], ": ", diff_by_loc[i,][["n"]], "人\n")
      } 
      text <- paste0(text, "\n")
    }
    text <- paste0(text, nrow(diff), " \n", url_guide)
    if(TEST){
      print(paste0("TEST for ", target, ": ", text))
    }else{
      POST(url = target, encode = "json", body = list(text = text))
    }
  }
}

notify_infection <- function(infection, target){
  latest <- infection$latest
  pref <- infection$pref 

  file_latest <- paste0("infections_", pref, ".csv")
  file_latest_path <- paste0("data/", file_latest)
  
  if (any(dir("data") %in% file_latest)){
    old_infections <- read_csv(file_latest_path, col_types = cols(.default = "c"))
  }else{
    old_infections <- latest
  }
  
  diff <- latest %>% anti_join(old_infections, by = "index") 
  
  growth <- nrow(latest) - nrow(old_infections) 
  
  check_health <- growth >= 0
  if(check_health){
    if(growth > 0){ 
      post_infection(diff, pref, target)
    }else{
      if(TEST){
        print(paste0("TEST for ", target, ": No infection in ", pref))
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

update_record <- function(infection){
  latest <- infection$latest
  pref <- infection$pref

  file_latest <- paste0("infections_", pref, ".csv")
  file_latest_path <- paste0("data/", file_latest)

  if (any(dir("data") %in% file_latest)){
      old_infections <- read_csv(file_latest_path, col_types = cols(.default = "c"))
  }else{
      old_infections <- latest
  }

  diff <- latest %>% anti_join(old_infections, by = "index")

  growth <- nrow(latest) - nrow(old_infections) 

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

