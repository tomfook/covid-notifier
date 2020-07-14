post_infection <- function(diff, pref, target, nmax = 20){
  col <- c(date = "発表日", age = "年代", sex = "性別", location = "居住地")

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
  		  col["date"], ": ", diff[i,][[col["date"]]], ", ",
  		  col["age"], ": ", diff[i,][[col["age"]]], ", ",
  		  col["sex"], ": ", diff[i,][[col["sex"]]], ", ",
  		  col["location"], ": ", diff[i,][[col["location"]]],
  		  "\n", url_guide
  		  ) 
      if(TEST){
        print(paste0("TEST for ", target, ": ", text))
      }else{
        POST(url = target, encode = "json", body = list(text = text))
      }
    }
  }else{
    text <- paste0(icon, " ", pref_name, " 新規感染者多数", nrow(diff), " \n", url_guide)
    if(TEST){
      print(paste0("TEST for ", target, ": ", text))
    }else{
      POST(url = target, encode = "json", body = list(text = text))
    }
  }
}

notify_infection <- function(infections, pref, target){
  file_latest <- paste0("infections_", pref, ".csv")
  file_latest_path <- paste0("data/", file_latest)
  
  if (any(dir("data") %in% file_latest)){
    old_infections <- read_csv(file_latest_path, col_types = cols(.default = "c"))
  }else{
    old_infections <- infections
  }
  
  diff <- infections %>% anti_join(old_infections, by = "index") 
  
  growth <- nrow(infections) - nrow(old_infections) 
  
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

update_record <- function(infections, pref){
  file_latest <- paste0("infections_", pref, ".csv")
  file_latest_path <- paste0("data/", file_latest)

  if (any(dir("data") %in% file_latest)){
      old_infections <- read_csv(file_latest_path, col_types = cols(.default = "c"))
  }else{
      old_infections <- infections
  }

  diff <- infections %>% anti_join(old_infections, by = "index")

  growth <- nrow(infections) - nrow(old_infections) 

  check_health <- growth >= 0
  if(check_health) write_csv(infections, file_latest_path, na = "")
}

zentohan <- function(text){
  out <- text
  zennum <- "０１２３４５６７８９"
  for(i in 0:9){
    out <- gsub(substr(zennum, i+1, i+1), i, out)
  }
  return(out)
}
