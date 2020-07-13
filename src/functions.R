post_infection <- function(diff, pref, target, test){
  col <- c(date = "発表日", age = "年代", sex = "性別", location = "居住地")

  pref_name <- switch(pref, kyoto = "京都府", osaka = "大阪府", okayama = "岡山県")
  icon <- switch(pref, kyoto = ":kyo:", osaka = ":han:", okayama = ":oka:")
  url_guide <- switch(pref,
		    kyoto = "http://www.pref.kyoto.jp/kentai/corona/hassei1-50.html",
		    osaka = "http://www.pref.osaka.lg.jp/hodo/index.php?site=fumin",
		    okayama = "https://www.pref.okayama.jp/page/667843.html"
		    ) 
  
  if(nrow(diff) < 20){
    for(i in seq(to = nrow(diff))){
      text <- paste0(icon, " ", pref_name, "発表\n",
  		  col["date"], ": ", diff[i,][[col["date"]]], ", ",
  		  col["age"], ": ", diff[i,][[col["age"]]], ", ",
  		  col["sex"], ": ", diff[i,][[col["sex"]]], ", ",
  		  col["location"], ": ", diff[i,][[col["location"]]],
  		  "\n", url_guide
  		  ) 
      if(test){
        print(paste0("TEST for ", target, ": ", text))
      }else{
        POST(url = target, encode = "json", body = list(text = text))
      }
    }
  }else{
    text <- paste0(icon, " ", pref_name, " 感染者多数\n", url_guide)
    if(test){
      print(paste0("TEST for ", target, ": ", text))
    }else{
      POST(url = target, encode = "json", body = list(text = text))
    }
  }
}


zentohan <- function(text){
  out <- text
  zennum <- "０１２３４５６７８９"
  for(i in 0:9){
    out <- gsub(substr(zennum, i+1, i+1), i, out)
  }
  return(out)
}
