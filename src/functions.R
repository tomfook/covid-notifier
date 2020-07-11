
post_infection <- function(diff, pref, target, test){
  pref_name <- switch(pref, kyoto = "京都府", osaka = "大阪府", okayama = "岡山県")
  icon <- switch(pref, kyoto = ":kyo:", osaka = ":han:", okayama = ":oka:")
  col_date <- switch(pref, kyoto = "発表日", osaka = "date", okayama = "月日")
  col_age <- "年代"
  col_sex <- "性別"
  col_location <- switch(pref, kyoto = "居住地等", osaka = "居住地", okayama = "居住地")
  url_guide <- switch(pref,
		    kyoto = "http://www.pref.kyoto.jp/kentai/corona/hassei1-50.html",
		    osaka = "http://www.pref.kyoto.jp/kentai/corona/hassei1-50.html",
		    okayama = "https://www.pref.okayama.jp/page/667843.html"
		    ) 
  
  for(i in seq(to = nrow(diff))){
    text <- paste0(icon, " ", pref_name, "発表\n",
		  col_date, ": ", diff[i,][[col_date]],
		  col_age, ": ", diff[i,][[col_age]],
		  col_sex, ": ", diff[i,][[col_sex]],
		  col_location, ": ", diff[i,][[col_location]],
		  "\n", url_guide
		  ) 
    if(test){
      print(paste("TEST:", text))
    }else{
      POST(url = target, encode = "json", body = list(text = text))
    }
  }
}

