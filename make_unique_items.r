#install.packages("readxl")
library(readxl)
 

data <- read.csv("C:\\Users\\TZL\\Desktop\\paper_cleann.csv",header = TRUE)
data <- data.frame(data["key"],data["year"],data["title"],data["author"],data["abstract"])
data$hit = rep("+",nrow(data["title"]))



CP <- function(a,b,v){
  sum_words=0
  if(is.na(a)||is.na(b)){
    return(0)
  }
  for (i in 1:length(a)){
    if(stringr::str_detect(b,a[i])){
      sum_words = sum_words +1
    }
  }
  return(sum_words/length(a))
}

rows = nrow(data["title"])

index <- 1

while (index <= rows) {
  
  year = data[index,2]
  title = data[index,3]
  print(data["key"][index,])

  source = toupper(gsub(" ","",title))
  words = as.vector(unlist(strsplit(toupper(title)," ")))
  words = toupper(gsub("[^[:alnum:]]","",words))
  
  authorA = as.vector(unlist(strsplit(toupper( data[index,4])," ")))
  abstA = as.vector(unlist(strsplit(toupper( data[index,5])," ")))
  abstA = toupper(gsub("[^[:alnum:]]","",abstA))

 
  
  for( k in c(1:20)){
    sub_year = data[index+k,2]
    sub_title = data[index+k,3]
    target = toupper(gsub(" ","",sub_title))
    
    if(index+k <= rows && abs(nchar(source)-nchar(target)) < 20){
      #if(data["key"][index+k,]=="7206") print(paste( CP(words,target), CP(authorA,authorB), CP(abstA,abstB)))
      if(CP(words,target)>0.7){
        authorB = toupper(gsub(" ","", data[index+k,4]))
        abstB = toupper(gsub(" ","", data[index+k,5]))
        if(CP(authorA,authorB)>0.9 && CP(words,target)>0.9 ) {
          
        }else if(sub_year!="-" && year!="-" && sub_year!=year && CP(authorA,authorB)<0.8){
          data["hit"][index,] = "@"
          index = index + k
          break
        }else if(sub_year!="-" && year!="-" && sub_year!=year && CP(words,target)<0.95){
          data["hit"][index,] = "@"
          index = index + k
          break
        }else if((sub_year == year && CP(authorA,authorB)>=0.5)
                 || (abstA!="-" && abstB=="-" && nchar(abstA)<=1 && nchar(abstB)<=1 &&CP(abstA,abstB)>0.8) 
                 || (CP(words,target)>0.8 && CP(authorA,authorB)==1)){
        }else{
          data["hit"][index,] = "@"
          index = index + k
          break
        }
      }
      else{
        data["hit"][index,] = "@"
        index = index + k
        break
      }
    }else{
      data["hit"][index,] = "@"
      index = index + k
      break
    }
  }
}



data[is.na(data)] <- "-"
data[data=='+'] <- NA
cleaned = na.omit(data)


write.csv(data,"C:\\Users\\TZL\\Desktop\\paper_marked2.csv", row.names = TRUE) 


write.csv(cleaned,"C:\\Users\\TZL\\Desktop\\paper_cleann.csv", row.names = TRUE) 















































title_s <- NULL

iter = 1
rows = 100


data$S_title = rep("-",nrow(data["title"]))
for (this_title in data["title"][1:rows,]) {
  index = 1
  matches = NULL
  source = toupper(gsub(" ","",this_title))
  words = as.vector(unlist(strsplit(toupper(this_title)," ")))
  words = toupper(gsub("\\(","",words))
  words = toupper(gsub("\\)","",words))
  words = toupper(gsub("\\:","",words))
  words = toupper(gsub("\\'","",words))
  words = toupper(gsub("\\[","",words))
  words = toupper(gsub("\\]","",words))
  words = toupper(gsub("\\?","",words))
 
  #print(source)
  for (per_title in data["title"][1:rows,]) {

    target = toupper(gsub(" ","",per_title))
    if(abs(nchar(source)-nchar(target)) < 5){
      sum_words=0
      for (i in 1:length(words)){
        if(stringr::str_detect(target,words[i])){
          sum_words = sum_words +1
        }
      }
      #similarity = sum_words/length(unlist(strsplit(per_title," ")))
      similarity = sum_words/length(words)
      #print(target)
      #print(abs(nchar(source)-nchar(target)))
      #print(similarity)
      if(similarity > 0.65 && iter != index){
        matches = paste(matches,data["key"][index,],sep=" ")
      }
    }
    index = index + 1
  }
 
  if(!is.null(matches)){ 
    data["S_title"][iter,] = matches
  }
  #(matches)
  print(iter)
  iter = iter + 1
}

 




 


