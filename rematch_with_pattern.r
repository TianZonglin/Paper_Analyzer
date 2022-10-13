##!/bash
# -*- coding: utf-8 -*-
# @Time    : 2022.5.3 9:22
# @Author  : ${AUTHOR}
# @FileName: ${NAME}.R
# @Software: ${PRODUCT_NAME}


library(readxl)
library(stringr)


data <- read_xlsx("C:\\Users\\TZL\\Desktop\\ProjPaper\\ccC\\Type of leadership.xlsx")
matches <- read_xlsx("C:\\Users\\TZL\\Desktop\\ProjPaper\\ccC\\IncludedPaper.xlsx")


# delete useless columns 
data<-data.frame(data["Author"], data["Year"], data["AAACategory_leader"])
matches<-data.frame(matches["Ref"], matches["Year"])

# delete NA rows - clean
data<-na.omit(data)
matches<-na.omit(matches)


# length
len_data =  nrow(data["Author"])
len_matches =  nrow(matches["Ref"])

# add a column
data$new_name = rep("-", len_data)
data$tmpAuthor = data$Author
matches$tmpName = matches$Ref

# format the splitting - clean
index = 1
while(index < len_data){
  
  temp = toupper(data[index, "tmpAuthor"])
  # delete blanks
  temp = gsub(" ", "", temp)
  
  if(!str_detect(temp, "\\.,")){
    # multiple authors with ','
    if(length(strsplit(temp, "&")[[1]]) - 1 > 0){
      # multiple authors with "&" not ".,"
      temp = gsub("&", ".,", temp)
    # multiple authors with "," not ".,"  
    }else if(length(strsplit(temp, ",", fixed = TRUE)[[1]]) - 1 > 1){
      
      temp = gsub(",", ".,", temp)
    }
    # multiple authors with "and" not ".,"
    temp = gsub("AND", ".,", temp)
    # multiple authors with ";" not ".,"
    temp = gsub(";", ".,", temp)

    # multiple authors with "•" not ".,"
    temp = gsub("•", ".,", temp)
  }
  temp = gsub("&", "", temp)
  # delete unused &
  data[index, "tmpAuthor"] = temp
  
  index = index + 1
}

# format the splitting - clean
index = 1
while(index < len_matches){
  
  templist = toupper(matches[index, "tmpName"])
  # delete blanks
  templist = gsub(" ", "", templist)
  # multiple authors with "," not "&"
  if(!str_detect( templist, "ETAL")){
    templist = gsub(",", "\\.,", templist)
  }else{
    # delete et al
    templist = gsub(",ETAL", "", templist)
    templist = gsub("ETAL", "", templist)   
  }
  # format the splitting
  templist = gsub("&", ",", templist)
  # delete ( + )
  templist = gsub("\\(", "", gsub("\\)", "", templist))
  
  matches[index, "tmpName"] = templist
  #print(templist)
  index = index + 1
}

#View(data)
#View(matches)

#loop for matching
# compare every author's names with name-list(matches) 
index = 1
while(index < len_data){
  # split authors
  authors = as.vector(
    unlist(strsplit(data[index, "tmpAuthor"], ".,", fixed = TRUE)))
  number = length(authors)
  years = data[index, "Year"]
  
  # loop the name list
  k = 1
  while(k < len_matches){
    # split name and year
    item = as.vector(
      unlist(strsplit(matches[k, "tmpName"], ".,", fixed = TRUE)))
    name = item[1]
    year = item[2]
    
    # split names
    match_names = as.vector(unlist(strsplit(name, ",", fixed = TRUE)))
    number_names = length(match_names)
    
    # only one author v.s. one name
    if(number == 1 && number_names == 1){
      # compare 
      if(str_detect( authors[1], match_names[1]) && years == year){
        
        data[index, "new_name"] = matches[k, "Ref"]
        
        print(paste("single author, in position", k))
        break
      }
    }else if(number >= 2 && number_names >= 2){
      # only compare first two authors
      if(str_detect(authors[1], match_names[1]) && str_detect(authors[2], match_names[2]) && years == year){
        
        data[index, "new_name"] = matches[k,"Ref"]
        
        print(paste("multi-authors, in position", k))
        break
      }
    }
    k = k + 1
  }
  
  #print(index) # to 210
  
  index = index + 1
}

# delete temp column
data<-data[, -grep("tmpAuthor", colnames(data))]
matches<-matches[, -grep("tmpName", colnames(matches))]
 

#View(data)
#View(matches)


# regroup and output
# matching all alphabet categories
group <- LETTERS[seq(1,26)]
object = NULL

for(cate in group){
  index = 1
  while(index < len_data){
    if(data[index, "AAACategory_leader"] == cate){
      # only paste the available format authors
      if(data[index, "new_name"] != "-"){
        object[cate] = paste(object[cate], ", ", data[index, "new_name"], sep = "")
      }
    }
    index = index + 1 
  }
  if(!is.null(object[cate]) && !is.na(object[cate])){
    
    print(paste("[[", cate, "]]"))   
    
    # delete the first comma while printing
    arr = unlist(strsplit(as.character(object[cate]), ","))
    rst = substring(str_c(arr[2:length(arr)],collapse=","), 2)
    
    print(rst )
  }
}

#end
