library(ggplot2)
library(tm)
library(quanteda)
library(tm)
library(stringi)
library(stringr)
library(data.table)


QuadgramData <- readRDS( file = "QuadgramData.rds" )
QuadgramProb <- readRDS( file = "QuadgramProb.rds" )
TrigramData <- readRDS( file = "UpdatedTrigram.rds" )
TrigramProb <- readRDS( file = "TrigramProb.rds" )
BigramData <- readRDS( file = "BigramData.rds" )
BigramProb <- readRDS( file = "BigramProb.rds" )
UnigramData <- readRDS( file = "UnigramData.rds" )

separateTerms = function(x){
  # Pre-allocate
  firstTerms = character(length(x))
  lastTerm = character(length(x))
  
  for(i in 1:length(x)){
    posOfSpaces = gregexpr("_", x[i])[[1]]
    posOfLastSpace = posOfSpaces[length(posOfSpaces)]
    firstTerms[i] = substr(x[i], 1, posOfLastSpace-1)
    lastTerm[i] = substr(x[i], posOfLastSpace+1, nchar(x[i]))
  }
  
  list(firstTerms=firstTerms, lastTerm=lastTerm)
}


##########################################################################################################
# This function is used to get the last "num" terms of a given text.
# Input: We are students of the university
# Output: of_the_university (if num = 3)
getLastTerms = function(inputString, num = 3){
  # Preprocessing
  inputString = gsub("[[:space:]]+", " ", str_trim(tolower(inputString)))
  
  # Now, ready!
  words = unlist(strsplit(inputString, " "))
  
  if (length(words) < num){
    stop("Number of Last Terms: Insufficient!")
  }
  
  from = length(words)-num+1
  to = length(words)
  tempWords = words[from:to]
  
  paste(tempWords, collapse="_")
}

getProbabilityFrom4Gram = function(inputString){
  # Preprocessing
  mylist = separateTerms(getLastTerms(inputString, num = 4))
  
  inFirstTerms4gram = mylist$firstTerms
  inLastTerm4gram = mylist$lastTerm
  
  finalProb = -1
  
  oneGroupIn4Gram = QuadgramData[firstTerms == inFirstTerms4gram]
  if (nrow(oneGroupIn4Gram) > 0){
    # Algorithm here
    oneRecordIn4Gram = QuadgramData[firstTerms == inFirstTerms4gram & lastTerm == inLastTerm4gram]
    if (nrow(oneRecordIn4Gram) > 0){
      # We found one in 4-gram
      all_freq = sum(oneGroupIn4Gram$freq)
      finalProb = ((oneRecordIn4Gram$discount * oneRecordIn4Gram$freq) / all_freq)
      ### We're done!
    } else {
      # NOT found in 4-gram => check 3,2gram & 1-gram
      mylist = separateTerms(getLastTerms(inputString, num = 3))
      inFirstTerms3gram = mylist$firstTerms
      inLastTerm3gram = mylist$lastTerm
      
      # Get the left-over probability so that we can distribute it for lower-order grams.
      beta_leftoverprob = QuadgramProb[firstTerms == inFirstTerms4gram]$leftoverprob
      
      oneGroupIn3Gram = TrigramData[firstTerms == inFirstTerms3gram]
      oneRecordIn3Gram = TrigramData[firstTerms == inFirstTerms3gram & lastTerm == inLastTerm3gram]
      if (nrow(oneRecordIn3Gram) > 0){
        # We found one in 3-gram!
        # We only consider ones that do not appear in 4-grams...
        oneGroupIn3Gram_Remain = oneGroupIn3Gram[!(oneGroupIn3Gram$lastTerm %in% oneGroupIn4Gram$lastTerm)]
        all_freq = sum(oneGroupIn3Gram$freq)
        
        alpha = beta_leftoverprob / sum((oneGroupIn3Gram_Remain$freq * oneGroupIn3Gram_Remain$discount) / all_freq)
        
        finalProb = alpha * ((oneRecordIn3Gram$freq * oneRecordIn3Gram$discount ) / all_freq)
        ### We're done!
      } else {
        
        mylist = separateTerms(getLastTerms(inputString, num = 2))
        inFirstTerms2gram = mylist$firstTerms
        inLastTerm2gram = mylist$lastTerm
        
        oneGroupIn2Gram = BigramData[firstTerms == inFirstTerms2gram]
        oneRecordIn2Gram = BigramData[firstTerms == inFirstTerms2gram & lastTerm == inLastTerm2gram]
        
        
        if (nrow(oneRecordIn2Gram) > 0){
          # We found one in 2-gram!
          # We only consider ones that do not appear in 4-grams...
          oneGroupIn2Gram_Remain = oneGroupIn2Gram[!(oneGroupIn2Gram$lastTerm %in% oneGroupIn4Gram$lastTerm)]
          all_freq = sum(oneGroupIn2Gram$freq)
          
          alpha = beta_leftoverprob / sum((oneGroupIn2Gram_Remain$freq * oneGroupIn2Gram_Remain$discount) / all_freq)
          
          finalProb = alpha * ((oneRecordIn2Gram$freq * oneRecordIn2Gram$discount ) / all_freq)
          ### We're done!
        }else{
          # We only have hope in 1-gram!
          oneGroupIn1Gram = UnigramData# we don't have "firstTerms" here!
          oneRecordIn1Gram = UnigramData[lastTerm == inLastTerm2gram] # what if this returns "zero" row?
          
          oneGroupIn1Gram_Remain = oneGroupIn1Gram[!(oneGroupIn1Gram$lastTerm %in% oneGroupIn3Gram$lastTerm)]
          all_freq = sum(oneGroupIn1Gram$freq)
          
          alpha = beta_leftoverprob / sum((oneGroupIn1Gram_Remain$freq * oneGroupIn1Gram_Remain$discount) / all_freq)
          
          finalProb = alpha * ((oneRecordIn1Gram$freq * oneRecordIn1Gram$discount) / all_freq)
          ### We're done!
          
        }
        
        
      }
    }
  }
  
  
  
  else {
    stop(sprintf("[%s] not found in the 3-gram model.", inFirstTerms3gram))
    # The workaround could be:
    # + Write another function in which we primarily use 2-gram with support from 1-gram.
    # + Increase the corpus size so that the 3-gram can capture more diversity of words...
  }
  
  finalProb
}


getProbabilityFrom3Gram = function(inputString){
  # Preprocessing
  
  #print("Inside Tri function")
  #print("Tri function input is")
  #print(inputString)
  mylist = separateTerms(getLastTerms(inputString, num = 3))
  
  inFirstTerms3gram = mylist$firstTerms
  inLastTerm3gram = mylist$lastTerm
  
  finalProb = -1
  
  oneGroupIn3Gram = TrigramData[firstTerms == inFirstTerms3gram]
  if (nrow(oneGroupIn3Gram) > 0){
    # Algorithm here
    oneRecordIn3Gram = TrigramData[firstTerms == inFirstTerms3gram & lastTerm == inLastTerm3gram]
    if (nrow(oneRecordIn3Gram) > 0){
      # We found one in 3-gram
      all_freq = sum(oneGroupIn3Gram$freq)
      finalProb = ((oneRecordIn3Gram$discount * oneRecordIn3Gram$freq) / all_freq)
      ### We're done!
    } else {
      # NOT found in 3-gram => check 2-gram & 1-gram
      mylist = separateTerms(getLastTerms(inputString, num = 2))
      inFirstTerms2gram = mylist$firstTerms
      inLastTerm2gram = mylist$lastTerm
      
      # Get the left-over probability so that we can distribute it for lower-order grams.
      beta_leftoverprob = TrigramProb[firstTerms == inFirstTerms3gram]$leftoverprob
      
      oneGroupIn2Gram = BigramData[firstTerms == inFirstTerms2gram]
      oneRecordIn2Gram = BigramData[firstTerms == inFirstTerms2gram & lastTerm == inLastTerm2gram]
      if (nrow(oneRecordIn2Gram) > 0){
        # We found one in 2-gram!
        # We only consider ones that do not appear in 3-grams...
        oneGroupIn2Gram_Remain = oneGroupIn2Gram[!(oneGroupIn2Gram$lastTerm %in% oneGroupIn3Gram$lastTerm)]
        all_freq = sum(oneGroupIn2Gram$freq)
        
        alpha = beta_leftoverprob / sum((oneGroupIn2Gram_Remain$freq * oneGroupIn2Gram_Remain$discount) / all_freq)
        
        finalProb = alpha * ((oneRecordIn2Gram$freq * oneRecordIn2Gram$discount ) / all_freq)
        ### We're done!
      } else {
        # We only have hope in 1-gram!
        oneGroupIn1Gram = UnigramData# we don't have "firstTerms" here!
        oneRecordIn1Gram = UnigramData[lastTerm == inLastTerm2gram] # what if this returns "zero" row?
        
        oneGroupIn1Gram_Remain = oneGroupIn1Gram[!(oneGroupIn1Gram$lastTerm %in% oneGroupIn3Gram$lastTerm)]
        all_freq = sum(oneGroupIn1Gram$freq)
        
        alpha = beta_leftoverprob / sum((oneGroupIn1Gram_Remain$freq * oneGroupIn1Gram_Remain$discount) / all_freq)
        
        finalProb = alpha * ((oneRecordIn1Gram$freq * oneRecordIn1Gram$discount) / all_freq)
        ### We're done!
      }
    }
  } else {
    stop(sprintf("[%s] not found in the 3-gram model.", inFirstTerms3gram))
    # The workaround could be:
    # + Write another function in which we primarily use 2-gram with support from 1-gram.
    # + Increase the corpus size so that the 3-gram can capture more diversity of words...
  }
  
  finalProb
}



getProbabilityFrom2Gram = function(inputString){
  # Preprocessing
  
  ###########################bigram checking starts from here##################3
  
  mylist = separateTerms(getLastTerms(inputString, num = 2))
  inFirstTerms2gram = mylist$firstTerms
  inLastTerm2gram = mylist$lastTerm
  
  oneGroupIn2Gram = BigramData[firstTerms == inFirstTerms2gram]
  oneRecordIn2Gram = BigramData[firstTerms == inFirstTerms2gram & lastTerm == inLastTerm2gram]
  if (nrow(oneRecordIn2Gram) > 0){
    
    all_freq = sum(oneGroupIn2Gram$freq)
    finalProb = ((oneRecordIn2Gram$discount * oneRecordIn2Gram$freq) / all_freq)
  }
  
  else{
    
    # Get the left-over probability so that we can distribute it for lower-order grams.
    beta_leftoverprob = BigramProb[firstTerms == inFirstTerms2gram]$leftoverprob
    
    oneRecordIn1Gram = UnigramData[firstTerms == inLastTerm2gram]
    ## oneRecordIn2Gram = DTNgram_bigram[firstTerms == inFirstTerms2gram & lastTerm == inLastTerm2gram]
    if (nrow(oneRecordIn1Gram) > 0){
      
      oneGroupIn1Gram_Remain = oneGroupIn1Gram[!(oneGroupIn1Gram$lastTerm %in% oneGroupIn2Gram$lastTerm)]
      all_freq = sum(oneGroupIn1Gram$freq)
      
      alpha = beta_leftoverprob / sum((oneGroupIn1Gram_Remain$freq * oneGroupIn1Gram_Remain$discount) / all_freq)
      
      finalProb = alpha * ((oneRecordIn1Gram$freq * oneRecordIn1Gram$discount ) / all_freq)     
    }
    
  }
  finalProb
  
}

proposedTerm <- function(text){
  
  tempQuadString = paste(word(text,-3),word(text,-2),word(text,-1),sep = "_")
  tempTriString = paste(word(text,-2),word(text,-1),sep = "_")
  tempBiString = word(text,-1)
  
  print("all terms")
  print(tempQuadString)
  print(tempTriString)
  print(tempBiString)
  
  
  #############For checking quad###########
  sortedQuad <- data.frame(lastTerm = character())
  sortedTri <- data.frame(lastTerm = character())
  sortedBi <- data.frame(lastTerm = character())
  
  searchTermsQuad = QuadgramData[firstTerms == tempQuadString]
  
  if(nrow(searchTermsQuad)>0){
    
    print ("I am at Quad Function")
    inputStringQuad = paste(word(text,-3),word(text,-2),word(text,-1),searchTermsQuad$lastTerm,sep = " ")
    
    searchTermsQuad$Probability = rep(1, nrow(searchTermsQuad))
    
    for(i in 1:length(inputStringQuad)){
      searchTermsQuad[i]$Probability = getProbabilityFrom4Gram(inputStringQuad[i])
    }
    
    sortedQuad = searchTermsQuad[order(-Probability),]
    ##sortedQuad[1]$lastTerm
    print(sortedQuad)
    print("Quad function over")
  }
  
  if(nrow(searchTermsQuad) <6) {
    
  searchTermsTri = TrigramData[firstTerms == tempTriString]
  if(nrow(searchTermsTri)>0){
    
    print ("I am at tri function")
    
    inputStringTri = paste(word(text,-2),word(text,-1),searchTermsTri$lastTerm,sep = " ")
    
    searchTermsTri$Probability = rep(1, nrow(searchTermsTri))
    
    for(i in 1:length(inputStringTri)){
      searchTermsTri[i]$Probability = getProbabilityFrom3Gram(inputStringTri[i])
    }
    
    sortedTri = searchTermsTri[order(-Probability),]
    
    print("tri function over")
  
  
  }
  if(nrow(searchTermsTri)<6){
    searchTermsBi = BigramData[firstTerms == tempBiString]
    
    if(nrow(searchTermsBi)>0){
      inputStringBi = paste(word(text,-1),searchTermsBi$lastTerm,sep = " ")
      
      print("I am at bi function")
      print(length(inputStringBi))
      searchTermsBi$Probability = rep(1, nrow(searchTermsBi))
      
        for(i in 1:length(inputStringBi)){
          searchTermsBi[i]$Probability = getProbabilityFrom2Gram(inputStringBi[i])
        }
      
     
      
      sortedBi = searchTermsBi[order(-Probability),]
      #   sortedBi[1]$lastTerm
      #print("bi function over")
    }
  }
  
  }
  
  print(sortedBi)
  #print("----------------------Searching Phase over.")
  
  
  ##Searching is done now refactoring what to show in screen 
  #print("Searching is done now refactoring what to show in screen ")
  
  if(nrow(sortedQuad)>0){
    print("Condition Validated  : Quad is having more than one record.")
    
    
    
    if(nrow(sortedQuad)<=5){
      print("Condition Validated : Quad is having less than 5 record.")
      finalresultQuad <-  sortedQuad[1:nrow(sortedQuad),]
      finalresultQuad[, ("Type") := c("Quad")]
      
      if(nrow(sortedTri)>0){
        print("Condition Validated : Tri is having more than one record.")
        
        if(nrow(sortedTri)>=5){
          print("Condition Validated : Tri is having less than 5 record.")
          
          j = 5-nrow(sortedQuad)
          finalresultTri <-  sortedTri[1:j,]
          finalresultTri[, ("Type") := c("Tri")]
          print("Output of finalResulttri")
          print(finalresultTri)
          print("Closing point of Condition : Tri is having less than 5 record")
        }
        
        else{
          print("Condition Validated : Tri is having more than 5 record.")
          finalresultTri <-  sortedTri[1:nrow(sortedTri),]
          finalresultTri[, ("Type") := c("Tri")]
          
          print("Output of finalResulttri")
          print(finalresultTri)
          print("Closing point of Condition : Tri is having more than 5 record")
          
        }
        
        
        
        if((nrow(sortedQuad)+nrow(sortedTri)) <5){
          print("Condition Validated : If quad and tri has less than 5 rows.")
          
          k = 5-(nrow(sortedQuad)+nrow(sortedTri))
          
          finalresultBi <-  sortedBi[1:k,]
          finalresultBi[, ("Type") := c("Bi")]
          
          print("finalresultBi")
          print(finalresultBi)
          
          print("finalresult inside Bi")
          finalresult <- c(finalresultQuad$lastTerm,finalresultTri$lastTerm,finalresultBi$lastTerm)
          finalDiscount <- c(finalresultQuad$discount,finalresultTri$discount,finalresultBi$discount)
          finalType <- c(finalresultQuad$Type,finalresultTri$Type,finalresultBi$Type)
          print(finalresult)
          print("Closing point of condition : If quad and tri has less than 5 rows.")
        }
        else{
          print("Condition Validated : If quad and tri has more than 5 rows.")
          finalresult <- c(finalresultQuad$lastTerm,finalresultTri$lastTerm)
          finalDiscount <- c(finalresultQuad$discount,finalresultTri$discount)
          finalType <- c(finalresultQuad$Type,finalresultTri$Type)
          print("Finalresult")
          print(finalresult)
          print("Closing point of condition : If quad and tri has more than 5 rows.")
        }
      }
    }
    else{
     
      finalresultQuad <-  sortedQuad[1:5,]
      finalresultQuad[, ("Type") := c("Quad")]
       finalresult <- c(finalresultQuad$lastTerm)
      finalDiscount <- c(finalresultQuad$discount)
      finalType <- c(finalresultQuad$Type)
    }
  }
  else{
    
    print("Condition validated: If quad has 0 rows.")
    
    if(nrow(sortedTri)>0){
      print("Condition validated: If tri has more than 0 rows.")
      if(nrow(sortedTri)>=5){
        
        print("Condition validated: If tri has more than 5 rows.")
        
        finalresultTri <-  sortedTri[1:5,]
        finalresultTri[, ("Type") := c("Tri")]
        print("FinalresultTri")
        print(finalresultTri)
        
        print("Closing point of condition : If tri has more than 5 rows.")
        finalresult <- c(finalresultTri$lastTerm)
        finalDiscount <- c(finalresultTri$discount)
        finalType <- c(finalresultTri$Type)
      }
      else{
        print("Condition validated: If tri has less than 5 rows.")
        
        finalresultTri <-  sortedTri[1:nrow(sortedTri),]
        
        finalresultTri[, ("Type") := c("Tri")]
        print("Tri result")
        print(finalresultTri)
        
        m = 5- nrow(sortedTri)
      
        
        finalresultBi <- sortedBi[1:m,]
      
        finalresultBi[, ("Type") := c("Bi")]
      
        print("Bi result")
        print(finalresultBi)
        print("Closing point of condition : If tri has less than 5 rows.")
        finalresult <- c(finalresultTri$lastTerm,finalresultBi$lastTerm)
        finalDiscount <- c(finalresultTri$discount,finalresultBi$discount)
        finalType <- c(finalresultTri$Type,finalresultBi$Type)
      }
      
      print("Finalresult")
      print(finalresult)
      print("Closing point of condiiton: If tri has more than 0 rows.")
      
    }
    else{
      print("Condition validated: If tri has 0 rows.")
      
      if(nrow(sortedBi)>=5){
        finalresultBi <-  sortedBi[1:5,]
        finalresultBi[, ("Type") := c("Bi")]
         print(finalresultBi)  
      }
      else{
        
        if(nrow(sortedBi)>0){
          finalresultBi <-  sortedBi[1:nrow(sortedBi),]
          finalresultBi[, ("Type") := c("Bi")]
          
        }
        else{
          return("Oops!!! couldn't find the word, Please report this to rish.shu29@gmail.com  Will include in the sample ")
        }
        
      }
      
      
      finalresult <- c(finalresultBi$lastTerm)
      finalDiscount <- c(finalresultBi$discount)
      finalType <- c(finalresultBi$Type)
     
      print("Closing point of condiiton: If tri has 0 rows.")
    }
  }
  
  
  print("The final step of server side")
  
  if(length(finalresult) > 4){
    finalReturn <- c(finalresult,finalDiscount,finalType)
  }
  else{
    finalReturn <- c(finalresult)
  }
  
  finalReturn
}


shinyServer(function(input, output) {
  
  lastText <- ""
  
  
  output$dynamicText <- renderUI({
    
    inputText = input$inputString
    
    if (inputText=="") return("Input text rendered...")
    if (inputText == lastText) return("Input text rendered...")
    
    lastText <<- inputText
    #print ("this is input text ")
    #print (lastText)
    lastText <-  tolower(lastText)
    lastText <- gsub("[']s"," is",lastText)
    lastText <- gsub("[']d"," would",lastText)
    lastText <- gsub("[']ve"," have",lastText)


    temp <- proposedTerm(lastText)
    print("temp Result")
    print(temp)
    
    output$wordPlot <- renderPlot({
      inputText = input$inputString
      wordDf <- data.frame(a=1:1, c=c(as.numeric(temp[6]),as.numeric(temp[7]),as.numeric(temp[8]),as.numeric(temp[9]),as.numeric(temp[10])), word = c(temp[1],temp[2],temp[3],temp[4],temp[5]),type = c(temp[11],temp[12],temp[13],temp[14],temp[15]))
      print(wordDf)
      print(typeof(wordDf$c))
      ggplot(wordDf,aes(y=wordDf$c, x=wordDf$word, fill = wordDf$type)) + geom_bar(position = "identity", stat = "identity") +  theme(plot.background = element_rect(fill = '#3CB371', colour = 'red'))+ coord_cartesian(ylim=c(0,1))+ geom_text(aes(label=sprintf("%0.2f", round(wordDf$c, digits = 2))), vjust=0)+xlab("Predicted Text")+ylab("Probability of Occurrence") + guides(fill=guide_legend(title="Type of n-gram"))
    })
    
    print("temp")
    print(temp)
    HTML(paste(temp[1], temp[2],temp[3],temp[4],temp[5], sep="<br/>"))
    
  })
 
  
  
})