library('stylo')

setwd("~/Testing_analySt/hungarian_letters/primary_set")

score <- function(tableOfResults){
  sumOfWrongGuesses <- 0;
  for(i in 1:length(rownames(tableOfResults))){
    sumOfPercents <- 0
    sumOfEPercents <- 0
    count <- 0
    Ecount <- 0
    for(j in 1:length(colnames(tableOfResults))){
      rName <- rownames(tableOfResults)[[i]]
      cName <- colnames(tableOfResults)[[j]]
      if(rName != cName){
        if(strsplit(rName, "_")[[1]][[1]] == strsplit(cName, "_")[[1]][[1]]){
          sumOfPercents = sumOfPercents + tableOfResults[i, j]
          count = count + 1
        }else{
          sumOfEPercents = sumOfEPercents + tableOfResults[i, j]
          Ecount = Ecount + 1
        }
      }
    }
    if(Ecount == 0 || (count > 0 && (sumOfPercents / count) < (sumOfEPercents / Ecount))){
      sumOfWrongGuesses = sumOfWrongGuesses - 1;
    } else {
      sumOfWrongGuesses = sumOfWrongGuesses + 1;
    }
  }
  return(sumOfWrongGuesses)
}

preanalyzer <- function(language){
  corpus <- load.corpus()
  
  parsed <- parse.corpus(
    corpus,
    "UTF-8",
    language = language,
    features = "w",
    ngram.size = 1
  )
  frequencyList <- make.frequency.list(parsed, value = TRUE)
  
  meanOfFreq <- mean(frequencyList)
  firstCap <- 0.01
  secondCap <- 0.05
  
  feat_ <- "w"
  val_ <- 2
  if(meanOfFreq < firstCap){
    feat_ <- "c"
    val_ <- 3
  } else if(meanOfFreq < secondCap){
    feat_ <- "w"
    val_ <- 1
  }
  features_select <- feat_
  ngram_input <- val_

  frequencyList2 <- make.frequency.list(parsed, value = TRUE, relative = FALSE)
  avglen <- sum(frequencyList2) / length(frequencyList2)
  val_ <- round((avglen / (avglen + 5)) * 100)
  mfw_val <- val_
  
  lengths_ <- vector(mode = "numeric", length = length(corpus))
  i <- 0
  for(item in corpus){
    var_ <- txt.to.words.ext(item)
    lengths_[i] <- length(var_)
    i = i + 1
  }
  avgOfFileLength <- mean(lengths_)
  culling_ <- round((avgOfFileLength / (avgOfFileLength + (100000 / length(corpus)))) * 100)
  culling_input <- culling_
  
  listcutoff <- 5000
  if(length(frequencyList) > sum(frequencyList2)/10){
    listcutoff = round(sum(frequencyList2)/10)
  }
  
  namesOfCorpus <- c()
  numbersOfCorpus <- c()
  index <- 1
  for(name in names(corpus)){
    writer <- strsplit(name, "_")[[1]][[1]]
    if(writer %in% namesOfCorpus){
      numbersOfCorpus[match(writer, namesOfCorpus)[[1]]] <- numbersOfCorpus[match(writer, namesOfCorpus)[[1]]] + 1
    }else{
      namesOfCorpus[[index]] <- writer
      index <- index + 1
    }
  }
  isVariedDistance <- FALSE
  for(itemA in numbersOfCorpus){
    for(itemB in numbersOfCorpus){
      if(itemA > itemB * 1.5){
        isVariedDistance <- TRUE
        break;
      }
    }
  }
  
  isVariedLength <- FALSE
  for(itemA in lengths_){
    for(itemB in lengths_){
      if(itemA > itemB * 2){
        isVariedLength <- TRUE
        break;
      }
    }
  }
  
  plotheight <- 10 + round(length(corpus) / 5)
  
  plotwidth <- 10 + round(length(corpus) / 10)
  
  sampling_input <- round(mean(lengths_) / 2)
  
  preanalyzerSet <- c("isVariedLength" = isVariedLength, "isVariedDistance" = isVariedDistance)
  
  if(preanalyzerSet[[1]]){
    sampling <- "normal.sampling"
  }else{
    sampling <- "no.sampling"
  }
  
  if(preanalyzerSet[[2]]){
    distance <- "Eder's Simple Distance" = "dist.simple"
  }else{
    distance <- "dist.delta"
  }
  
  result <- stylo(
    
    # Invoke without GUI with predefined corpus
    gui = FALSE, 
    parsed.corpus = parsed,
    
    # Input & language
    corpus.format = "txt",
    encoding = "UTF-8",
    corpus.lang = language,
    
    # Features
    analyzed.features = features_select,
    ngram.size = ngram_input,
    preserve.case = FALSE,
    
    # Most Frequent Words
    mfw.min = mfw_val,
    mfw.max = mfw_val,
    mfw.incr = 0,
    start.at = 1,
    
    # Culling
    culling.min = culling_input,
    culling.max = culling_input,
    culling.incr = 0,
    mfw.list.cutoff = listcutoff,
    delete.pronouns = FALSE,
    
    #Statistics
    analysis.type = "CA",
    consensus.strength = 0.5,
    text.id.on.graph = "labels",
    add.to.margins = 2,
    label.offset = 3,
    pca.visual.flavour = "classic",
    dendrogram.layout.horizontal = TRUE,
    distance.measure = distance,
    
    # Sampling
    sampling = sampling,
    sample.size = 5000,
    
    # Output
    display.on.screen = TRUE,
    write.pdf.file = FALSE,
    write.jpg.file = FALSE,
    write.png.file = FALSE,
    write.svg.file = FALSE,
    plot.options.reset = FALSE,
    plot.custom.height = plotheight,
    plot.custom.width = plotwidth,
    plot.font.size = 10,
    plot.line.thickness = 1,
    colors.on.graphs = "colors",
    titles.on.graphs = TRUE,
    
    # Undocumented but useful options
    custom.graph.filename = 'graph'
  )
  scores <- score(result$distance.table)
  return(scores)
}

analyzer <- function(language, precycles, cycles){
  
  corpus <- load.corpus()
  
  parseCorpus <- function(feature, ngram) {
    parsedCorpus <- parse.corpus(
      corpus, 
      encoding = "UTF-8",
      language = language,
      features = feature,
      ngram.size = ngram
    )
    return(parsedCorpus)
  }
  
  #parsed <- parseCorpus(feature = "w", ngram = 1)
  
  distanceSelect <- function(x){
    distanceVector <- c(
      "dist.simple",
      "dist.manhattan",
      "dist.canberra",
      "dist.euclidean",
      "dist.cosine",
      "dist.delta",
      "dist.argamon",
      "dist.eder"
    )
    return(distanceVector[[x]])  
  }
  
  typeSelect <- function(x){
    r <- ifelse(
      x,
      "c",
      "w"
    )
    return(r)
  }
  
  #vector: ngramtype(true/false), ngramSize(1-7), mfwPercent(0-100), cullingPercent(0-100), distanceMeasurement(1-8)
  invokeStylo <- function(optionVector) {
    result = stylo(
      
      # Invoke without GUI with predefined corpus
      gui = FALSE, 
      parsed.corpus = parsed,
      
      # Input & language
      corpus.format = "txt",
      encoding = "UTF-8",
      corpus.lang = language,
      
      # Features
      analyzed.features = typeSelect(optionVector[[1]]),
      ngram.size = ifelse(optionVector[[1]], optionVector[[2]], round(optionVector[[2]] / 2)),
      preserve.case = FALSE,
      
      # Most Frequent Words
      mfw.min = optionVector[[3]],
      mfw.max = optionVector[[3]],
      mfw.incr = 0,
      start.at = 1,
      
      # Culling
      culling.min = optionVector[[4]],
      culling.max = optionVector[[4]],
      culling.incr = 0,
      mfw.list.cutoff = 5000,
      delete.pronouns = FALSE,
      
      #Statistics
      analysis.type = "CA",
      consensus.strength = 0.5,
      text.id.on.graph = "labels",
      add.to.margins = 2,
      label.offset = 3,
      pca.visual.flavour = "classic",
      dendrogram.layout.horizontal = TRUE,
      distance.measure = distanceSelect(optionVector[[5]]),
      
      # Sampling
      sampling = TRUE,
      sample.size = 10000,
      
      # Output
      display.on.screen = TRUE,
      write.pdf.file = FALSE,
      write.jpg.file = FALSE,
      write.png.file = FALSE,
      write.svg.file = FALSE,
      plot.options.reset = FALSE,
      plot.custom.height = 10,
      plot.custom.width = 10,
      plot.font.size = 10,
      plot.line.thickness = 1,
      colors.on.graphs = "colors",
      titles.on.graphs = TRUE,
      
      # Undocumented but useful options
      custom.graph.filename = 'graph'
    )
    return(result)
  }
  
  checkbounds <- function(v){
    upperBounds <- c(1, 6, 100, 90, 8)
    lowerBounds <- c(0, 2, 20, 0, 1)
    for(i in 1:5){
      if(v[[i]] < lowerBounds[[i]] || v[[i]] > upperBounds[[i]])
        return(FALSE)
    }
    if((v[[2]] > 2 && v[[4]] > 30) || (v[[2]] > 4 && v[[4]] > 10))
      return(FALSE)
    return(TRUE)
  }
  
  lut <- c()
  res <- c()
  index <- 1
  candidates <- c()
  results <- c()
  while(index < precycles + 1){
    candidates[[index]] <- c(
      ifelse(runif(1) > 0.5, TRUE, FALSE),
      round(runif(1)*4) + 2,
      round(runif(1)*16)*5 + 20,
      round(runif(1)*18)*5,
      round(runif(1)*7) + 1
    )
    if(checkbounds(candidates[[index]])){
      sc <- 100000
      try({
        parsed <- parseCorpus(typeSelect(candidates[[index]][[1]]), candidates[[index]][[2]])
        result <- invokeStylo(candidates[[index]])
        sc <- score(result$distance.table)
      })
      results[[index]] <- sc
      lut[[index]] <- candidates[[index]]
      res[[index]] <- results[[index]]
      index = index + 1
    }
  }
  start <- candidates[[match(min(results), results)]]
  e <- results[[match(min(results), results)]]
  changeVector <- c(1, 1, 5, 5, 1)
  
  kmax <- cycles
  k <- kmax
  while(k > 0){
    changeIndex <- round(runif(1)*4)+1
    nextVector <- start
    nextVector[[changeIndex]] <- start[[changeIndex]] + ifelse(runif(1) > 0.5, -1, 1) * changeVector[[changeIndex]]
    if(checkbounds(nextVector)){
      enew <- NULL
      for(i in 1:length(lut)){
        if(identical(x = lut[[i]], y = nextVector, num.eq = TRUE)){
          enew <- res[[i]]
        }
      }
      if(is.null(enew)){
        enew <- 100000
        try({
          if(changeIndex < 3){
            parsed <- parseCorpus(typeSelect(nextVector[[1]]), nextVector[[2]])
          }
          result <- invokeStylo(nextVector)
          enew <- score(result$distance.table)
        })
        lut[[index]] <- nextVector
        res[[index]] <- enew
        index = index + 1
      }
      Temp <- k/kmax
      if(enew < e){
        P <- 1
      }else {
        P <- exp((-1*(enew - e))/Temp)
      }
      if(P > runif(1)){
        e <- enew
        start <- nextVector
      }
      k = k - 1
    }
  }
  
  for(i in 1:length(res)){
    if(res[[i]] < e){
      e <- res[[i]]
      start <- lut[[i]]
    }
  }
  
  logData <- c()
  index2 <- 1
  for(i in 1:length(lut)){
    logData[[index2]] <- toString(c(lut[[i]], res[[i]]))
    index2 = index2 + 1
  }
  logData[[index2]] <- " "
  index2 = index2 + 1
  logData[[index2]] <- toString(c(start, e))
  
  fileConn<-file("log.txt")
  writeLines(logData, fileConn, sep = "\n")
  close(fileConn)
  
  returnvec <- c(
    typeSelect(start[[1]]),
    ifelse(start[[1]], start[[2]], round(start[[2]]/2)),
    start[[3]],
    start[[4]],
    distanceSelect(start[[5]])
  )
  
  return(e)
}

result <- preanalyzer('German')
print(result)

result <- analyzer("Hungarian", 50, 100)
print(result)