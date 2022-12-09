## Matthew Ryan
## 15/07/2021
## NOTE: These are not my functions
## These are functions from RISmed that I needed to change so that they actually work
## The change is in one line of ParseMedline
## It is identified as a comment

ParseMedline <- function(x){
  
  Tags <- c("PMID",
            "YearRevised",
            "MonthRevised",
            "DayRevised",
            "YearPubDate",
            "MonthPubDate",
            "DayPubDate",
            "YearArticleDate",
            "MonthArticleDate",
            "DayArticleDate",
            "YearEntrez",
            "MonthEntrez",
            "DayEntrez",
            "HourEntrez",
            "MinuteEntrez",
            "YearMedline",
            "MonthMedline",
            "DayMedline",
            "HourMedline",
            "MinuteMedline",
            "YearAccepted",
            "MonthAccepted",
            "DayAccepted",
            "HourAccepted",
            "MinuteAccepted",
            "YearReceived",
            "MonthReceived",
            "DayReceived",
            "HourReceived",
            "MinuteReceived",
            "YearEpublish",
            "MonthEpublish",
            "DayEpublish",
            "HourEpublish",
            "MinuteEpublish",
            "YearPpublish",
            "MonthPpublish",
            "DayPpublish",
            "HourPpublish",
            "MinutePpublish",
            "YearPmc",
            "MonthPmc",
            "DayPmc",
            "HourPmc",
            "MinutePmc",
            "YearPubmed",
            "MonthPubmed",
            "DayPubmed",
            "HourPubmed",
            "MinutePubmed",
            "ISSN",
            "Title",
            "Author",
            "ArticleTitle",
            "ELocationID",
            "AbstractText",
            "Affiliation",
            "Language",
            "PublicationType",
            "MedlineTA",
            "NlmUniqueID",
            "ISSNLinking",
            "PublicationStatus",
            "ArticleId",
            "DOI",
            "Volume",
            "Issue",
            "ISOAbbreviation",
            "MedlinePgn",
            "CopyrightInformation",
            "Country",
            "GrantID",
            "COIStatement",
            "Mesh",
            "Keywords",
            "Citations")
  
  Fields <- vector(mode = "list", length = length(Tags))
  names(Fields) <- Tags
  
  
  Fields[["PMID"]] <- x$MedlineCitation$PMID[[1]]
  
  Fields[["YearRevised"]] <- x$MedlineCitation$DateRevised$Year[[1]]
  Fields[["MonthRevised"]] <- x$MedlineCitation$DateRevised$Month[[1]]
  Fields[["DayRevised"]] <- x$MedlineCitation$DateRevised$Day[[1]]
  Fields[["YearPubDate"]] <- x$MedlineCitation$Article$Journal$JournalIssue$PubDate$Year[[1]]
  Fields[["MonthPubDate"]] <- x$MedlineCitation$Article$Journal$JournalIssue$PubDate$Month[[1]]
  Fields[["DayPubDate"]] <- x$MedlineCitation$Article$Journal$JournalIssue$PubDate$Day[[1]]
  Fields[["YearArticleDate"]] <- x$MedlineCitation$Article$ArticleDate$Year[[1]]
  Fields[["MonthArticleDate"]] <- x$MedlineCitation$Article$ArticleDate$Month[[1]]
  Fields[["DayArticleDate"]] <- x$MedlineCitation$Article$ArticleDate$Day[[1]]
  
  pubmed_states <- sapply(x$PubmedData$History, function(z) attr(z, "PubStatus"))
  pubmed_states <- unique(pubmed_states) # I ADDED THIS LINE SO THAT IT WORKED
  
  if(length(pubmed_states) > 0){
    if(any(pubmed_states == "entrez")){
      i <- which(pubmed_states == "entrez")
      Fields[["YearEntrez"]] <- x$PubmedData$History[[i]]$Year[[1]]
      Fields[["MonthEntrez"]] <- x$PubmedData$History[[i]]$Month[[1]]
      Fields[["DayEntrez"]] <- x$PubmedData$History[[i]]$Day[[1]]
      Fields[["MinuteEntrez"]] <- x$PubmedData$History[[i]]$Minute[[1]]
      Fields[["HourEntrez"]] <- x$PubmedData$History[[i]]$Hour[[1]]
    }
    
    if(any(pubmed_states == "medline")){
      i <- which(pubmed_states == "medline")
      Fields[["YearMedline"]] <- x$PubmedData$History[[i]]$Year[[1]]
      Fields[["MonthMedline"]] <- x$PubmedData$History[[i]]$Month[[1]]
      Fields[["DayMedline"]] <- x$PubmedData$History[[i]]$Day[[1]]
      Fields[["MinuteMedline"]] <- x$PubmedData$History[[i]]$Minute[[1]]
      Fields[["HourMedline"]] <- x$PubmedData$History[[i]]$Hour[[1]]
    }	
    
    if(any(pubmed_states == "accepted")){
      i <- which(pubmed_states == "accepted")
      Fields[["YearAccepted"]] <- x$PubmedData$History[[i]]$Year[[1]]
      Fields[["MonthAccepted"]] <- x$PubmedData$History[[i]]$Month[[1]]
      Fields[["DayAccepted"]] <- x$PubmedData$History[[i]]$Day[[1]]
      Fields[["MinuteAccepted"]] <- x$PubmedData$History[[i]]$Minute[[1]]
      Fields[["HourAccepted"]] <- x$PubmedData$History[[i]]$Hour[[1]]
    }	
    
    if(any(pubmed_states == "received")){
      i <- which(pubmed_states == "received")
      Fields[["YearReceived"]] <- x$PubmedData$History[[i]]$Year[[1]]
      Fields[["MonthReceived"]] <- x$PubmedData$History[[i]]$Month[[1]]
      Fields[["DayReceived"]] <- x$PubmedData$History[[i]]$Day[[1]]
      Fields[["MinuteReceived"]] <- x$PubmedData$History[[i]]$Minute[[1]]
      Fields[["HourReceived"]] <- x$PubmedData$History[[i]]$Hour[[1]]
    }	
    
    if(any(pubmed_states == "epublish")){
      i <- which(pubmed_states == "epublish")
      Fields[["YearEpublish"]] <- x$PubmedData$History[[i]]$Year[[1]]
      Fields[["MonthEpublish"]] <- x$PubmedData$History[[i]]$Month[[1]]
      Fields[["DayEpublish"]] <- x$PubmedData$History[[i]]$Day[[1]]
      Fields[["MinuteEpublish"]] <- x$PubmedData$History[[i]]$Minute[[1]]
      Fields[["HourEpublish"]] <- x$PubmedData$History[[i]]$Hour[[1]]
    }			
    
    if(any(pubmed_states == "ppublish")){
      i <- which(pubmed_states == "ppublish")
      Fields[["YearPpublish"]] <- x$PubmedData$History[[i]]$Year[[1]]
      Fields[["MonthPpublish"]] <- x$PubmedData$History[[i]]$Month[[1]]
      Fields[["DayPpublish"]] <- x$PubmedData$History[[i]]$Day[[1]]
      Fields[["MinutePpublish"]] <- x$PubmedData$History[[i]]$Minute[[1]]
      Fields[["HourPpublish"]] <- x$PubmedData$History[[i]]$Hour[[1]]
    }	
    
    if(any(pubmed_states == "pmc")){
      i <- which(pubmed_states == "pmc")
      Fields[["YearPmc"]] <- x$PubmedData$History[[i]]$Year[[1]]
      Fields[["MonthPmc"]] <- x$PubmedData$History[[i]]$Month[[1]]
      Fields[["DayPmc"]] <- x$PubmedData$History[[i]]$Day[[1]]
      Fields[["MinutePmc"]] <- x$PubmedData$History[[i]]$Minute[[1]]
      Fields[["HourPmc"]] <- x$PubmedData$History[[i]]$Hour[[1]]
    }		
    
    if(any(pubmed_states == "pubmed")){
      i <- which(pubmed_states == "pubmed")
      Fields[["YearPubmed"]] <- x$PubmedData$History[[i]]$Year[[1]]
      Fields[["MonthPubmed"]] <- x$PubmedData$History[[i]]$Month[[1]]
      Fields[["DayPubmed"]] <- x$PubmedData$History[[i]]$Day[[1]]
      Fields[["MinutePubmed"]] <- x$PubmedData$History[[i]]$Minute[[1]]
      Fields[["HourPubmed"]] <- x$PubmedData$History[[i]]$Hour[[1]]
    }
  }
  
  Fields[["ISSN"]] <-  x$MedlineCitation$Article$Journal$JournalIssue$ISSN[[1]]
  Fields[["Volume"]] <-  x$MedlineCitation$Article$Journal$JournalIssue$Volume[[1]]
  Fields[["Issue"]] <-  x$MedlineCitation$Article$Journal$JournalIssue$Issue[[1]]
  Fields[["Title"]] <-  x$MedlineCitation$Article$Journal$JournalIssue$Title[[1]]
  
  if(!is.null(x$MedlineCitation$Article$ArticleTitle))
    Fields[["ArticleTitle"]] <-  paste(unlist(x$MedlineCitation$Article[names(x$MedlineCitation$Article) == "ArticleTitle"]), collapse = "")
  
  if(!is.null(x$MedlineCitation$Article$AuthorList)){
    
    Authors <- do.call("rbind", lapply(x$MedlineCitation$Article$AuthorList, function(z){
      
      if(any(names(z) == "CollectiveName") | (is.null(z$ForeName) & is.null(z$Initials))){
        data.frame(
          CollectiveName = ifelse(is.null(z$CollectiveName), as.character(z$LastName[[1]]), as.character(z$CollectiveName[[1]])),
          LastName = NA,
          ForeName = NA,
          Initials = NA,
          stringsAsFactors=FALSE
        )
      }
      else{
        data.frame(
          CollectiveName = NA,
          LastName = ifelse(is.null(z$LastName[[1]]), NA, as.character(z$LastName[[1]])),
          ForeName = ifelse(is.null(z$ForeName[[1]]), NA, as.character(z$ForeName[[1]])),
          Initials = ifelse(is.null(z$Initials[[1]]), NA, as.character(z$Initials[[1]])),
          stringsAsFactors=FALSE
        )			
      }
    }))
    
    Authors$order <- 1:nrow(Authors)
    
    Fields[["Author"]] <- Authors
    
    Affiliations <- lapply(x$MedlineCitation$Article$AuthorList, function(z){
      z$AffiliationInfo$Affiliation[[1]]
    })
    
    names(Affiliations) <- 1:length(Affiliations)
    
    Fields[["Affiliation"]] <- unlist(Affiliations)
  }
  
  elocations <- names(x$MedlineCitation$Article)
  
  if(any(elocations == "ELocationID")){
    
    elocations <- x$MedlineCitation$Article[which(elocations == "ELocationID")]
    
    for(e in elocations){
      if(attr(e, "EIdType") == "pii")
        Fields[["ELocationID"]] <- e$ELocationID[[1]]
      
      if(attr(e, "EIdType") == "doi")
        Fields[["DOI"]] <- e$ELocationID[[1]]			
    }	
  }
  
  if(!is.null(x$MedlineCitation$Article$Abstract)){
    AbstractText <- paste(unlist(x$MedlineCitation$Article$Abstract[names(x$MedlineCitation$Article$Abstract) == "AbstractText"]), collapse = " ")
    
    Fields[["AbstractText"]] <- AbstractText
  }
  
  Fields[["Language"]] <- x$MedlineCitation$Article$Language[[1]]
  Fields[["PublicationType"]] <- x$MedlineCitation$Article$PublicationTypeList$PublicationType[[1]]
  Fields[["Country"]] <- x$MedlineCitation$Article$Country[[1]]
  Fields[["MedlineTA"]] <- x$MedlineCitation$Article$MedlineTA[[1]]
  Fields[["NlmUniqueID"]] <- x$MedlineCitation$Article$NlmUniqueID[[1]]
  Fields[["ISSNLinking"]] <- x$MedlineCitation$Article$ISSNLinking[[1]]
  Fields[["PublicationStatus"]] <- x$PubmedData$PublicationStatus[[1]]
  Fields[["ArticleId"]] <- x$PubmedData$ArticleIdList$ArticleId[[1]]
  Fields[["ISOAbbreviation"]] <- x$MedlineCitation$Article$Journal$ISOAbbreviation[[1]]
  Fields[["MedlinePgn"]] <- x$MedlineCitation$Article$Pagination$MedlinePgn[[1]]
  Fields[["CopyrightInformation"]] <- x$MedlineCitation$Article$Abstract$CopyrightInformation[[1]]
  
  
  if(!is.null(x$MedlineCitation$Article$GrantList)){
    
    Grants <- do.call("rbind", lapply(x$MedlineCitation$Article$GrantList, function(z){
      data.frame(
        GrantID = ifelse(is.null(z$GrantID[[1]]), NA, z$GrantID[[1]]),
        Agency =  ifelse(is.null(z$Agency[[1]]), NA, z$Agency[[1]]),
        stringsAsFactors = F
      )	
    }))
    
    Fields[["GrantID"]] <- Grants
  }
  
  Fields[["COIStatement"]] <- x$MedlineCitation$CoiStatement[[1]]
  
  if(!is.null(x$MedlineCitation$KeywordList)){
    keywords <- x$MedlineCitation$KeywordList[names(x$MedlineCitation$KeywordList) == "Keyword"]
    Fields[["Keywords"]] <- unlist(keywords)
  }
  
  
  if(!is.null(x$MedlineCitation$MeshHeadingList$MeshHeading)){
    Mesh <- data.frame(
      Heading = unlist(x$MedlineCitation$MeshHeadingList),
      Type = ifelse(grepl("DescriptorName", names(unlist(x$MedlineCitation$MeshHeadingList))),"Descriptor","Qualifier"),
      stringsAsFactors = F
    )
    
    Fields[["Mesh"]] <- Mesh
  }
  
  
  if(!is.null(x$PubmedData$ReferenceList)){
    References <- lapply(x$PubmedData$ReferenceList, function(z){
      paste(unlist(z), collapse = "")
    })
    
    Fields[["Citations"]] <- References
  } 
  
  Fields
}




# MAKING RNCBI PACKAGE
collapse <- function(...){paste(...,sep="",collapse="")}

EUtilsURL <- function(type="esearch",db="pubmed"){
  
  # CONSTRUCT ANY SERVICE TYPE AND DATABASE
  url <- 	"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/type.fcgi?db=DB&"
  
  sub("(.*)(type)(.*)(DB)(.*)",collapse("\\1",type,"\\3",db,"\\5"),url)
}


EUtilsQuery <- function(query,type="esearch",db="pubmed",...){
  
  # CREATE URL WITH REQUEST
  PubMedURL <- EUtilsURL(type,db)
  Query <- gsub(" ","+",query)
  
  # PARTIAL MATCH OF POSSIBLE LIMITS
  OPTIONS <- c(
    "retstart",
    "retmax",
    "rettype",
    "field",
    "datetype",
    "reldate",
    "mindate",
    "maxdate"
  )
  
  ArgList <- list(...) # NEED FUNCTION PARTIAL MATCH HERE
  
  if(length(ArgList)==0){
    ArgList$retmax <- 1000 # DEFAULT 1000 RECORDS RETURNED
  }
  else{
    WhichArgs <- pmatch(names(ArgList),OPTIONS)	
    if(any(is.na(WhichArgs))||sapply(WhichArgs,length)>1)
      stop("Error in specified limits.")
    names(ArgList) <- OPTIONS[WhichArgs]
    if(all(names(ArgList)!="retmax"))
      ArgList$retmax <- 1000 # DEFAULT 1000 RECORDS RETURNED
  }
  
  ArgList$tool <- "RISmed"
  ArgList$email <- "s.a.kovalchik@gmail.com"
  
  # REPLACE RETMAX IF NOT USED
  ArgStr <- paste(names(ArgList),unlist(ArgList),sep="=")
  ArgStr <- paste(ArgStr,collapse="&")
  
  paste(PubMedURL,"term=",Query,"&",ArgStr,sep="",collapse="")	
}

ParseTags <- function(lines){
  
  StripLines <- sapply(lines, function(x)gsub("> +",">",x)) # REPLACE WHITE SPACE
  
  Fields <- c("Count",
              "RetMax",
              "RetStart",
              "Id",
              "QueryTranslation")
  
  Patterns <- paste("(.*<",Fields,">)(.*)(<\\/",Fields,">).*",sep="")
  
  FieldIndex <- lapply(Patterns, function(pattern) grep(pattern, StripLines))
  FieldIndex[[1]] <- FieldIndex[[1]][1] # TAKE COUNT OF COMBINED QUERY
  
  Values <- lapply(1:length(Fields),
                   function(i){
                     result <- sapply(StripLines[FieldIndex[[i]]],
                                      function(x) sub(Patterns[i],"\\2",x),USE.NAMES=FALSE)
                     as.vector(result)
                   })
  
  
  names(Values) <- Fields
  Values$Count <- as.numeric(Values$Count)
  Values$RetMax <- as.numeric(Values$RetMax)
  Values$RetStart <- as.numeric(Values$RetStart)
  
  Values
}


SplitIDs <- function(ids){
  
  if(length(ids)>200){
    group <- rep(1:ceiling(length(ids)/200),each=200)
    group <- group[1:length(ids)]
    split(ids,group)
  }
  else{
    list(ids)
  }
  
}


EUtilsGet <- function(x, type="efetch", db="pubmed"){
  
  if(class(x)[1]=="EUtilsSummary"){
    query <- x@querytranslation
    x <- x@PMID
  }
  else{
    query <- ""
  }
  
  IDList <- SplitIDs(x)
  Result <- lapply(IDList, EUtilsSubGet, type = type, db = db)
  Result <- unlist(Result, recursive=FALSE)
  
  if(type=="efetch"&db=="pubmed"){
    
    Result <- Medline(Result, query)
  }
  
  Result
}


EUtilsSubGet <- function(ids, type="efetch", db="pubmed"){
  
  FetchURL <- EUtilsURL(type,db=db)
  IDStr <- collapse("id=",paste(ids,collapse=","))
  EUtilsFetch <- collapse(FetchURL,IDStr)	
  
  res <- readLines(collapse(EUtilsFetch,"&retmode=xml"), warn = FALSE, encoding = "UTF-8")	
  res <- readLines(collapse(EUtilsFetch,"&retmode=xml"), warn = FALSE, encoding = "UTF-8")
  res <- xml2::read_xml(collapse(EUtilsFetch,"&retmode=xml"), encoding = "UTF-8")	
  res <- xml2::as_list(res)[[1]]
  
  if(db == "pubmed"){
    res <- lapply(res, ParseMedline) # Return list for each artcile, missing elements are NULL
  }
  
  res
}





