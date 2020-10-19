library(RSelenium)
library(dplyr)
library(sentimentr)
library(rvest)
library(xml2)
library(XML)
library(stringr)



####################### FUNCTIONS NEEDED TO SCRAPE ######################################

##### TO GET TOTAL PAGES PER CATEGORY #####
TotalProductPages <- function(){
  productnumbers <- rd$findElement(using = 'css', value = ".css-fgy0ne span")
  productnumbers <- productnumbers$getElementText()
  productnumbers <- unlist(productnumbers)
  findindexof <- str_locate_all(pattern =" product",productnumbers)
  startindex <- unlist(findindexof)[1]
  productnumber <- as.integer(substr(productnumbers,1,startindex-1))
  pages <- ceiling(productnumber/60)
  
  return(pages)
}
  

#### TO GET INDIVIDUAL PRODUCT BRAND NAME ####

GetBrandName <- function(){
  if(length(rd$findElements(using = "css", value = '.css-euydo4')) != 0){
  brand <- rd$findElement(using = 'css', value = '.css-euydo4')
  brand <- brand$getElementText()
  brand <- brand[[1]]
  
  return(brand)}
  else{
    return(NA)
  }
  }

#### TO GET INDIVIDUAL PRODUCT NAME ####

GetProductName <- function(){
  if(length(rd$findElements(using = "css", value = ".css-140z8k4 .css-0")) != 0){
  productname <- rd$findElement(using = "css", value = ".css-140z8k4 .css-0")
  productname <- productname$getElementText()
  productname <- productname[[1]]
  
  return(productname)}
  else{
    return(NA)
  }
}

#### TO GET INDIVIDUAL PRODUCT PRICE ####

GetPrice <- function(){
  if(length(rd$findElements(using = "css", value = ".css-slwsq8 span")) != 0){
  price <- rd$findElement(using = "css", value = ".css-slwsq8 span")
  price <- price$getElementText()
  price <- price[[1]]
  
  return(price)}
  else{
    return(NA)
  }
}


#### TO GET INDIVIDUAL PRODUCT DESCRIPTION ####

GetDesc <- function(){
  if(length(rd$findElements(using = "css", value = ".css-pz80c5")) != 0){
  desc <- rd$findElement(using = "css", value = ".css-pz80c5")
  desc <- desc$getElementText()
  desc <- desc[[1]]
  
  return(desc)}
  else{
    return (NA)
  }
}


#### TO CLOSE ADS WHEN IT POPS UP ####
CloseAdIfHave <- function(){
  if(length(rd$findElements(using = "css", value = ".css-1xg1q2j")) != 0){
  ads <- rd$findElement(using = 'css', value = '.css-1xg1q2j')
  ads$clickElement()
  }
}

#### SCROLL DOWN BY X TIMES FUNCTION ####
ScrollDownXTimes <- function(x){ 
webElem <- rd$findElement("css", "body")
replicate(x,webElem$sendKeysToElement(list(key = "down_arrow")))
}


#### RETURNS A LIST OF ALL THE PRODUCT ELEMENTS IN A PAGE ####
GetProductElements <- function(){
  products <- rd$findElements(using = 'css', value = '.css-12egk0t')
  
  return(products)
}


#### CLICK INTO ELEMENT i IN THE PAGE ####
GoIntoElementi <- function(products,i){
  products[[i]]$clickElement()
  }



#### TO CLICK INTO THE PRODUCT AGAIN AS SEPHORA NEEDS TO CLICK TWICE (WHEN CLICKING WITH THE PREVIOUS FUNCTION's ELEMENT) ####
ClickIntoProductAgainIfNeeded <- function(){
  if(length(rd$findElements(using = "css", value = ".css-bt729o")) != 0) {
    link <- rd$findElement(using = "css", value = ".css-bt729o")
    link$clickElement()
  }
}



#### EXPAND PRODUCT REVIEWS BY 6 EACH TIME IT CLICKS, I HAVE SET IT TO EXPAND 75 TIMES (MAX OF 75*6 = 450 REVIEWS PER PRODUCT) AS IT WILL TAKE TOO LONG TO OPEN ALL THOUSANDS OF REVIEWS ####
ExpandReviews <- function(){
  for(i in 1:75){
    CloseAdIfHave()
    CloseAdIfHave()
  if(length(rd$findElements(using = "css", value = ".css-frqcui")) != 0){
      clickreview <- rd$findElement(using = "css", value = ".css-frqcui")
      clickreview$clickElement()}
  else
  {break}
  }
}


#### SCRAPE ALL REVIEWS THAT ARE OPENED IN EACH PRODUCT ####
ScrapeReviewsDF <- function(){
  reviews <- c()
  if(length(rd$findElements(using = "css", value = ".css-7rv8g1")) != 0){
    reviewelements <- rd$findElements(using = 'css', value = '.css-7rv8g1')
    for(i in reviewelements){
      review <- i$getElementText()
      reviews <- c(reviews,review)}
  
  reviews <- unlist(reviews)
  df <- data.frame(matrix(reviews,ncol=1)) 
  colnames(df) <- "Reviews"
  df$Reviews <- as.character(df$Reviews)
  sentiments <- sentiment_by(df$Reviews)
  df <- cbind(df,sentiments$ave_sentiment)
  colnames(df) <- c("Reviews","Sentiment Score")
  return(df)}
  else {
    reviews <- matrix(c(NA,0),nrow=1,ncol=2)
    df <- data.frame(reviews)
    colnames(df) <- c("Reviews","Sentiment Score")
    return(df)
  }
}



#### EXPORT THE REVIEWS(OR DATAFRAME) TO MY COMPUTER ####
ExportReviews <- function(df,productname,type){
  if (grepl("/",productname,fixed  = TRUE) == TRUE){
    productname <- gsub("/","",productname)
    write.csv(df,paste0("C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\",type,"\\",productname,".csv"), row.names = FALSE)
  } else if (grepl("\\",productname,fixed = TRUE) == TRUE) {
    productname <- gsub("\\","",productname)
    write.csv(df,paste0("C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\",type,"\\",productname,".csv"), row.names = FALSE)
  } else if (grepl(":",productname,fixed = TRUE) == TRUE) {
    productname <- gsub(":","",productname)
    write.csv(df,paste0("C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\",type,"\\",productname,".csv"), row.names = FALSE)
  } else if (grepl("*",productname,fixed = TRUE) == TRUE) {
    productname <- gsub("*","",productname)
    write.csv(df,paste0("C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\",type,"\\",productname,".csv"), row.names = FALSE)
  } else if (grepl("?",productname,fixed = TRUE) == TRUE) {
    productname <- gsub("?","",productname)
    write.csv(df,paste0("C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\",type,"\\",productname,".csv"), row.names = FALSE)
  } else if (grepl("\"",productname,fixed = TRUE) == TRUE) {
    productname <- gsub("\"","",productname)
    write.csv(df,paste0("C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\",type,"\\",productname,".csv"), row.names = FALSE)
  } else if (grepl("<",productname,fixed = TRUE) == TRUE) {
    productname <- gsub("<","",productname) 
    write.csv(df,paste0("C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\",type,"\\",productname,".csv"), row.names = FALSE)
  } else if (grepl(">",productname,fixed = TRUE) == TRUE) {
    productname <- gsub(">","",productname) 
    write.csv(df,paste0("C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\",type,"\\",productname,".csv"), row.names = FALSE)
  } else if (grepl("|",productname,fixed = TRUE) == TRUE) {
    productname <- gsub("|","",productname) 
    write.csv(df,paste0("C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\",type,"\\",productname,".csv"), row.names = FALSE)
  } else {
    write.csv(df,paste0("C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\",type,"\\",productname,".csv"), row.names = FALSE)
  }
}



###Steps to Get Product Reviews and Scores
###Function for one whole product
Stepsforoneproduct <- function (link,i) {
  rd$navigate(link)
  Sys.sleep(3)
  CloseAdIfHave() #Step 1. Close any ads that pop up when opening Sephora
  CloseAdIfHave()
  ScrollDownXTimes(130) #Step 2. Scroll Down to End of Page to Load all Elements (Sephora has Lazy Load)
  productelements <- GetProductElements() #Step 3. Get All Product Elements in The Page
  GoIntoElementi(productelements,1) #Step 4. Click Into Product
  Sys.sleep(3)
  ClickIntoProductAgainIfNeeded() #Step 5. Sometimes it doesn't go in so click again if the element still exists
  CloseAdIfHave()
  productname <- GetProductName() #Step 6. Get the Product Name
  brandname <- GetBrandName() #Step 7. Get the Brand Name
  ScrollDownXTimes(45) #Step 6. Scroll Down A Little to Load the Review Elements
  Sys.sleep(3)
  CloseAdIfHave()
  ExpandReviews() #Step 7. Expand all reviews in the page until no more to expand
  df <- ScrapeReviewsDF() #Step 8. Get all the Reviews with Sentiment Scores
  df <- cbind(productname,brandname,df)
  ExportReviews(df,productname,type = "Treatments") #Step 9. Export into a CSV
  meansentiments <- mean(df$`Sentiment Score`)  #Step 10. Add it into the total df
  
  return(c(productname,brandname,meansentiments))
}

###Automating
GetReviewsAllProductsPerPage <- function(link){
  bigdf <- data.frame(matrix(ncol=3))
  colnames(bigdf) <- c("Brand", "Product","Avg Sentiment Score")
  CloseAdIfHave()
  CloseAdIfHave()                                     #Close Ad
  ScrollDownXTimes(130)                               #Scroll Down all the way
  productelements <- GetProductElements()             #Total product numbers in the page
  for(i in 1:length(productelements)){                #Iterate to get review for each product
  vector <- Stepsforoneproduct(link,i)
  bigdf <- rbind(bigdf,vector)
  }
  return(bigdf)
}


###################End of functions needed######################

###################Start to scrape#######################

###Start Server (always start server before performing any scraping) ###
rD1 <- rsDriver(browser = "chrome", port = 8581L, geckover = NULL, 
                chromever =  "80.0.3987.106", iedrver = NULL, 
                phantomver = NULL)

###Assign Server to rd
rd <- rD1[["client"]]
###########################

############### Run this portion to get Reviews###############
rd$navigate("https://www.sephora.com/shop/eye-treatment-dark-circle-treatment")
pages <- TotalProductPages()
totaldf <- data.frame(matrix(ncol=3))
colnames(totaldf) <- c("Product", "Price","Description")


for(page in 1:pages){
  link <- paste0("https://www.sephora.com/shop/eye-treatment-dark-circle-treatment?currentPage=",page)
  rd$navigate(link)
  Sys.sleep(3)
  bigdf <- GetReviewsAllProductsPerPage(link)
}
#############################################################





###############Price and Description##################
#### EXTRA FUNCTIONS NEEDED FOR PRICE AND DESCRIPTION ####


#### FUNCTION TO GET 1 PRODUCT NAME, PRICE AND DESCRIPTION AND TO EXPORT IT INTO A CSV FILE ####
ExportPriceandDesc <- function (link,i,newtype) {
  rd$navigate(link)
  Sys.sleep(2)
  CloseAdIfHave() #Step 1. Close any ads that pop up when opening Sephora
  CloseAdIfHave()
  ScrollDownXTimes(130) #Step 2. Scroll Down to End of Page to Load all Elements (Sephora has Lazy Load)
  productelements <- GetProductElements() #Step 3. Get All Product Elements in The Page
  GoIntoElementi(productelements,i) #Step 4. Click Into Product
  Sys.sleep(3)
  ClickIntoProductAgainIfNeeded() #Step 5. Sometimes it doesn't go in so click again if the element still exists
  CloseAdIfHave()
  Sys.sleep(2)
  productname <- GetProductName() #Step 6. Get the Product Name
  price <- GetPrice()
  desc <- GetDesc()
  CloseAdIfHave()
  df <- cbind(productname,price,desc)
  ExportReviews(df,productname,newtype) #Step 9. Export into a CSV
}

#### FUNCTION TO AUTOMATE THE SCRAPING PROCESS BY LOOPING INTO EACH ELEMENT IN A PAGE####
GetAllPriceandDesc <- function(link,newtype){
  CloseAdIfHave()
  CloseAdIfHave()                                     #Close Ad
  ScrollDownXTimes(130)                               #Scroll Down all the way
  productelements <- GetProductElements()             #Total product numbers in the page
  for(i in 1:length(productelements)){                #Iterate to get review for each product
    ExportPriceandDesc(link,i,newtype)
    
  }
}

#### FUNCTION TO LOOP EVERY PRODUCT IN EVERY PAGE ####
EveryPriceandDesc <- function(link) {
  linkindex <- which(link == links)
  newtype <- type[linkindex]
  rd$navigate(paste0(link,1))
  pages <- TotalProductPages()
  for(page in 12:12){
    link1 <- paste0(link,page)
    rd$navigate(link1)
    Sys.sleep(3)
    GetAllPriceandDesc(link1,newtype)
    
  }
}


############### Run this Portion to get all Prices and Desc ###########################
type <- c("FaceMasks","Cleansers","FacialTreatments","Moisturizers")
links <- c("https://www.sephora.com/shop/face-mask?currentPage=",
           "https://www.sephora.com/shop/cleanser?currentPage=",
           "https://www.sephora.com/shop/facial-treatments?currentPage=",
           "https://www.sephora.com/shop/moisturizing-cream-oils-mists?currentPage=")


for(link in links){
 EveryPriceandDesc(link)
}
###################################################################################


##################Cleaning up Data#####################


############ Function to clean the data to group product by product & brand name then get mean sentiment score for each product #########
Averaging <- function(df){
  data1 <- df %>% group_by(`Product Name`,`Brand Name`) %>% summarise("Mean Sentiment Score" = round(mean(`Sentiment Score`),2),"Number of Reviews" = n())
  data1[is.na(data1)] <- 0
  
  return(data1)
}


#Following steps are to average the df then merge it with the df with Product Name, Price & Product Description


##Reimporting all csvs I need
library(readxl)
library(writexl)

setwd("C:/Users/ChengYao/Desktop/DBA project/Sephora")

CleaningandCombining <- function(type){
  df <- read_excel(paste0("Price ",type,".xlsx"))
  df1 <- read_excel(paste0("Total ",type,".xlsx"))
  newdf <- Averaging(df1)
  newdf1 <- merge(newdf,df,by = "Product Name", all = TRUE)
  
  return(newdf1)
}


types <- c("Cleansers", "Eye Treatments", "Face Masks", "Facial Treatments", "Moisturisers")

######### CLEANING AND EXPORTING CLEANED DATAFRAME WHICH WILL CONSIST OF PRODUCTNAME,BRANDNAME,MEANSENTIMENTSCORE,PRICE,DESCRIPTION #########


CleansersDF <- CleaningandCombining(types[1])
CleansersDF <- CleansersDF[-1,]
CleansersDF <- CleansersDF[-(which(is.na(CleansersDF$`Product Name`))),]

EyeTreatmentsDF <- CleaningandCombining(types[2])
EyeTreatmentsDF <- EyeTreatmentsDF[-1,]
EyeTreatmentsDF <- EyeTreatmentsDF[-(which(is.na(EyeTreatmentsDF$`Product Name`))),]


FaceMasksDF <- CleaningandCombining(types[3])
FaceMasksDF <- FaceMasksDF[-3,]
FaceMasksDF <- FaceMasksDF[-(which(is.na(FaceMasksDF$`Product Name`))),]

FacialTreatmentsDF <- CleaningandCombining(types[4])
FacialTreatmentsDF <- FacialTreatmentsDF[-5,]
FacialTreatmentsDF <- FacialTreatmentsDF[-(which(is.na(FacialTreatmentsDF$`Product Name`))),]

MoisturisersDF <- CleaningandCombining(types[5])
MoisturisersDF <- MoisturisersDF[-4,]
MoisturisersDF <- MoisturisersDF[-(which(is.na(MoisturisersDF$`Product Name`))),]

#######  Converting the Mean Sentiment Scores from range of -1 to 1 into a range of 0 to 5 ########

ConvertScores <- function(df){
  # Formula for new value = (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin
  df$`Assigned Score` <- NA
  df$`Assigned Score` <- ((df$`Mean Sentiment Score` - (-1)) * (5 - 0)) / ((1 - (-1)) + 0)
  df$`Assigned Score`[df$`Number of Reviews` == 0] <- NA
  
  return (df)

}


CleansersDF <- ConvertScores(CleansersDF)
EyeTreatmentsDF <- ConvertScores(EyeTreatmentsDF)
FaceMasksDF <- ConvertScores(FaceMasksDF)
FacialTreatmentsDF <- ConvertScores(FacialTreatmentsDF)
MoisturisersDF <- ConvertScores(MoisturisersDF)

######################################################################

##### Assigning our own TRUE score which consists of 70% weightage of number of reviews and 30% of the Assigned Score because we believe that number of reviews should be a higher weightage than the score as scores can be skewed if the number of reviews are low ####

TrueScores <- function(df){
  df$`True Score` <- NA
  df$`True Score` <- (0.70 * (df$`Number of Reviews`)) + (0.30 * (df$`Assigned Score`)/5)
  df[is.na(df)] <- "NA"
return (df)
}

CleansersDF <- TrueScores(CleansersDF)
EyeTreatmentsDF <- TrueScores(EyeTreatmentsDF)
FaceMasksDF <- TrueScores(FaceMasksDF)
FacialTreatmentsDF <- TrueScores(FacialTreatmentsDF)
MoisturisersDF <- TrueScores(MoisturisersDF)

######################################################################

### Export the final DF out ####


CleansersDF <- CleansersDF[,c(1,2,3,4,7,8,5,6)]
EyeTreatmentsDF <- EyeTreatmentsDF[,c(1,2,3,4,7,8,5,6)]
FaceMasksDF <- FaceMasksDF[,c(1,2,3,4,7,8,5,6)]
FacialTreatmentsDF <- FacialTreatmentsDF[,c(1,2,3,4,7,8,5,6)]
MoisturisersDF <- MoisturisersDF[,c(1,2,3,4,7,8,5,6)]



write_xlsx(MoisturisersDF, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\MoisturisersFinalDF.xlsx", col_names = TRUE)
write_xlsx(FacialTreatmentsDF, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\FacialTreatmentsFinalDF.xlsx", col_names = TRUE)
write_xlsx(FaceMasksDF, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\FaceMasksFinalDF.xlsx", col_names = TRUE)
write_xlsx(EyeTreatmentsDF, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\EyeTreatmentsFinalDF.xlsx", col_names = TRUE)
write_xlsx(CleansersDF, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\CleansersFinalDF.xlsx", col_names = TRUE)


############# RE - CLEANING FILES ###############

Total_Cleansers <- read_excel("Total Cleansers.xlsx")
Total_Eye_Treatments <- read_excel("Total Eye Treatments.xlsx")
Total_Face_Masks <- read_excel("Total Face Masks.xlsx")
Total_Facial_Treatments <- read_excel("Total Facial Treatments.xlsx")
Total_Moisturisers <- read_excel("Total Moisturisers.xlsx")

Total_Cleansers[is.na(Total_Cleansers)] <- "NA"
Total_Eye_Treatments[is.na(Total_Eye_Treatments)] <- "NA"
Total_Face_Masks[is.na(Total_Face_Masks)] <- "NA"
Total_Facial_Treatments[is.na(Total_Facial_Treatments)] <- "NA"
Total_Moisturisers[is.na(Total_Moisturisers)] <- "NA"

write_xlsx(Total_Cleansers, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\Total Cleansers.xlsx", col_names = TRUE)
write_xlsx(Total_Eye_Treatments, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\Total Eye Treatments.xlsx", col_names = TRUE)
write_xlsx(Total_Face_Masks, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\Total Face Masks.xlsx", col_names = TRUE)
write_xlsx(Total_Facial_Treatments, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\Total Facial Treatments.xlsx", col_names = TRUE)
write_xlsx(Total_Moisturisers, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\Total Moisturisers.xlsx", col_names = TRUE)


Price_Cleansers <- read_excel("Price Cleansers.xlsx")
Price_Eye_Treatments <- read_excel("Price Eye Treatments.xlsx")
Price_Face_Masks <- read_excel("Price Face Masks.xlsx")
Price_Facial_Treatments <- read_excel("Price Facial Treatments.xlsx")
Price_Moisturisers <- read_excel("Price Moisturisers.xlsx")

Price_Cleansers[is.na(Price_Cleansers)] <- "NA"
Price_Eye_Treatments[is.na(Price_Eye_Treatments)] <- "NA"
Price_Face_Masks[is.na(Price_Face_Masks)] <- "NA"
Price_Facial_Treatments[is.na(Price_Facial_Treatments)] <- "NA"
Price_Moisturisers[is.na(Price_Moisturisers)] <- "NA"

write_xlsx(Price_Cleansers, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\Price Cleansers.xlsx", col_names = TRUE)
write_xlsx(Price_Eye_Treatments, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\Price Eye Treatments.xlsx", col_names = TRUE)
write_xlsx(Price_Face_Masks, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\Price Face Masks.xlsx", col_names = TRUE)
write_xlsx(Price_Facial_Treatments, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\Price Facial Treatments.xlsx", col_names = TRUE)
write_xlsx(Price_Moisturisers, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\Price Moisturisers.xlsx", col_names = TRUE)



############### CONVERTING THE ALREADY DONE ALL REVIEW FILES SCORES ################

setwd("C:/Users/ChengYao/Desktop/DBA project/Sephora/All Products Reviews Data")


scleansers<- read_excel("Sehpora All Cleansers.xlsx")
setreat <- read_excel("Sephora Eye Treatments.xlsx")
sfmask <- read_excel("Sephora Face Masks.xlsx")
sfacetreat <- read_excel("Sephora Facial Treatments.xlsx")
smoist <- read_excel("Sephora Moisturisers.xlsx")

ConvertScores2 <- function(df){
  # Formula for new value = (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin
  df$`Sentiment Score` <- as.numeric(df$`Sentiment Score`)
  df$`Sentiment Score` <- round(((df$`Sentiment Score` - (-1)) * (5 - 0)) / ((1 - (-1)) + 0),2)
  df$`Sentiment Score` <- as.character(df$`Sentiment Score`)
  
  
  return (df)
  
}



scleansersnew <- ConvertScores2(scleansers)
scleansersnew <- scleansersnew[!is.na(scleansersnew$`Sentiment Score`),]

setreatnew <- ConvertScores2(setreat)
setreatnew <- setreatnew[!is.na(setreatnew$`Sentiment Score`),]

sfmasknew <- ConvertScores2(sfmask)
sfmasknew <- sfmasknew[!is.na(sfmasknew$`Sentiment Score`),]

sfacetreatnew <- ConvertScores2(sfacetreat)
sfacetreatnew <- sfacetreatnew[!is.na(sfacetreatnew$`Sentiment Score`),]

smoistnew <- ConvertScores2(smoist)
smoistnew <- smoistnew[!is.na(smoistnew$`Sentiment Score`),]

library(writexl)

write_xlsx(scleansersnew, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\FINALCLEANSERSREVIEWS.xlsx", col_names = TRUE)
write_xlsx(setreatnew, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\FINALEYETREATMENTREVIEWS.xlsx", col_names = TRUE)
write_xlsx(sfmasknew, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\FINALFACEMASKREVIEWS.xlsx", col_names = TRUE)
write_xlsx(sfacetreatnew, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\FINALFACETREATMENTREVIEWS.xlsx", col_names = TRUE)
write_xlsx(smoistnew, "C:\\Users\\ChengYao\\Desktop\\DBA project\\Sephora\\FINALMOISTURISERREVIEWS.xlsx", col_names = TRUE)


###################### END OF SCRAPING AND CLEANING FOR SEPHORA ############################




