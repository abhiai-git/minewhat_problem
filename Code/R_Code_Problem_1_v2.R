# # Author : Abhishek Bhardwaj
# # 
# Problem - 1 

#   Identify categories of products based on associated descriptions and categories of similar products. 

#   => input , csv of products with description fields and  category column having uncategorized.
#            e.g PID, category, name, description
#             "Pid1" ,  ["shoes"],    Nike Emerge 3, Running shoes with Jogging experience ...
# #             "Pid2" ,  ["uncategorized"] ,  Puma Mens Axis, Fitness companion for Jogging ...

  
# Problem - 2
#   Filling missing purchase counts for the products. 

#   => input , csv of product id with column of purchases by month from Aug till Nov. There will be missing columns for these purchase numbers. You Need to fill the missing values for columns “purchases_Y15/M08” and “purchases_Y15/M09”. Note: 0 is valid value not an empty cell.
#    e.g Product id, aug, sep , oct
#             "Pid1" , 25, 12, 13
#             "Pid2" ,   , 23, 45
#             "Pid3" ,  65 ,  , 99
#   => output , csv of products with missing date values column filled. 

# Hint: Use Regression techniques. Evaluation will be based on the % of closest match in values in original data set. Use Libraries rather coding basic algorithms.




library(tm)

data<-read.csv("D:/Personal/minewhat_problem/Data/products.csv",header=T,stringsAsFactors = F)

keyword<-function(key1){
  
  corpus.tmp=Corpus(VectorSource(key1))
  corpus.tmp<- tm_map(corpus.tmp,removePunctuation)
  corpus.tmp<- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp<- tm_map(corpus.tmp, tolower)
  corpus.tmp<- tm_map(corpus.tmp, removeWords, stopwords("english"))
  corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument)
  dtm <- TermDocumentMatrix(corpus.tmp)
  temp <- inspect(dtm)
  FreqMat <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
  row.names(FreqMat) <- NULL
  return(FreqMat[FreqMat$Freq==max(FreqMat$Freq),1])
}



kmeans_clutering<-function(data, cluster){ 
corpus.tmp<-Corpus(VectorSource(data))
corpus.tmp<- tm_map(corpus.tmp,removePunctuation)
corpus.tmp<- tm_map(corpus.tmp, stripWhitespace)
corpus.tmp<- tm_map(corpus.tmp, tolower)
corpus.tmp<- tm_map(corpus.tmp, removeWords, stopwords("english"))
corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument)
TDM <- DocumentTermMatrix(corpus.tmp)
inspect(TDM)
tdm_tfxidf<-weightTfIdf(TDM)
m<- as.matrix(tdm_tfxidf)
rownames(m)<- 1:nrow(m)

norm_eucl<- function(m)
  m/apply(m,1,function(x) sum(x^2)^.5)
m_norm<-norm_eucl(m)

  results<-kmeans(m_norm,cluster,cluster)  
  return(results$cluster)
}


result_based_on_name<-as.integer(kmeans_clutering(data[,2],30))
result_based_on_info<-as.integer(kmeans_clutering(data[,3],16))
result_based_on_cat<-as.integer(kmeans_clutering(data[,4],60))
result<-cbind(data,result_based_on_cat,result_based_on_info,result_based_on_name)
mean_class=c()
for (i in 1:nrow(data)) {
  mean_class[i]<-mean(result_based_on_cat[i],result_based_on_info[i],result_based_on_name[i])
}
mean_class=as.factor(as.integer(mean_class))
result<-cbind(result,mean_class)

unique_classes=unique(result$mean_class)
keywordsClass=c()
keywordsClass_Name=c()
for (i in 1:length(unique_classes)) {
  key1=keyword(result[result$mean_class==unique_classes[i],4])
  key2=keyword(result[result$mean_class==unique_classes[i],3])
  key3=keyword(result[result$mean_class==unique_classes[i],2])
  keywordsClass[i]=unique_classes[i]
  
  keywordsClass_Name[i]=paste0(key1,"-",key2,"-",key3)
  result$cat1[result$mean_class==unique_classes[i]]=paste0(key1,"-",key2,"-",key3)
  
}

write.csv(result,"D:/Personal/minewhat_problem/Data/Solution_Problem_1.csv")


subdata=data[6:9]
alpha1=coef(glm(data=subdata,subdata$purchases_Y15.M08~subdata$purchases_Y15.M10+subdata$purchases_Y15.M11))
alpha2=coef(glm(data=subdata,subdata$purchases_Y15.M09~subdata$purchases_Y15.M10+subdata$purchases_Y15.M11))

subdata$purchases_Y15.M08_fited=alpha1[1]+alpha1[2]*subdata$purchases_Y15.M10+alpha1[3]*subdata$purchases_Y15.M11
subdata$purchases_Y15.M09_fited=alpha2[1]+alpha2[2]*subdata$purchases_Y15.M10+alpha2[3]*subdata$purchases_Y15.M11

result_2<-cbind(data,subdata[,c(5,6)])
write.csv(result_2,"D:/Personal/minewhat_problem/Data/Solution_Problem_2.csv")

