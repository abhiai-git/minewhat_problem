# # Author : Abhishek Bhardwaj
# # 
# Problem - 1 

#   Identify categories of products based on associated descriptions and categories of similar products. 

#   => input , csv of products with description fields and  category column having uncategorized.
#            e.g PID, category, name, description
#             "Pid1" ,  ["shoes"],    Nike Emerge 3, Running shoes with Jogging experience ...
#             "Pid2" ,  ["uncategorized"] ,  Puma Mens Axis, Fitness companion for Jogging ...

library(tm)

data<-read.csv("D:/Personal/minewhat_problem/Data/products.csv",header=T,stringsAsFactors = F)

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


result<-cbind(data,result_based_on_cat,result_based_on_info,result_based_on_name,mean_cat)
write.csv(result,"D:/Personal/minewhat_problem/Data/resultant.csv")
