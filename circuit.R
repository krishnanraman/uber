origdest<- group_split(final %>% group_by(origin_taz,destination_taz))
mid<-sapply(1:840, function(i) {origdest[[i]]$`quantile(fare)`[3]})
a<-sort(unname(mid), decreasing = TRUE, index.return=TRUE)
# by inspection, notice top 68 median fares exceed $50 
indices<-a$ix[1:68]
origins<-sapply(indices, function(i) { origdest[[i]]$origin_taz[1] })
dest<-sapply(indices, function(i) { origdest[[i]]$destination_taz[1] })
common<-intersect(origins, dest)
x<-sapply(1:68, function(i) { (dest[i] %in% common) & (origins[i] %in% common) })
length(x[x==1])
# 68 tuples now reduce to 52
