library(asnipe)
library(igraph)
library(readr)
library(cluster)
library(animation)
library(RColorBrewer)
library(adehabitatHR)
library(ggmap)

actbud.sub$freq <- as.POSIXct(actbud.sub$freq, tz="UTC")  #Make Posix time-CHECK TIME ZONE!!!

#Make a dataframe to define time bins.

bins <- data.frame( bin1 = seq.POSIXt(from = min(actbud.sub$freq)-1*60, to = max(actbud.sub$freq)-1*60, by = 5*60),
                    
                    bin2 = seq.POSIXt(from = min(actbud.sub$freq)+4*60, to = max(actbud.sub$freq)+4*60, by = 5*60) )

#make a dataframe with 30mins
bins_30 <- data.frame( bin30 = seq.POSIXt(from = min(actbud.sub$freq)-1*60, to = max(actbud.sub$freq)-1*60, by = 30*60))

#Make a new column for time bin
actbud.sub$timeBin <- actbud.sub$freq

actbud.sub$bins <- cut(actbud.sub$ts, breaks= bins$bin1)
#bin with 30mins
actbud.sub$bin_30<- cut(actbud.sub$ts, breaks= bins_30$bin30)


#Importing data file
uni.df<-unique(x=actbud.sub[,c("bin_30","node_lat_mode","node_lng_mode")])
#assuming if the birds have pinged in nodes, they are in the general vicinity (ignore lat, lng of nodes)
uni.df<- actbud.sub[c("bin_30")]
uni.df$point<-paste0("X",rownames(uni.df))

df.with.pointnames<-left_join(actbud.sub,uni.df)
df.with.pointnames$presence<-1
df.with.pointnames<-unique(df.with.pointnames)
unique(df.with.pointnames$TagId)
unique(actbud.sub$TagId)

attribute.LO<- pivot_wider(data=df.with.pointnames, names_from =TagId, values_from =presence, values_fill=0)

attribute.LO2<-data.frame(t(attribute.LO[,48:ncol(attribute.LO)]))

names(attribute.LO2)<-attribute.LO$point
attribute.LO2

association=(as.matrix(attribute.LO2))
association[is.na(association)]=0
association

#Total points recorded for each bird
rowSums(association)

#Eliminating birds who has less than 5 data points
association=association[which(rowSums(association)>4),]

#Eliminating empty columns
association<-association[,which(colSums(association)>0)]

#Generating matrix
adjm=get_network(t(association),data_format = "GBI","SRI")

association.g=graph_from_adjacency_matrix(adjm,"undirected",weighted = T)
set.seed(2)

plot(association.g,edge.width=E(association.g)$weight*10,vertex.label=names(association.g),vertex.size=5)

#Converting association matrix into adjacency matrix using simple ratio index
gbi.lo=t(association)
adj.lo=get_network(gbi.lo,association_index="SRI")

#create weighted network
association.g=graph_from_adjacency_matrix(adj.lo,"undirected",weighted=T)
plot(association.g,edge.width = E(association.g)$weight * 10 )
plot(association.g,edge.width=E(association.g)$weight*10,vertex.label="",
     edge.color="blue", vertex.frame.color="black") 

#Community detection algorithms using Louvain method
community=cluster_louvain(association.g)

set.seed(2)
plot(community,association.g,main= "Community structures", vertex.label=names(association.g),
     vertex.label.cex=0.5,edge.width=E(association.g)$weight*12)


