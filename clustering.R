rm(list=ls())
library(dplyr)


#setwd("C:/Users/fudf/Desktop/Construction_science")
california <- read.csv("C:/Users/fudf/Desktop/Construction_science/Data/california.csv")

california <- tbl_df(california)

item_id <- california$ITEM_ID

item_dictionary <- unique(item_id)

item_dictionary <- sort(item_dictionary)

item_names <- NULL
for(i in 1:length(item_dictionary)){#length(item_dictionary)){
  items <- california %>% filter(ITEM_ID==item_dictionary[i])
  item_names <- c(item_names, as.character(items$DESC[1]))
}

item_descriptions = cbind(item_dictionary, item_names)
#write.table(item_descriptions,file="item_description.txt",sep="\t")

item_test <- item_dictionary/1000

item_group_index = NULL
for(i in 1:length(item_dictionary)){
  item_group_index = c(item_group_index,floor(item_dictionary[i]/1000))
}
item_group_dictionary = unique(item_group_index)
#itemi <- california %>% filter(ITEM_ID == item_dictionary[2])


by_project = california %>% 
  group_by(PROJECT_ID) %>% 
  summarise(st=sum(TOTAL)) %>% 
  arrange(desc(st))

num = nrow(by_project)  ### number of top projects
proj_id <- as.character(unlist(by_project[,1]))

proj_worktype=NULL
for(i in 1:num){
   proji_worktype <- unique(as.matrix(california %>% filter(PROJECT_ID==proj_id[i]) %>% select(WORK_TYPE)))
   proj_worktype=rbind(proj_worktype,c(proj_id[i],proji_worktype))
}
#write.table(proj_worktype,file="proj_worktype.txt",sep="\t")


composition_matrix <- NULL
composition_group_matrix <- NULL
for(i in 1:num){   ## project id
  proji <- california %>% filter(PROJECT_ID==proj_id[i]) %>% arrange(desc(TOTAL))
  temp_item_id <- unique(proji$ITEM_ID)
  
  temp_total <- rep(0,length(item_dictionary))
  
  ### for grouping items ####
  temp_total_group <- rep(0, length(unique(item_group_index)))
  for(j in 1:length(temp_item_id)){
    itemj <- proji %>% filter(ITEM_ID == temp_item_id[j])
    ind_item <- which(item_dictionary == temp_item_id[j])
    item_group_cost = sum(itemj$TOTAL)
    ind_group = item_group_index[ind_item]
    
    temp = temp_total_group[which(item_group_dictionary==ind_group)]+item_group_cost
  #  print(item_group_cost)
    temp_total_group[which(item_group_dictionary==ind_group)] = temp
  }
  temp_total_group = temp_total_group/sum(temp_total_group)
  #ind_eliminate_group = which(temp_total_group<0.01)
  #temp_total_group[ind_eliminate_group] = 0.0
  composition_group_matrix = rbind(composition_group_matrix,temp_total_group)
  ### end grouping items ####
  
  
  
  for(j in 1:length(temp_item_id)){
    itemj <- proji %>% filter(ITEM_ID == temp_item_id[j])
    itemj_cost = sum(itemj$TOTAL)
    temp_total[which(item_dictionary==temp_item_id[j])] = itemj_cost
  }
  temp_total = temp_total/sum(temp_total)
  ind_eliminate = which(temp_total < 0.01)
  temp_total[ind_eliminate] = 0.0 # clean the items whose cost proportion < 1%
  composition_matrix = rbind(composition_matrix,temp_total)
}

#pdf("composition_group_matrix.pdf")
#fields::image.plot(composition_group_matrix)
#dev.off()

#comsvd = svd(composition_group_matrix)



##### hierarchical clustering #####
colnames(composition_matrix) = NULL
rownames(composition_matrix) = NULL
distance = dist(composition_matrix)
hc <- hclust(distance,method="ward.D")


par(mfrow=c(1,1))
plot(hc,hang=-1)

#ncluster = 7
#clusters <- cutree(hc,k=ncluster)



##### hierarchical clustering for grouped items #####
colnames(composition_group_matrix) = NULL
rownames(composition_group_matrix) = NULL
distance_group = dist(composition_group_matrix)

hc_group <- hclust(distance_group,method="ward.D")
par(mfrow=c(1,1))
#pdf("test.pdf")
plot(hc_group,hang=-1)
#dev.off()



ncluster= 4
clusters <- cutree(hc_group,k=ncluster)

number = 1
ind <- which(clusters==number)


test_matrix = composition_group_matrix

pdf(paste("./results/Group_",number,"_item_price_proportion_visualization.pdf",sep=""))
par(mfrow=c(4,4),mar=c(3,3,1,1))
for(i in 1:length(ind)){
  plot(test_matrix[ind[i],],ylab="",xlab="",type="l",col=i)
  mtext("Index",side=1,line=2,cex=0.5)
  mtext("Proportion",side=2,line=2,cex=0.5)
}
dev.off()

test = which(test_matrix[ind[1],] == max(test_matrix[ind[1],]))

ind_max_proportion = unique(item_group_index)[test]

ind_max_proportion


### try to know the patterns of the same cluster
project_cluster_i = proj_id[ind]
composition_cluster_i = composition_group_matrix[ind,]

composition_rearrange = NULL
for(i in 1:ncluster){
  ind <- which(clusters==i)
  composition_cluster_i = composition_group_matrix[ind,]
  print(dim(composition_cluster_i))
  composition_rearrange = rbind(composition_rearrange,composition_cluster_i)
}

pdf("composition_rearrange.pdf")
fields::image.plot(composition_rearrange)
dev.off()


#item <- california %>% filter(ITEM_ID == item_dictionary[test])
#item$DESC


#worktypes <- NULL
#for(i in 1:length(ind)){
#    proji <- california %>% filter(PROJECT_ID == proj_id[ind[i]])
#    worktype <- as.character(unique(proji$WORK_TYPE)[1])
#    worktypes <- c(worktypes, worktype)
#}
#worktypes

#write.table(worktypes,file=paste("Group_",number,"_worktypes.txt"))

#worktypes[grep("BRIDGE",worktypes)]
#worktypes[grep("PAVEMENT",worktypes)]

# for 4 groups
# group 1: road related (596), keyword: CONCRETE PAVEMENT
# group 2: asphalt related (194), keyword: ASPHALT
# group 3: rest areas related (39), keyword: BUILDING WORK
# group 4: traffic signals related (49), keyword: TRAFFIC





##### k means ######
km = kmeans(composition_group_matrix,centers=4,algorithm="Hartigan-Wong")

kmclusters = km$cluster
#for(i in 1:length(kmclusters)){
#    if(kmclusters[i]==1){kmclusters[i]=2}
#    if(kmclusters[i]==2){kmclusters[i]=1}
#}


total_worktype = NULL
for(i in 1:num){
  proji <- california %>% filter(PROJECT_ID == proj_id[i])
  worktype <- as.character(unique(proji$WORK_TYPE)[1])
  total_worktype <- c(total_worktype,worktype)
}

z = cbind(kmclusters,clusters,total_worktype)
#write.table(z,file="comparison.txt")

for(i in 1:4){
  indi = which(kmclusters==i)
  worktypei = z[indi,]
  #write.table(worktypei,file=paste("./results/kmeans_worktype",i,".txt",sep=""))
}


for(number in 1:4){
ind = which(kmclusters==number)

#pdf(paste("./results/kmeans_Group_",number,"_item_price_proportion_visualization.pdf",sep=""))
par(mfrow=c(4,4),mar=c(3,3,1,1))
for(i in 1:length(ind)){
  plot(test_matrix[ind[i],],ylab="",xlab="",type="l",col=i)
  mtext("Index",side=1,line=2,cex=0.5)
  mtext("Proportion",side=2,line=2,cex=0.5)
}
#dev.off()
}

count = 0

indi = which(kmclusters==4)
temp = length(which(clusters[indi]==3))

count = count + temp  #total 642 (73%) projects keep the same.


#### spectral clustering ####
W = as.matrix(distance_group)  #assimilarity matrix
G = diag(apply(W,2,sum))
L = G-W
L.eigen = eigen(L,symmetric =TRUE)
nc = nrow(L)
X_sc = L.eigen$vectors[,(nc-100):(nc-1)] #number of eigenvectors the smallest eigenvalues

X_sc_kmeans = kmeans(X_sc,4) # the results are not promising, since it 
            ##########can only divide the projects into one cluster
            ####### the other clusters have less number of projects, like 1or2

X_sc_kmeans$cluster











#### for 7 groups
#### group 4: hot mix asphalt, overlay works
#### group 5: overlay works like asphalt (12)
#### group 6: rest areas (39)
#### group 7: traffic signals (49)




total_worktype = NULL
for(i in 1:num){
  proji <- california %>% filter(PROJECT_ID == proj_id[i])
  worktype <- as.character(unique(proji$WORK_TYPE)[1])
  total_worktype <- c(total_worktype,worktype)
}

z = cbind(kmclusters,clusters,total_worktype)

#cluster 1 can be treated as ROAD-related projects
#cluster 2 can be treated as PAVEMENT-COVER-related projects



par(mfrow=c(2,2))
plot(composition_matrix[1,],type="l",col=1)
plot(composition_matrix[7,],type="l",col=2)
plot(composition_matrix[28,],type="l",col=3)
plot(composition_matrix[135,],type="l",col=4)



##### biclustering #######
composition_matrix1 = round(composition_group_matrix,digits=2) * 100

library(biclust)
biclusters <- biclust(composition_group_matrix, method=BCCC(), delta = 1.0, alpha=1.5, number=10)

library(biclust)
biclusters <- biclust(composition_group_matrix,method=BCSpectral(),numberOfEigenvalues=1)

index_1 = NULL
for(i in c(1,3,6,9)){
  indi = which(biclusters@RowxNumber[,i]=="TRUE")
  index_1 = c(index_1, indi)
}
index1 = sort(unique(index_1))
#index_rearrange=NULL
#for(i in 1:ncol(biclusters@RowxNumber)){
#  print(i)
#  indi = which(biclusters@RowxNumber[,i] == "TRUE")
#  indj = which(biclusters@NumberxCol[i,]=="TRUE")
#  composition_matrix_part = composition_group_matrix[indi,indj]
#  pdf(paste("./results/",i,"_bicluster_composition.pdf",sep=""))
#  fields::image.plot(composition_matrix_part)
#  dev.off()
#}

##### end biclustering #####




#### for specific selected projects clustering ####
colnames(composition_matrix) = NULL
rownames(composition_matrix) = NULL
hc <- hclust(dist(composition_matrix[1:50,]),method="ward.D")
plot(hc,hang=-1)

test <- california %>% filter(PROJECT_ID==proj_id[41])
unique(test$WORK_TYPE)

# 21, COLD PLANE, OVERLAY AND MBGR
# 29, HOT MIX ASPHALT SURFACING
# 43, PLACE RUBBERIZED HMA TYPE G AND


# 15, PLACE HOT MIX ASPHALT
# 33, REPLACE PCC SLABS COLD PLANE AC

# 44, REPLACE PAVEMENT
# 14, GRIND PAVEMENT AND REPLACE SLAB
# 50, REHABILITATE ROADWAY

# 36, REPLACE PAVEMENT
# 17, REHABILITATE ROADWAY AND RAMPS
# 40, CONSTRUCT BRIDGE

# 13, HOT MIX ASPHALT OVERLAY
# 5,  REHABILITATE CONCRETE
# 42, REPLACE CONCRETE PAVEMENT





# 20, WIDEN ROUTE 46 AND REALIGN ROUTE 33
# 24, WIDEN TO FOUR LANES



#  3, CONSTRUCT FOUR LANE EXPRESSWAY
#  5, CONVERT TWO LANE HIGHWAY
# 47, FREEWAY WIDENING
# 32, RUBBERIZED WARM MIX ASPHALT

