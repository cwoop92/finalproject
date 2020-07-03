##### DATA ����빮���б�

library(ggplot2)
library('devtools')
library('ggmap')
register_google(key='�ڽ��Ǳ���APIkey')

#rm(list=ls())
##### ��絥���� ���հ���, �����ɷ��� ����������ϵ� ���� ÷���Ͽ����ϴ�.�ʿ��Ͻø� �ּ�#Ǫ�ø� �˴ϴ�.
#src_dir <- c("C:/Users/Jun Young/Documents/R/DATA") # ��� ���� : '\'�� '/'�� �ٲ�� ��
#src_file <- list.files(src_dir)
#src_file_cnt <- length(src_file)
#for(i in 1:src_file_cnt)
#  {
#  dataset <- read.table(paste(src_dir, "/", src_file[i], sep=""),sep=",")
#  write.table(dataset, paste(src_dir, "/", "dataset_all.txt", sep=""), sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)   
#  }

rm(list = ls())
src_dir <- c("C:/Users/ispl/Documents/R/DATA") #��� ���� �ʿ�
final_dataset <- read.table("C:/Users/ispl/Documents/R/DATA/dataset_all.txt", sep="") #��� ��ǻ�� - R - DATA���� �ʿ�
final_dataset <- as.matrix(final_dataset[,2])

write.table(final_dataset, paste(src_dir, "/", "dataset_all.txt", sep=""), sep = "", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)   
final_dataset <- read.table("C:/Users/ispl/Documents/R/DATA/dataset_all.txt", sep=",")
time <- final_dataset[,2]
final_dataset1 = round(final_dataset[,c(3,4)],3) 
final_dataset <- cbind(time,final_dataset1)
str(final_dataset)

# �����Ͱ� �ʰ� �ҷ�������,���� �����ؼ� ���� R dataset����Ͻø� �˴ϴ�. ÷���صξ����ϴ�.���� �ּ�ó�� �� �Ʒ� �ڵ��̿�
# load("C:/Users/Jun Young/Documents/R/DATA/save_test.RData")

######�ð��� ���� DBSCAN & Clustering ����
final_dataset$time <- strptime(final_dataset$time, format = "%H:%M:%S")
unclass(final_dataset$time)
final_dataset$time <- format(final_dataset$time, "%H%M%S")
final_dataset$time <- as.matrix(final_dataset$time) #�ð��� ��Ʈ������ȭ
final_dataset$time <- as.numeric(final_dataset$time) #�ð� ������ ��ȯ

mlength <- length(final_dataset$time)
for(i in 1:mlength)
{
  if (final_dataset$time[i] <= 60000 & final_dataset$time[i] >= 20000){
    final_dataset$time_num[i] <- 0
  }else if (final_dataset$time[i] <= 100000 & final_dataset$time[i] > 60000){
    final_dataset$time_num[i] <- 1
  }else if (final_dataset$time[i] <= 140000 & final_dataset$time[i] > 100000){
    final_dataset$time_num[i] <- 2
  }else if (final_dataset$time[i] <= 180000 & final_dataset$time[i] > 140000){
    final_dataset$time_num[i] <- 3
  }else if (final_dataset$time[i] <= 220000 & final_dataset$time[i] > 180000){
    final_dataset$time_num[i] <- 4
  }else {
    final_dataset$time_num[i] <- 5
  }
}
final_dataset11 <- as.factor(final_dataset[,5])
final_dataset[,5] <- final_dataset11

##### k means(���� �ð��뺰��)
locationInfo <- data.frame(lon = c(final_dataset$V3), lat = c(final_dataset$V4),time_num =c(final_dataset$time_num))

cluster_1 <- locationInfo[locationInfo$time_num == 1,]
cluster_1 <- cluster_1[1:2]
result_1 <- kmeans(cluster_1, 20)

cluster_2 <- locationInfo[locationInfo$time_num == 2,]
cluster_2 <- cluster_2[1:2]
result_2 <- kmeans(cluster_2, 20)

cluster_3 <- locationInfo[locationInfo$time_num == 3,]
cluster_3 <- cluster_3[1:2]
result_3 <- kmeans(cluster_3, 20)

cluster_4 <- locationInfo[locationInfo$time_num == 4,]
cluster_4 <- cluster_4[1:2]
result_4 <- kmeans(cluster_4, 20)

cluster_5 <- locationInfo[locationInfo$time_num == 5,]
cluster_5 <- cluster_5[1:2]
result_5 <- kmeans(cluster_5, 20)

cluster_6 <- locationInfo[locationInfo$time_num == 6,]
cluster_6 <- cluster_6[1:2]
result_6 <- kmeans(cluster_6, 20)

num_1 <- result_1$cluster
cluster_1 <- cbind(cluster_1,num_1)

num_2 <- result_2$cluster
cluster_2 <- cbind(cluster_2,num_2)

num_3 <- result_3$cluster
cluster_3 <- cbind(cluster_3,num_3)

num_4 <- result_4$cluster
cluster_4 <- cbind(cluster_4,num_4)

num_5 <- result_5$cluster
cluster_5 <- cbind(cluster_5,num_5)

num_6 <- result_6$cluster
cluster_6 <- cbind(cluster_6,num_6)

###### Ŭ�����͸� �� �ѹ���
locationInfo_1 <- data.frame( 
  lon = c(cluster_1$lon),
  lat = c(cluster_1$lat),
  num = c(cluster_1$num_1)
)

locationInfo_2 <- data.frame( 
  lon = c(cluster_2$lon),
  lat = c(cluster_2$lat),
  num = c(cluster_2$num_2)
)

locationInfo_3 <- data.frame( 
  lon = c(cluster_3$lon),
  lat = c(cluster_3$lat),
  num = c(cluster_3$num_3)
)

locationInfo_4 <- data.frame( 
  lon = c(cluster_4$lon),
  lat = c(cluster_4$lat),
  num = c(cluster_4$num_4)
)

locationInfo_5 <- data.frame( 
  lon = c(cluster_5$lon),
  lat = c(cluster_5$lat),
  num = c(cluster_5$num_5)
)

locationInfo_6 <- data.frame( 
  lon = c(cluster_6$lon),
  lat = c(cluster_6$lat),
  num = c(cluster_6$num_6)
)


##### ���� �׸���
center <- c(mean(x = final_dataset$V3), mean(x = final_dataset$V4))
p1 <- get_map(location = 'bajing',
              zoom = 8,
              maptype = 'roadmap',
              source = 'google') %>% ggmap
##### ���
num_5 <- as.factor(num_5)
p1 + geom_point(data = locationInfo_5, mapping = aes(x = lon, y = lat, color = num_5))
result_5$centers <- as.numeric(result_5$centers)
result5_centers <- matrix(result_5$centers, byrow = F, ncol = 2)
result5_centers <- as.matrix(result5_centers)
result5_centers <- data.frame(result5_centers)
names(result5_centers) <- c("long", "lati")

p2 <- get_map(location = 'bajing',
              zoom = 10,
              maptype = 'roadmap',
              source = 'google') %>% ggmap
p2 + geom_point(data = result5_centers, mapping = aes(x = long, y = lati, size = result_5$size), shape = 16, color = "red", alpha = 0.4) + scale_size_area(max_size=30)

##### �ð��뺰 ����׷���
a <-length(which(final_dataset$time_num==0))
b <-length(which(final_dataset$time_num==1))
c <-length(which(final_dataset$time_num==2))
d <-length(which(final_dataset$time_num==3))
e <-length(which(final_dataset$time_num==4))
f <-length(which(final_dataset$time_num==5))
x <- c(a/100000,b/100000,c/100000,d/100000,e/100000,f/100000,a/17662984*100,b/17662984*100,c/17662984*100,d/17662984*100,e/17662984*100,f/17662984*100)
x <- matrix(x, ncol=2)
x <- t(x)

Grap <- barplot(x,xlab ="�ð���",ylab="�����α��� & ����(%)",beside=T,col=c("red","blue"),names=c('02:00~06:00','06:00~10:00','10:00~14:00','14:00~18:00','18:00~22:00','22:00~02:00'))

