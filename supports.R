require("stats")
require("rgl")

# load data file
BaseDir <- "E:\\R\\PISA\\data"
vizDir <- "E:\\R\\PISA\\viz"
load(paste0(BaseDir, '\\', 'student2012.rda'))

# Indicators for support at school
school.support <- c("DISCLIMA", "MTSUP", "TEACHSUP", "STUDREL", "TCHBEHFA",
                    "TCHBEHSO", "TCHBEHTD")

# Indicators for support at home
home.support <- c("HISCED", "HISEI", "HOMEPOS", "CULTPOS", "ESCS", "HEDRES") 

data <- student2012[, c(school.support, home.support, "PV1MATH")]
# rm NAs
data <- data[complete.cases(data), ]

data.school <- data[, school.support]
data.home <- data[, home.support]

# convert variable into numeric
data.home$HISCED <- as.numeric(data.home$HISCED)

normalize <- function(x){
               (x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))}

# Normalize and combine indicators to get a numeric value for school and home
# support
data.home <- as.data.frame(apply(data.home, 2, normalize)) 
data.home$supp <- apply(data.home, 1, mean, na.rm=TRUE)

data.school <- as.data.frame(apply(data.school, 2, normalize)) 
data.school$supp <- apply(data.school, 1, mean, na.rm=TRUE)

data.result <- data.frame(school.supp=data.school$supp, 
                          home.supp=data.home$supp, 
                          score=normalize(data$PV1MATH))

# Divide data into different clusters
set.seed(43)
cl <- kmeans(data.result, 7)
data.result$cluster <- cl$cluster
#plot3d(data.result$school.supp, data.result$home.supp, data.result$score,
#       col=data.result$cluster)

# Determine mean values for each cluster
sp <- split(data.result, data.result$cluster)
data.centers <- as.data.frame(t(sapply(sp, colMeans)))
data.centers$n <- as.data.frame(t(sapply(sp, dim)))[, 1]

# Divide cluster size by maximum for plotting size
data.centers$n <- data.centers$n/max(data.centers$n)

plot3d(data.centers$school.supp*100, data.centers$home.supp, data.centers$score,
col=data.centers$cluster, size=300)

#Plot was made with POVray
