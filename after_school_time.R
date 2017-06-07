#setwd("/Users/lukefostvedt/Documents/PISA 2012/RDA files/")
library(stringr)
library(ggplot2)
library(reshape)
library(dplyr)
library(lubridate)
library(ggvis)
library(doBy)
library(maps)
library(ggmap)
library(rworldmap)
sessionInfo()
################## IMPORTANT #######################
# ************************************************
# My file path has the rda files in the main folder.
# They are not in a "data" folder.
# ************************************************
####################################################

sets <- c("item", "parent", "school", "scoredItem", "student")
BaseDir <- "E:\\R\\PISA\\data"
vizDir <- "E:\\R\\PISA\\viz"
# function to build the file names
fn_build <- function(file_name) {

    template <- c("2012.rda", "2012dict.rda")

    file_name %>% vapply(str_join, template, template) %>% file.path(BaseDir, 
        .)
}

# load the data
sets %>% fn_build %>% lapply(load, .GlobalEnv)


fn_make_df <- function(named_vector){
  data.frame(
    variable = attr(named_vector, "names"),
    description = named_vector,
    row.names = NULL
  )
}

# there's a clever way to do this, but beyond me for naw
dict_item2012 <- fn_make_df(item2012dict) 
dict_parent2012 <- fn_make_df(parent2012dict) 
dict_school2012 <- fn_make_df(school2012dict) 
dict_scoredItem2012 <- fn_make_df(scoredItem2012dict) 
dict_student2012 <- fn_make_df(student2012dict) 

# clean
rm(fn_make_df)
rm(item2012dict, parent2012dict, school2012dict, scoredItem2012dict, student2012dict)
load(paste0(BaseDir, '\\', "student2012pvmeans.Rdata"))
student2012 <- cbind(student2012,a)

student2012$ST15Q01 <- addNA(student2012$ST15Q01)
student2012$ST19Q01 <- addNA(student2012$ST19Q01)

student2012$name <- as.character(student2012$CNT)
# unique(anti_join(student2012.sub, world.polys)[1])
student2012$name[student2012$name=="United Arab Emirates"] <- "UAE"
student2012$name[student2012$name=="United Kingdom"] <- "UK"
student2012$name[student2012$name=="Serbia"] <- "Serbia"
student2012$name[student2012$name=="Korea"] <- "South Korea"
student2012$name[student2012$name=="Chinese Taipei"] <- "Taiwan"
student2012$name[student2012$name=="Slovak Republic"] <- "Slovakia"
student2012$name[student2012$name=="Russian Federation"] <- "Russia"
student2012$name[student2012$name=="Perm(Russian Federation)"] <- "Russia"
student2012$name[student2012$name=="Hong Kong-China"] <- "Hong Kong"
student2012$name[student2012$name=="China-Shanghai"] <- "China"
student2012$name[student2012$name=="Macau"] <- "China"
student2012$name[student2012$name=="Connecticut (USA)"] <- "USA"
student2012$name[student2012$name=="Florida (USA)"] <- "USA"
student2012$name[student2012$name=="Massachusetts (USA)"] <- "USA"
student2012$name[student2012$name=="United States of America"] <- "USA"
unique(student2012$name)
student2012$name <- factor(student2012$name)

student2012$UID <- paste(student2012$STRATUM,student2012$SCHOOLID,sep="")
school2012$UID <- paste(school2012$STRATUM,school2012$SCHOOLID,sep="")
ind <- match(student2012$UID,school2012$UID)
studschool <- cbind(student2012,school2012[ind,-c(1:6)])



# Out of School Study time
b1 <- summaryBy(data=student2012, pvM + pvR + pvS+ ST57Q01+ ST57Q02 +ST57Q03+ ST57Q04+ ST57Q05+ ST57Q06 ~CNT,FUN=mean, na.rm=T)
names(b1) <- c("CNT","pvM","pvR","pvS","ST57Q01", "ST57Q02","ST57Q03", "ST57Q04", "ST57Q05", "ST57Q06" )
# Out of school study time (total hours HW)
b1$CNT <- reorder(factor(b1$CNT),b1$pvS, mean)
b2 <- melt(b1[,c("CNT","pvM","pvR","pvS")],id="CNT")
b2 <- cbind(b2, b1[match(b2$CNT,b1$CNT), 5:10])
qplot(CNT, value,color=variable,size=ST57Q01, data = b2)+ coord_flip()
head(b2)
qplot(ST57Q01, value,color=variable,size=ST57Q02, data = b2)+ coord_flip()+facet_wrap(~CNT)

student.sub <- studschool[,c("name","pvM","pvR","pvS","ST57Q01","OECD","SC03Q01")]
study <- melt(student.sub,id=c("name","ST57Q01","OECD","SC03Q01"))
names(study) <- c("Country","HomeworkHours","OECD","AREA","Subject","Score")
levels(study$Subject) <- c("Math","Reading","Science")
p <- ggplot(data = study, aes(HomeworkHours,Score,colour=Subject) )+xlim(0,20)+facet_wrap(~Country)+stat_smooth(se=F)
#ggsave(p,file="studytime.pdf",height=13,width=13)


p <- ggplot(data=student.sub, aes(ST57Q01, ..density..)) + geom_histogram(binwidth=5) + xlab("Out-of-School Study Hours") +xlim(0,30)
#ggsave(p,file="HistHWschool.pdf",width=8,height=6)

student.sub <- studschool[,c("name","pvM","ST57Q01","SC03Q01")]
study <- student.sub
names(study) <- c("Country","Math","HomeworkHours","Schoolplace")







