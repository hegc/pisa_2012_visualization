library("dplyr")
library("ggplot2")
library("gbm")
library("reshape2")
library("grid")
library("gridExtra")

#Set BaseDir
BaseDir <- "E:\\R\\PISA\\data"
vizDir <- "E:\\R\\PISA\\viz"

# 
#

BooksGraphic <- function(){
  
  #FileNames <- c("'student2012") #, "'student2012dict")
  
  #for(fn in FileNames){
    #load(paste0(BaseDir, "\\", fn, ".rda"))
  #}
  load(paste0(BaseDir, '\\', 'student2012.rda'))
  load(paste0(BaseDir, '\\', 'student2012dict.rda'))

  # "Country code 3-character"
  # "How many books at home"
  # "Mother<Highest Schooling>"
  # "Father<Highest Schooling>"

  keepColumns <- c(
    "CNT",
    "ST28Q01",
    "ST13Q01",
    "ST17Q01",
    "PV1MATH", 
    "PV1READ", 
    "PV1SCIE")
  
  student2012 <- student2012[, keepColumns]
  
  student2012$AvgScore <- (student2012$PV1MATH + student2012$PV1READ + student2012$PV1SCIE) / 3
  
  student2012$Books <- "> 25 books"
  student2012$Books[is.element(as.character(student2012$ST28Q01), c("0-10 books ", "11-25 books "))] <-
    "<= 25 books"
  
  theme_pisa <- theme_grey() + theme(panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color="darkgray", linetype="dashed", size=.3),
    panel.background = element_rect(color="black"),
    plot.title = element_text(size = 20, vjust=2),
    axis.title = element_text(vjust=-.1, size = 14),
    axis.title.x = element_text(vjust=-.11), 
    axis.title.y = element_text(angle=90),
    axis.text = element_text(size=11, color="black"), plot.margin = unit(c(1,1,1,1), "cm"),
    legend.position = "bottom", legend.background = element_rect(color="black"),
    legend.title = element_blank(), legend.text = element_text(size = 14, vjust=1),
    strip.background = element_rect(color="black", fill="slategray1"), 
    strip.text = element_text(size = 12))
  
  booksPlot <- ggplot(data=student2012, aes(x=AvgScore, y = ..density..)) +
    
    geom_histogram(data=student2012[student2012$Books=="<= 25 books", ],
                   aes(fill="navy"), color="darkgrey", alpha=0.4) +
    geom_histogram(data=student2012[student2012$Books=="> 25 books", ],
                   aes(fill="orange"), color="darkgrey", alpha=0.4) +
    
    labs(x="学生总分数", y=NULL, 
         title="家里书的数量与分数，家里书越多，总分数越高") +
    
    scale_fill_identity(name="", guide="legend",labels = c(" <= 25 本", " > 25 本")) +
    
    theme_pisa + theme(
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank())
  
  return(booksPlot)
  
}

#
# Create books inequality .png
#
png(paste0(vizDir, "\\BooksInequality.png"), width=800, height=600)
print(BooksGraphic())
dev.off()