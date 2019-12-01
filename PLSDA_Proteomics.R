# Set working directory
setwd("/Users/nickw/Desktop/Data511/Final_Project")

# Load libraries
library(MetaboAnalystR)
library(plotly)
library(shiny)
library(shinydashboard)

# PLSDA function
file_name <- "metaboanalyst_data.csv"
gender <- "both"
bmi <- "both"

create_PLSDA <- function(gender, bmi) {
  if (gender == "female"){
    if (bmi == "normal"){
      file_name = "female_normal_data.csv"
    } else if (bmi == "overweight"){
      file_name = "female_overweight_data.csv"
    } else{
      file_name = "female_both_data.csv"
    }
  }
  
  if (gender == "male"){
    if (bmi == "normal"){
      file_name = "male_normal_data.csv"
    }
    else if (bmi == "overweight"){
      file_name = "male_overweight_data.csv"
    }
    else{
      file_name = "male_both_data.csv"
    }
  }
  
  if (gender == "both"){
    if (bmi == "normal"){
      file_name = "both_normal_data.csv"
    }
    else if (bmi == "overweight"){
      file_name = "both_overweight_data.csv"
    }
  }

  df_sample <- read.csv(file_name)
  df_sample <- as.data.frame(df_sample$Sample)
  mSet<-InitDataObjects("conc", "stat", TRUE)
  mSet<-Read.TextData(mSet, file_name, "rowp", "disc");
  mSet<-SanityCheckData(mSet)
  mSet<-ReplaceMin(mSet);
  mSet<-PreparePrenormData(mSet)
  mSet<-Normalization(mSet, "NULL", "NULL", "NULL", ratio=FALSE, ratioNum=20)
  mSet<-PlotNormSummary(mSet, "norm_0_", "png", 72, width=NA)
  mSet<-PlotSampleNormSummary(mSet, "snorm_0_", "png", 72, width=NA)
  mSet<-PLSR.Anal(mSet, reg=TRUE)
  mSet<-PlotPLSPairSummary(mSet, "pls_pair_0_", "png", 72, width=NA, 5)
  mSet<-PlotPLS2DScore(mSet, "pls_score2d_0_", "png", 72, width=NA, 1,2,0.95,0,0)
  mSet<-PlotPLS3DScoreImg(mSet, "pls_score3d_0_", "png", 72, width=NA, 1,2,3, 40)
  mSet<-PlotPLSLoading(mSet, "pls_loading_0_", "png", 72, width=NA, 1, 2);
  mSet<-PlotPLS3DLoading(mSet, "pls_loading3d_0_", "json", 1,2,3)
  mSet<-PLSDA.CV(mSet, "T",5, "Q2")
  mSet<-PlotPLS.Classification(mSet, "pls_cv_0_", "png", 72, width=NA)
  mSet<-PlotPLS.Imp(mSet, "pls_imp_0_", "png", 72, width=NA, "vip", "Comp. 1", 15,FALSE)

  # Add in demographic information
  write.csv(mSet$analSet$plsr$scores, "test.csv")
  df <- read.csv("test.csv")
  df$X <- NULL
  df <- cbind(df,df_sample)
  names(df)[names(df) == "df_sample$Sample"] <- "Sample"
  demo_df <- read.csv('metaboanalyst_data_with_demographics.csv')
  columns = c("AGE", "GENDER", "BMI", "Sample")
  demo_df <- demo_df[columns]
  df <- merge(df, demo_df, how="inner", on = "Sample")
 # rownames(df) <- df$Sample
  df$Sample <- NULL
  df$Info <- paste("AGE : ", df$AGE, " GENDER : ", df$GENDER, " BMI : ", df$BMI)
  
  # Label and plot graph
  levels(mSet$dataSet$cls)[levels(mSet$dataSet$cls)==0] <- "Placebo"
  levels(mSet$dataSet$cls)[levels(mSet$dataSet$cls)==1] <- "Aspirin"
  xlabel <- paste("Component", 1, "(", round(100*mSet$analSet$plsr$Xvar[1]/mSet$analSet$plsr$Xtotvar,1), "%)");
  ylabel <- paste("Component", 2, "(", round(100*mSet$analSet$plsr$Xvar[2]/mSet$analSet$plsr$Xtotvar,1), "%)");
  zlabel <- paste("Component", 3, "(", round(100*mSet$analSet$plsr$Xvar[3]/mSet$analSet$plsr$Xtotvar,1), "%)");
  p <- plotly::plot_ly(x = df$Comp.1, y = df$Comp.2, z = df$Comp.3,
                       color = mSet$dataSet$cls,text = df$Info, colors = c("#1972A4","#FF7070"),hovertemplate = paste('C1 Score: %{x:.2f}\nC2 Score: %{y:.2f}\nC3 Score: %{z:.2f}\n%{text}'))
  p <- plotly::add_markers(p, sizes = 5)
  p <- plotly::layout(p, scene = list(xaxis = list(title = xlabel),
                                      yaxis = list(title = ylabel),
                                      zaxis = list(title = zlabel)))
  p
}

#'Plot PLS important variables,
#'@description Plot PLS important variables, BHan: added bgcolor parameter for B/W color
#'@param mSetObj Input name of the created mSet Object
#'@param imp.vec Input the vector of important variables
#'@param xlbl Input the x-label
#'@param feat.num Numeric, set the feature numbers, default is set to 15
#'@param color.BW Use black-white for plot (T) or colors (F)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotImpVar <- function(mSetObj=NA, imp.vec, xlbl, feat.num=15, color.BW=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  cls.len <- length(levels(mSetObj$dataSet$cls));
  if(cls.len == 2){
    rt.mrg <- 5;
  }else if(cls.len == 3){
    rt.mrg <- 6;
  }else if(cls.len == 4){
    rt.mrg <- 7;
  }else if(cls.len == 5){
    rt.mrg <- 8;
  }else if(cls.len == 6){
    rt.mrg <- 9;
  }else{
    rt.mrg <- 11;
  }
  op <- par(mar=c(5,7,3,rt.mrg)); # set right side margin with the number of class
  
  if(feat.num <= 0){
    feat.num = 15;
  }
  
  if(feat.num > length(imp.vec)){
    feat.num <- length(imp.vec);
  }
  
  # first get the top subset
  imp.vec <- rev(sort(imp.vec))[1:feat.num];
  
  # reverser the order for display
  imp.vec <- sort(imp.vec);
  
  # as data should already be normalized, use mean/median should be the same
  # mns is a list contains means of all vars at each level
  # conver the list into a matrix with each row contains var averages across different lvls
  mns <- by(mSetObj$dataSet$norm[, names(imp.vec)], mSetObj$dataSet$cls,
            function(x){ # inner function note, by send a subset of dataframe
              apply(x, 2, mean, trim=0.1)
            });
  mns <- t(matrix(unlist(mns), ncol=feat.num, byrow=TRUE));
  
  # vip.nms <-substr(names(imp.vec), 1, 12);
  vip.nms <- substr(names(imp.vec), 1, 14);
  names(imp.vec) <- NULL;
  
  # modified for B/W color
  dotcolor <- ifelse(color.BW, "darkgrey", "blue");
  dotchart(imp.vec, bg=dotcolor, xlab= xlbl, cex=1.3);
  
  mtext(side=2, at=1:feat.num, vip.nms, las=2, line=1)
  
  axis.lims <- par("usr"); # x1, x2, y1 ,y2
  
  # get character width
  shift <- 2*par("cxy")[1];
  lgd.x <- axis.lims[2] + shift;
  
  x <- rep(lgd.x, feat.num);
  y <- 1:feat.num;
  par(xpd=T);
  
  nc <- ncol(mns);
  
  # modified for B/W color
  colorpalette <- ifelse(color.BW, "Greys", "RdYlGn");
  col <- colorRampPalette(RColorBrewer::brewer.pal(10, colorpalette))(nc); # set colors for each class
  if(color.BW) col <- rev(col);
  
  # calculate background
  bg <- matrix("", nrow(mns), nc);
  for (m in 1:nrow(mns)){
    bg[m,] <- (col[nc:1])[rank(mns[m,])];
  }
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    cls <- mSetObj$dataSet$cls;
  }
  
  cls.lbl <- levels(cls);
  
  for (n in 1:ncol(mns)){
    points(x,y, bty="n", pch=22, bg=bg[,n], cex=3);
    # now add label
    text(x[1], axis.lims[4], cls.lbl[n], srt=45, adj=c(0.2,0.5));
    # shift x, note, this is good for current size
    x <- x + shift/1.25;
  }
  
  # now add color key, padding with more intermediate colors for contiuous band
  col <- colorRampPalette(RColorBrewer::brewer.pal(25, colorpalette))(50)
  if(color.BW) col <- rev(col);
  
  nc <- length(col);
  x <- rep(x[1] + shift, nc);
  
  shifty <- (axis.lims[4]-axis.lims[3])/3;
  starty <- axis.lims[3] + shifty;
  endy <- axis.lims[3] + 2*shifty;
  y <- seq(from = starty, to = endy, length = nc);
  
  points(x,y, bty="n", pch=15, col=rev(col), cex=2);
  
  text(x[1], endy+shifty/8, "High");
  text(x[1], starty-shifty/8, "Low");
  
  par(op);
}

# Export to Shiny
ui <- fluidPage(
  plotlyOutput("plot"),
  checkboxGroupInput("gender", "Choose Gender:", c("female", "male")
  ),
  checkboxGroupInput("bmi", "Choose BMI:", c("normal", "overweight")),
  imageOutput("image")
)
server <- function(input, output) {
  output$plot <- renderPlotly({
    if (is.null(input$gender) & is.null(input$bmi) ){return(create_PLSDA("both", "both"))}
    
    else if (is.null(input$gender)){
      return(create_PLSDA("both", input$bmi))
    }
    
    else if (is.null(input$bmi)){
      return(create_PLSDA(input$gender, "both"))
    }
    else{
      {return(create_PLSDA(input$gender, input$bmi))}
    }
  })
  
  output$image <- renderImage({
    filename <- "pls_imp_0_dpi72.png"
    
    list(src = filename,
         contentType = "image/png")
    
  }, deleteFile = TRUE)
}
shinyApp(ui, server)
