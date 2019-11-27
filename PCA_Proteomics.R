library(plotly)
library(shiny)
library(shinydashboard)
library(MetaboAnalystR)
file_name <- "metaboanalyst_data-2.csv"
gender <- "both"
bmi <- "both"

create_PCA <- function(gender, bmi) {
  if (gender == "female"){
    if (bmi == "normal"){
      file_name = "female_normal_data.csv"
    }
    else if (bmi == "overweight"){
      file_name = "female_overweight_data.csv"
    }
    else{
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
  
  mSet<-InitDataObjects("conc", "stat", TRUE)
  mSet<-Read.TextData(mSet, file_name, "rowp", "disc");
  mSet<-SanityCheckData(mSet)
  mSet<-ReplaceMin(mSet);
  mSet<-PreparePrenormData(mSet)
  mSet<-Normalization(mSet, "NULL", "NULL", "NULL", ratio=FALSE, ratioNum=20)
  mSet<-PCA.Anal(mSet)
  mSet<-PlotPCAPairSummary(mSet, "pca_pair_0_", "png", 72, width=NA, 5)
  mSet<-PlotPCAScree(mSet, "pca_scree_0_", "png", 72, width=NA, 5)
  mSet<-PlotPCA2DScore(mSet, "pca_score2d_0_", "png", 72, width=NA, 1,2,0.95,0,0)
  mSet<-PlotPCALoading(mSet, "pca_loading_0_", "png", 72, width=NA, 1,2);
  mSet<-PlotPCABiplot(mSet, "pca_biplot_0_", "png", 72, width=NA, 1,2)
  mSet<-PlotPCA3DScoreImg(mSet, "pca_score3d_0_", "png", 72, width=NA, 1,2,3, 40)
  mSet<-PlotPCA3DLoading(mSet, "pca_loading3d_0_", "json", 1,2,3)
  mSet<-PlotPCAPairSummary(mSet, "pca_pair_1_", "png", 72, width=NA, 2)
  
  df <- mSet$analSet$pca$x
  df <- cbind(Sample = rownames(df), df)
  rownames(df) <- 1:nrow(df)
  
  demo_df <- read.csv('metaboanalyst_data_with_demographics.csv')
  columns = c("AGE", "GENDER", "BMI", "Sample")
  demo_df <- demo_df[columns]
  merged_data <- merge(df, demo_df, how="inner", on = "Sample")
  rownames(merged_data) <- merged_data$Sample
  merged_data$Sample <- NULL
  merged_data$Info <- paste("AGE : ", merged_data$AGE, " GENDER : ", merged_data$GENDER, " BMI : ", merged_data$BMI)
  mSet$analSet$pca$x <- merged_data
  col <- c("#1972A4", "#FF7070")
  
  
  xlabel = paste("PC",1, "(", round(100*mSet$analSet$pca$variance[1],1), "%)");
  ylabel = paste("PC",2, "(", round(100*mSet$analSet$pca$variance[2],1), "%)");
  zlabel = paste("PC",3, "(", round(100*mSet$analSet$pca$variance[3],1), "%)");
  
  p <- plotly::plot_ly(x = mSet$analSet$pca$x[, 1], y = mSet$analSet$pca$x[, 2], z = mSet$analSet$pca$x[, 3], text = mSet$analSet$pca$x$Info,
                       color = mSet$dataSet$cls, colors = col,hovertemplate = paste('X: %{x:.2f}\nY: %{y:.2f}\nZ: %{z:.2f}\nInfo: %{text}'))
  
  p <- plotly::add_markers(p, sizes = 5)
  p <- plotly::layout(p, scene = list(xaxis = list(title = xlabel),
                                      yaxis = list(title = ylabel),
                                      zaxis = list(title = zlabel)))
  
  print("The Interactive 3D PCA plot has been created, please find it in mSet$imgSet$pca.3d.")
  p
}

# Export to Shiny
ui <- fluidPage(
  plotlyOutput("plot"),
  checkboxGroupInput("gender", "Choose Gender:", c("female", "male")
  ),
  checkboxGroupInput("bmi", "Choose BMI:", c("normal", "overweight")
  )
)
server <- function(input, output) {
  output$plot <- renderPlotly({
    if (is.null(input$gender) & is.null(input$bmi) ){return(create_PCA("both", "both"))}
    
    else if (is.null(input$gender)){
      return(create_PCA("both", input$bmi))
    }
    
    else if (is.null(input$bmi)){
      return(create_PCA(input$gender, "both"))
    }
    else{
      {return(create_PCA(input$gender, input$bmi))}
    }
  })

}
shinyApp(ui, server)