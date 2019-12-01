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
  
  output$image <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- "pls_imp_0_dpi72.png"
    
    # Return a list containing the filename and alt text
    list(src = filename,
         contentType = "image/png")
    
  }, deleteFile = FALSE)
}
shinyApp(ui, server)
