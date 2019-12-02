library(plotly)
library(shiny)
library(shinydashboard)
library(MetaboAnalystR)
file_name <- "metaboanalyst_data-2.csv"
gender <- "both"
bmi <- "both"

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
create_PLSDA <- function(gender, bmi) {
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
  mSet<-PlotPLS.Imp(mSet, "pls_imp_0_", "png", 72, width=6, "vip", "Comp. 1", 15,FALSE)
  
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

create_PCA <- function(gender, bmi) {
  
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

## app.R ##
ui <- dashboardPage(
  dashboardHeader(title = "Cancer Prevention Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("PCA and PLSDA", tabName = "PCAPLSDA", icon = icon("th")),
      menuItem("Delta Analysis", tabName = "Delta", icon = icon("dashboard"))
    ),
    checkboxGroupInput("gender", "Choose Gender:", c("female", "male")),
    checkboxGroupInput("bmi", "Choose BMI:", c("normal", "overweight"))
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "PCAPLSDA",
              fluidPage(
                fluidRow(
                  column(7, plotlyOutput("plotpca", height = 400)),
                  column(3, wellPanel(p("Summary description placeholder", height = 400)))),
                
                fluidRow(
                  column(7, plotlyOutput("plotplsda",height = 435)),
                  column(3, imageOutput("image", height = 435)))
              )
      ),
      # Second tab content
      tabItem(tabName = "Delta",
              h2("Delta Analysis placeholder")
      )
    )
  )
)

server <- function(input, output) {
  output$plotplsda <- renderPlotly({
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
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- "pls_imp_0_dpi72.png"
    # Return a list containing the filename and alt text
    list(src = filename,
         contentType = "image/png")
  }, deleteFile = FALSE
  )
  
  output$plotpca <- renderPlotly({
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
