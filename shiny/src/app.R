library(plotly)
library(shiny)

create_PCA <- function(gender, bmi) {
  demographics_dataset <- "./data/metaboanalyst_data_with_demographics.csv"
  x_dataset <- paste("./data/pca_", gender, "_", bmi, "_x.csv", sep = "")
  var_dataset <- paste("./data/pca_", gender, "_", bmi, "_var.csv", sep = "")
  cls_dataset <- paste("./data/pca_", gender, "_", bmi, "_cls.csv", sep = "")
  
  df_demographics <- read.csv(demographics_dataset)
  df_x <- read.csv(x_dataset)
  rownames(df_x) <- 1:nrow(df_x)
  df_var <- read.csv(var_dataset)
  df_cls <- read.csv(cls_dataset)
  df_cls$x <- as.factor(df_cls$x)
  
  columns = c("AGE", "GENDER", "BMI", "Sample", "TREATMENT")
  df_demographics <- df_demographics[columns]
  merged_data <- merge(df_x, df_demographics, how="inner", on = "Sample")
  rownames(merged_data) <- merged_data$Sample
  merged_data$Sample <- NULL
  merged_data$Info <- paste("AGE : ", merged_data$AGE, " GENDER : ", merged_data$GENDER, " BMI : ", merged_data$BMI)
  col <- c("#4A274F", "#CCF381")
  
  levels(merged_data$TREATMENT)[levels(merged_data$TREATMENT)=='A'] <- "Aspirin"
  levels(merged_data$TREATMENT)[levels(merged_data$TREATMENT)=='B'] <- "Placebo"
  treatment <- factor(merged_data$TREATMENT, levels=rev(levels(merged_data$TREATMENT)))
  xlabel = paste("PC",1, "(", round(100*df_var$x[1],1), "%)");
  ylabel = paste("PC",2, "(", round(100*df_var$x[2],1), "%)");
  zlabel = paste("PC",3, "(", round(100*df_var$x[3],1), "%)");
  
  p <- plotly::plot_ly(x = merged_data$PC1, y = merged_data$PC2, z = merged_data$PC3, text = merged_data$Info,
                       color = treatment, colors = col,hovertemplate = paste('C1 Score: %{x:.2f}\nC2 Score: %{y:.2f}\nC3 Score: %{z:.2f}\n%{text}'))
  
  p <- plotly::add_markers(p, sizes = 5)
  p <- plotly::layout(p, scene = list(xaxis = list(title = xlabel),
                                      yaxis = list(title = ylabel),
                                      zaxis = list(title = zlabel)))
  p
}

create_PLSDA <- function(gender, bmi) {
  demographics_dataset <- "./data/metaboanalyst_data_with_demographics.csv"
  x_dataset <- paste("./data/plsda_", gender, "_", bmi, "_x.csv", sep = "")
  var_dataset <- paste("./data/plsda_", gender, "_", bmi, "_var.csv", sep = "")
  cls_dataset <- paste("./data/plsda_", gender, "_", bmi, "_cls.csv", sep = "")
  
  df_demographics <- read.csv(demographics_dataset)
  df_x <- read.csv(x_dataset)
  df_var <- read.csv(var_dataset)
  df_cls <- read.csv(cls_dataset)
  df_cls$x <- as.factor(df_cls$x)
  
  columns = c("AGE", "GENDER", "BMI", "Sample")
  df_demographics <- df_demographics[columns]
  df_x <- merge(df_x, df_demographics, how="inner", on = "Sample")
  df_x$Sample <- NULL
  df_x$Info <- paste("AGE : ", df_x$AGE, " GENDER : ", df_x$GENDER, " BMI : ", df_x$BMI)
  
  # Label and plot graph
  levels(df_cls$x)[levels(df_cls$x)==0] <- "Placebo"
  levels(df_cls$x)[levels(df_cls$x)==1] <- "Aspirin"
  xlabel <- paste("Component", 1, "(", round(100*df_var$x[1]/df_var$x[9],1), "%)");
  ylabel <- paste("Component", 2, "(", round(100*df_var$x[2]/df_var$x[9],1), "%)");
  zlabel <- paste("Component", 3, "(", round(100*df_var$x[3]/df_var$x[9],1), "%)");
  p <- plotly::plot_ly(x = df_x$Comp.1, y = df_x$Comp.2, z = df_x$Comp.3,
                       color = df_cls$x,text = df_x$Info, colors = c("#4A274F", "#CCF381"),hovertemplate = paste('C1 Score: %{x:.2f}\nC2 Score: %{y:.2f}\nC3 Score: %{z:.2f}\n%{text}'))
  p <- plotly::add_markers(p, sizes = 5)
  p <- plotly::layout(p, scene = list(xaxis = list(title = xlabel),
                                      yaxis = list(title = ylabel),
                                      zaxis = list(title = zlabel)))
  p
}

ui <- fluidPage(
  sidebarPanel(
    fluidRow(
      selectInput("gender", "Gender", c("both", "male", "female")),
      selectInput("bmi", "BMI", c("both", "normal", "overweight"))
    ),
    fluidRow(
      h4("Effect of Aspirin on Plasma Protein Concentration in a Colorectal Cancer Prevention Study"),
      HTML("<ul>
           <li>Long term use of aspirin is associated with lower risk of colorectal cancer but the mechanism is not known</li>
           <li>This cross-over randomized study of 44 individuals investigates how aspirin may change biological response, specifically plasma protein concentration after taking aspirin</li>
           <li>Participants were randomized to take a regular dose of aspirin (325 mg/day) and placebo, each for 60 days, while switching treatment after a 3-month washout period</li>
           </ul>")
      )
      ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Delta Analysis",
        sliderInput(
          "deltaPercentile",
          label = "Percentile",
          min = 1, max = 99, value = 95),
        plotOutput("deltaHistogram"),
        uiOutput("plotsTitle"),
        uiOutput("proteinPlots"),
        verbatimTextOutput("ph")),
      
      tabPanel(
        "PCA / PLS-DA Analysis",
        fluidRow(
          h4("PCA plot of plasma protein concentration after 60 days of taking aspirin and placebo in a randomized cross-over controlled study of 44 individuals")
        ),
        fluidRow(
          column(8, plotlyOutput("pcaPlot")),
          column(
            3,
            h4("PCA Analysis"),
            HTML("<ul>
                 <li>Unsupervised analysis shows a fair clustering for 2700 plasma proteins</li>
                 <li>Better clustering is observed for men with normal BMI and women with overweight BMI</li>
                 <li>The number of observations for subcategory analysis is small so this result should be interpreted cautiously</li>
                 </ul>"))
            ),
        fluidRow(
          h4("PLS-DA plot of plasma protein concentration after 60 days of taking aspirin and placebo in a randomized cross-over controlled study of 44 individuals")
        ),
        fluidRow(
          column(8, plotlyOutput("plsdaPlot")),
          column(
            3,
            h4("PLS-DA Analysis"),
            HTML("<ul>
                 <li>Supervised analysis shows a fair separation for 2700 plasma proteins after taking aspirin compared to placebo</li>
                 <li>The separation is more prominent for men with normal BMI, women with overweight BMI, and men with overweight BMI</li>
                 <li>The number of observations for subcategory analysis is small, and this result should be interpreted cautiously</li>
                 </ul>"))
            )
            )
            )
            )
        )

server <- function(input, output, session) {
  delta <- reactive({
    delta_dataset <- paste("./data/delta_", input$gender, "_", input$bmi, ".csv", sep = "")
    data <- read.csv(delta_dataset, header = TRUE)
    data[,]
  })
  
  significantProteins <- reactive({
    delta()[delta()$DeltaSquared > quantile(delta()$DeltaSquared, input$deltaPercentile / 100),]
  })
  
  n_plots <- reactive({
    n <- nrow(significantProteins())
    n2 <- n %/% 10
    if (n %% 10 > 0) {
      n2 <- n2 + 1
    }
    n2
  })
  
  output$deltaHistogram <- renderPlot({
    hist(delta()$DeltaSquared,
         col = 'gray',
         border = 'white',
         ylim = c(0, 2000),
         labels = TRUE,
         xlab = "Squared Difference in Plasma Protein Concentration",
         breaks = seq(0, 3.3, by = 0.05),
         main="Histogram of change in concentration for each plasma protein after taking aspirin vs. placebo")
    abline(v = quantile(delta()$DeltaSquared, input$deltaPercentile / 100), col = "red", lwd = 2)
  })
  
  output$plotsTitle <- renderUI({
    h2(paste("List of proteins with concentration change above ", input$deltaPercentile, "-th percentile", sep = ""))
  })
  
  output$proteinPlots <- renderUI({
    plots <- n_plots()
    plotOutputs <- lapply(1:plots, function(i) {
      plotOutput(paste("plot", i, sep = ""))
    })
    
    do.call(tagList, plotOutputs)
  })
  
  output$ph <- renderPrint({
    for (i in 1:n_plots()) {
      local({
        my_i <- i
        plotName <- paste("plot", my_i, sep = "")
        output[[plotName]] <- renderPlot({
          end <- my_i * 10
          start <- end - 9
          if (my_i == n_plots()) {
            n <- nrow(significantProteins())
            prev <- end - 10
            end <- n %% 10 + prev
          }
          df <- delta()[order(abs(delta()$Delta), decreasing = TRUE),]
          df <- df[start:end,]
          par(mai=c(1,5,1,1))
          barplot(
            df[order(abs(df$Delta), decreasing = FALSE),]$Delta,
            names.arg = df[order(abs(df$Delta), decreasing = FALSE),]$Protein,
            xlim = c(-0.5, 0.5),
            horiz=TRUE,
            las=1)
        })
      })
    }
  })
  
  output$pcaPlot <- renderPlotly({
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
  
  output$plsdaPlot <- renderPlotly({
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
}

shinyApp(ui, server)
