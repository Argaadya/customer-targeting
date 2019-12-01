library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(data.table)
library(plotly)
library(ggalluvial)
library(DT)
library(scales)
library(fishualize)
library(ranger)
library(networkD3)
library(caret)
library(tidymodels)
library(rpart)
library(e1071)
library(rattle)
library(clustMixType)
library(glue)
library(treemap)
library(d3treeR)


seq_rep <- function(x,y){
  lol <- 1:n_distinct(x)
  vec <- numeric()
  for (i in 1:n_distinct(x)) {
    vec <- c(vec,rep(lol[i],n_distinct(y)))
  }
  return(vec)
}

options(scipen = 100)
load("modeling.RData")
rownames(df) <- 1:nrow(df)

ui <- dashboardPage(skin = "red",
          dashboardHeader(title = "Customer Targeting"),
          dashboardSidebar(
            sidebarMenu(
              # Overview
              menuItem("Overview", tabName = "overview", icon = icon("poll-h")),
              
              # Customer Profiling
              menuItem("Profiling", tabName = "profiling", icon = icon("layer-group"),
                       menuSubItem(tabName = "sankey", text = "Sankey Diagram"),
                       menuSubItem(tabName = "cluster", text = "Clustering")),
              
              # Classification
              menuItem("Predict", tabName = "classic", icon = icon("eye")),
              
              # Data
              menuItem("Data",tabName = "data",icon = icon("database"))
            )
          ),
          dashboardBody(
            tabItems(
              tabItem(tabName = "overview",
                      h2("Overview"), 
                      br(),
                      "Predicting whether a potential customer will buy our product is critical for the sales team. Targeting the right customer with high precision can save cost of targetting the mistargetting while also increase the sales.", 
                      br(),
                      "Here is an overview of number of people who bought our product. We can breakdown the number of people who bought the product based on a category.",
                      br(),br(),
                      selectInput(inputId = "bar_cat", label = "Insert Category for Grouping", 
                                  choices = c(names(df %>% select(-c(flag, house_val))), "none"), selected = "none"),
                      plotlyOutput(outputId = "bar_flag"),
                      br(),
                      "More people actually buy our car. To increase our sales, we want to better understand the customer through exploring some demographics data and try to predict if someone will actually buy our products using machine learning models."
                      ),
              
              tabItem(tabName = "sankey",
                      h2("Profiling Customer with Sankey Diagram"),br(),
                      h4("On this tab, you can do more detailed and sophisticated customer profiling using the Sankey Diagram"),
                      br(),
                      fluidRow(
                        column(width = 3,
                               selectInput(inputId = "axis1", label = "Select First Category", 
                                           choices = names(df %>% select(-flag) %>%select_if(is.factor)), 
                                           selected = "education")),
                        column(width = 3,
                               selectInput(inputId = "axis2", label = "Select Second Category", 
                                           choices = names(df %>% select(-flag) %>%select_if(is.factor)), 
                                           selected = "gender")),
                        column(width = 3,
                               selectInput(inputId = "axis3", label = "Select Third Category", 
                                           choices = names(df %>% select(-flag) %>%select_if(is.factor)), 
                                           selected = "marriage")),
                        column(width = 3,
                               selectInput(inputId = "axis4", label = "Select Fourth Category", 
                                           choices = names(df %>% select(-flag) %>% select_if(is.factor)), 
                                           selected = "age"))
                        
                      ),
                      h3("Customer Who Bought The Product"),br(),
                      plotlyOutput("sankey_yes"),
                      br(),
                      h3("Customer Who Didn't Buy The Product"),br(),
                      plotlyOutput("sankey_no")
                      ),
              
              tabItem(tabName = "cluster",
                      h2("Profiling Customer with Clustering Method"), br(),
                      h4("Determine how many clusters you want to make and find unique characteristics from each clusters"), br(),
                      sliderInput(inputId = "cluster_k", label = "Select Number of Cluster",
                                  min = 2, max = 20, value = 4, step = 1),
                      actionButton(inputId = "go_cluster", label = "Start Clustering"),
                      br(), br(),
                      tabBox(width = 12,
                        tabPanel(title = "Cluster Size",
                                 plotlyOutput(outputId = "cluster_sun")),
                        tabPanel(title = "Treemap",
                                 h3("Treemap of Variable Proportion on Each Cluster"),br(),
                                 "Number indicate each clusters",
                                 selectInput(inputId = "treemap_input",
                                             label = "Select Variable",
                                             choices = names(df %>% select(-house_val))),
                                 d3treeOutput("cluster_treemap", height = 480)),
                        tabPanel(title = "Table",
                                 h4("Table shows the most common characteristics on each clusters"),
                                 dataTableOutput(outputId = "cluster_table"))
                      )
                      ),
              
              tabItem(tabName = "classic",
                      h2("Predict Customer's Probability to Buy"),
                      h3("Insert The Customer's Profile"),
                      br(),
                      fluidRow(
                                   column(width = 2,
                                          selectInput(inputId = "genderi", label = "Gender", 
                                                      choices = df$gender, selected = "Male")),
                                   column(width = 2,
                                          selectInput(inputId = "educationi", label = "Education", 
                                                      choices = df$education, selected = "Grad")),
                                   column(width = 2,
                                          selectInput(inputId = "agei", label = "Age", 
                                                      choices = df$age, selected = "<=35")),
                                   column(width = 3,
                                          selectInput(inputId = "onlinei", label = "Had online shopping experience", 
                                                      choices = df$online, selected = "Yes")),
                                   column(width = 2,
                                          selectInput(inputId = "childreni", label = "Has children", 
                                                      choices = df$child, selected = "Yes"))
                                 ),
                                 fluidRow(
                                   column(width = 2,
                                          selectInput(inputId = "marriagei", label = "Marriage", 
                                                      choices = df$marriage,selected = "Married")),
                                   column(width = 2,
                                          selectInput(inputId = "occupationi", label = "Occupation", 
                                                      choices = df$occupation, selected = "Professional")),
                                   column(width = 2,
                                          selectInput(inputId = "mortgagei", label = "Housing Loan", 
                                                      choices = df$mortgage, selected = "Low")),
                                   column(width = 3,
                                          selectInput(inputId = "housei", label = "House Ownership", 
                                                      choices = df$house_owner, selected = "Owner")),
                                   column(width = 2,
                                          selectInput(inputId = "regioni", label = "Customer's Address", 
                                                      choices = df$region, selected = "Midwest"))
                                  
                                 ),
                                 numericInput(inputId = "hoval",label = "House Valuation",
                                              min = min(df$house_val), max = max(df$house_val),
                                              value = median(df$house_val)),
                               actionButton(inputId = "start_predict",
                                            label = "Predict"),
                        br(),
                        h3("Prediction from 3 Machine Learning Models"),
                        
                        tabBox(width = 12, height = "300px",
                          tabPanel(title = "Naive Bayes",
                                   column(width = 4,
                                   infoBox("Prediction",
                                           value = textOutput(outputId = "naive_pred"), 
                                           subtitle = textOutput("naive_yes"),
                                           width = 12,icon = icon("search"))
                                   ),
                                   column(width = 8,
                                    fluidRow(
                                   valueBox("Precision", color = "green",
                                            value = textOutput("naive_precision"),width = 5),
                                   valueBox("Accuracy",
                                            value = textOutput("naive_accuracy"),width = 5)
                                     ),
                                    fluidRow(
                                   valueBox("Sensitivity",
                                            value = textOutput("naive_sensitivity"),width = 5),
                                   valueBox("Specificity",
                                            value = textOutput("naive_specificity"),width = 5)
                                    )
                                   )
                                   ),
                          tabPanel(title = "Decision Tree",
                                   column(width = 4,
                                          infoBox("Prediction",
                                           value = textOutput(outputId = "dt_pred"), 
                                           subtitle = textOutput(outputId = "dt_yes"),
                                           width = 12,icon = icon("search"))),
                                   column(width = 8,
                                          fluidRow(
                                            valueBox("Precision", color = "green",
                                                     value = textOutput("dt_precision"),width = 5),
                                            valueBox("Accuracy",
                                                     value = textOutput("dt_accuracy"),width = 5)
                                          ),
                                          fluidRow(
                                            valueBox("Sensitivity",
                                                     value = textOutput("dt_sensitivity"),width = 5),
                                            valueBox("Specificity",
                                                     value = textOutput("dt_specificity"),width = 5)
                                          )
                                   )
                          ),
                          tabPanel(title = "Random Forest",
                                   column(width = 4,
                                   infoBox("Prediction",
                                           value = textOutput(outputId = "rf_pred"),
                                           subtitle = textOutput(outputId = "rf_yes"),  
                                           width = 12,icon = icon("search"))),
                                   column(width = 8,
                                          fluidRow(
                                            valueBox("Precision", color = "green",
                                                     value = textOutput("rf_precision"),width = 5),
                                            valueBox("Accuracy",
                                                     value = textOutput("rf_accuracy"),width = 5)
                                          ),
                                          fluidRow(
                                            valueBox("Sensitivity",
                                                     value = textOutput("rf_sensitivity"),width = 5),
                                            valueBox("Specificity",
                                                     value = textOutput("rf_specificity"),width = 5)
                                          )
                                   )
                          )
                      )
                      ),
                      
              tabItem(tabName = "data",
                      h2("Raw Data"),
                      br(),
              "This is a sales data from an individual company. Each row corresponds to a customer infomation",
                      dataTableOutput(outputId = "datadf"))
          )
        
            )
          )

server <- function(input,output){
  
  # Bar Chart Flag on overview ------------------------------------------
  output$bar_flag <- renderPlotly({

    if (input$bar_cat == "none") {
      dfd <- df %>% 
        group_by(flag) %>% 
        summarise(total = n())
      
      p <- dfd %>% 
        ggplot(aes(flag, total, fill = flag,
                   text = 
                   glue("Buy: {flag}
                        Frequency: {total}")))+
        labs(title = "Number of People Buying vs Not Buying Products",
             x = NULL, y = NULL)+
        geom_col()+
        scale_fill_fish_d("Centropyge_loricula")+
        coord_flip()+
        theme_minimal()
      
      ggplotly(p, tooltip = "text") %>% 
        layout(showlegend = F)
    }
    else {
      dfd <- df %>% 
        mutate(kol = get(input$bar_cat, df)) %>% 
        group_by(flag, kol) %>% 
        summarise(total = n())
      
      p <- dfd %>% 
        ggplot(aes(flag, total, fill = kol,
                   text = 
                   glue("Buy: {flag}
                   Category: {kol}
                    Frequency: {total}")
                     ))+
        labs(title = "Number of People Buying vs Not Buying Products",
             x = NULL, y = NULL)+
        geom_col(position = "fill")+
        scale_fill_fish_d("Centropyge_loricula")+
        coord_flip()+
        theme_minimal()
      
      ggplotly(p, tooltip = "text") %>% 
        layout(showlegend = F)
    }
    
    
  })
  
  
  # Alluvial Chart on Profiling -------------------------
  output$sankey_yes <- renderPlotly({
    ax1 <- get(input$axis1,df)
    ax2 <- get(input$axis2,df)
    ax3 <- get(input$axis3,df)
    ax4 <- get(input$axis4,df)

    df_sankey <- data.frame(ax1 = ax1,
                            ax2 = ax2,
                            ax3 = ax3,
                            ax4 = ax4,
                            flag = df$flag)
    df_sankey <- df_sankey %>% filter(flag == "Yes")
    
    ax12 <- df_sankey %>% 
      group_by(ax1, ax2) %>% 
      summarise(total = n()) %>% 
      ungroup()
    
    ax23 <- df_sankey %>% 
      group_by(ax2, ax3) %>% 
      summarise(total = n()) %>% 
      ungroup()
    
    ax34 <- df_sankey %>% 
      group_by(ax3, ax4) %>% 
      summarise(total = n()) %>% 
      ungroup()
    
    p <- plot_ly(
      type = "sankey",
      orientation = "h",
      
      node = list(
        label = c(levels(ax1), 
                  levels(ax2),
                  levels(ax3),
                  levels(ax4)),
        color = c(rep("blue",n_distinct(ax1)), rep("red", n_distinct(ax2)),
                  rep("green", n_distinct(ax3)), rep("yellow", n_distinct(ax4))),
        pad = 15,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      
      link = list(
        source = c(seq_rep(ax1,ax2)-1, 
                   seq_rep(ax2,ax3)-1+n_distinct(ax1),
                   seq_rep(ax3,ax4)-1+n_distinct(ax1)+n_distinct(ax2)),
        target = c(rep(1:n_distinct(ax2)+n_distinct(ax1)-1, n_distinct(ax1)),
                   rep(1:n_distinct(ax3)+n_distinct(ax1)+n_distinct(ax2)-1, n_distinct(ax2)),
                   rep(1:n_distinct(ax4)+n_distinct(ax1)+n_distinct(ax2)+n_distinct(ax3)-1, 
                       n_distinct(ax3))),
        value =  c(ax12$total, ax23$total, ax34$total)
      )
    )
    p
    })
  
  output$sankey_no <- renderPlotly({
    ax1 <- get(input$axis1,df)
    ax2 <- get(input$axis2,df)
    ax3 <- get(input$axis3,df)
    ax4 <- get(input$axis4,df)
    
    df_sankey <- data.frame(ax1 = ax1,
                            ax2 = ax2,
                            ax3 = ax3,
                            ax4 = ax4,
                            flag = df$flag)
    df_sankey <- df_sankey %>% filter(flag == "No")
    
    ax12 <- df_sankey %>% 
      group_by(ax1, ax2) %>% 
      summarise(total = n()) %>% 
      ungroup()
    
    ax23 <- df_sankey %>% 
      group_by(ax2, ax3) %>% 
      summarise(total = n()) %>% 
      ungroup()
    
    ax34 <- df_sankey %>% 
      group_by(ax3, ax4) %>% 
      summarise(total = n()) %>% 
      ungroup()
    
    p <- plot_ly(
      type = "sankey",
      orientation = "h",
      
      node = list(
        label = c(levels(ax1), 
                  levels(ax2),
                  levels(ax3),
                  levels(ax4)),
        color = c(rep("blue",n_distinct(ax1)), rep("red", n_distinct(ax2)),
                  rep("green", n_distinct(ax3)), rep("yellow", n_distinct(ax4))),
        pad = 15,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      
      link = list(
        source = c(seq_rep(ax1,ax2)-1, 
                   seq_rep(ax2,ax3)-1+n_distinct(ax1),
                   seq_rep(ax3,ax4)-1+n_distinct(ax1)+n_distinct(ax2)),
        target = c(rep(1:n_distinct(ax2)+n_distinct(ax1)-1, n_distinct(ax1)),
                   rep(1:n_distinct(ax3)+n_distinct(ax1)+n_distinct(ax2)-1, n_distinct(ax2)),
                   rep(1:n_distinct(ax4)+n_distinct(ax1)+n_distinct(ax2)+n_distinct(ax3)-1, 
                       n_distinct(ax3))),
        value =  c(ax12$total, ax23$total, ax34$total)
      )
    )
    p
  })
  
  
  # K-Prototypes Clustering -----------------
  
  observeEvent(input$go_cluster, {
    showModal(modalDialog(title = "Processing"))
    set.seed(123)
    df_clust <- kproto(df, k = input$cluster_k)
    showModal(modalDialog(title = "Finished"))
    
    output$cluster_sun <- renderPlotly({
      
      p <- df %>% 
        mutate(clusters = df_clust$cluster) %>% 
        filter(flag == "Yes") %>% 
        group_by(clusters, flag) %>% 
        summarise(Freq = n()) %>% 
        arrange(desc(Freq)) %>%
        ungroup() %>% 
        mutate(perc = Freq/sum(Freq),
               clusters = factor(clusters, levels = rev(unique(clusters)))) %>% 
        ggplot(aes(reorder(clusters, perc), perc))+
        geom_segment(aes(x = clusters, xend = clusters, y = 0, yend = perc))+
        geom_point(aes(size = perc, color = perc,
                       text = 
                         glue("Cluster: {clusters}
                 Frequency: {comma(Freq, accuracy = 1)}
                 Percentage: {percent(perc, accuracy = 0.01, scale = 100)}")), 
                   alpha = 0.6)+
        coord_flip()+
        scale_size_continuous(range = c(3,10))+
        scale_y_continuous(labels = number_format(scale = 100, accuracy = 1, suffix = "%"))+
        scale_color_viridis_c()+
        theme_minimal()+
        labs(title = "Number of People Who Bought Products on Each Cluster",
             x = "Cluster", y = NULL)+
        theme(axis.text = element_text(colour = "black"),
              legend.position = "none")
      
      ggplotly(p, tooltip = "text") %>% 
        hide_legend() %>% 
        config(displayModeBar = F)
    })
    
    output$cluster_treemap <- renderD3tree({
      
      x <- data.frame(axis = get(input$treemap_input, df)) %>% 
        mutate(cluster = as.factor(df_clust$cluster)) %>% 
        group_by(cluster, axis) %>% 
        summarise(freq = n(),
                  perc = n()/nrow(df)*100) %>% 
        arrange(axis, cluster)

      p <- treemap(x,
                   index=c("cluster","axis"),
                   vSize="freq",
                   type="index",
                   palette = "Set1",
                   bg.labels=c("white"),
                   align.labels=list(
                     c("center", "center"), 
                     c("right", "bottom")
                   )  
      )            
      
      # make it interactive ("rootname" becomes the title of the plot):
      d3tree(p ,  
             rootname = glue("{input$treemap_input}"))
      
    })
    
    output$cluster_table <- renderDataTable({
      
        df_clust$centers %>% 
        bind_cols(size = as.matrix(df_clust$size)) %>% 
        mutate(cluster = 1:input$cluster_k,
               house_val = round(house_val, digits = 3)) %>% 
        select(cluster, size, everything()) %>% 
        rename("house valuation" = house_val,
               "house ownership" = house_owner)
    }, 
    options = list(scrollX = TRUE))
    
    
  })
    
  
  # Random Forest Prediction -----------------
  
  observeEvent(input$start_predict, {
    inputo <- c(input$genderi, input$educationi, input$agei, input$onlinei, input$childreni,
                input$occupationi, input$marriagei, input$mortgagei, 
                input$housei, input$regioni, input$hoval)

    if (any(inputo == "")) {
      showModal(modalDialog("Please fill all the inputs first"))
      
      output$rf_precision <- renderText({
        print("")
      })
    }
    else {
      dft <- data.frame(gender = factor(input$genderi, levels = levels(df$gender)),
                        education = factor(input$educationi, levels = levels(df$education)),
                        age = factor(input$agei, levels = levels(df$age)),
                        online = factor(input$onlinei, levels = levels(df$online)),
                        child = factor(input$childreni, levels = levels(df$child)),
                        marriage = factor(input$marriagei, levels = levels(df$marriage)),
                        occupation = factor(input$occupationi, levels = levels(df$occupation)),
                        mortgage = factor(input$mortgagei, levels = levels(df$mortgage)),
                        house_owner = factor(input$housei, levels = levels(df$house_owner)),
                        region = factor(input$regioni, levels = levels(df$region)),
                        house_val = as.numeric(input$hoval)
                        )
      
      dft_naive <- dft %>% 
        mutate(house_val = as.factor(if_else(house_val >= mean(df$house_val), 
                                             "above_average", "below_average")))
      
      # RANDOM FOREST PREDICTION
      output$rf_pred <- renderText({
        refo <- predict(model, new_data = dft, type = "class")
        toupper(as.character(refo$.pred_class))
      })
      
      output$rf_yes <- renderText({
        ref_yes <- predict(model, new_data = dft, type = "prob")
        paste("Probability to Yes:",round(ref_yes$.pred_Yes,digits = 3))
      })
      
      output$rf_precision <- renderText({
        paste(round(perf_rf$precision*100,
                    digits = 2),"%")
      })
      
      output$rf_accuracy <- renderText({
        paste(round(perf_rf$accuracy*100,
                    digits = 2),"%")
      })
      
      output$rf_precision <- renderText({
        paste(round(perf_rf$precision*100,
                    digits = 2),"%")
      })
      
      output$rf_sensitivity <- renderText({
        paste(round(perf_rf$sensitivity*100,
                    digits = 2),"%")
      })
      
      output$rf_specificity <- renderText({
        paste(round(perf_rf$specificity*100,
                    digits = 2),"%")
      })
      
      # DECISION TREE PREDICTION
      output$dt_pred <- renderText({
        dto <- predict(dtree, newdata = dft, type = "class")
        toupper(as.character(dto))
      })
      
      output$dt_yes <- renderText({
        ref_yes <- predict(dtree, newdata = dft, type = "prob")
        paste("Probability to Yes:",round(ref_yes[1],digits = 3))
      })
      
      output$dt_precision <- renderText({
        paste(round(perf_dt$precision*100,
                    digits = 2),"%")
      })
      
      output$dt_accuracy <- renderText({
        paste(round(perf_dt$accuracy*100,
                    digits = 2),"%")
      })
      
      output$dt_precision <- renderText({
        paste(round(perf_dt$precision*100,
                    digits = 2),"%")
      })
      
      output$dt_sensitivity <- renderText({
        paste(round(perf_dt$sensitivity*100,
                    digits = 2),"%")
      })
      
      output$dt_specificity <- renderText({
        paste(round(perf_dt$specificity*100,
                    digits = 2),"%")
      })
      
      # NAIVE BAYES PREDICTION
      output$naive_pred <- renderText({
        bay_pred <- predict(bayes_mod, newdata = dft_naive, type = "class")
        toupper(as.character(bay_pred))
      })
      
      output$naive_yes <- renderText({
        bay_yes <- predict(bayes_mod, newdata = dft_naive, type = "raw")
        paste("Probability to Yes:",round(bay_yes[1],digits = 3))
      })
      
      output$naive_precision <- renderText({
        paste(round(perf_bayes$precision*100,
                    digits = 2),"%")
      })
      
      output$naive_accuracy <- renderText({
        paste(round(perf_bayes$accuracy*100,
                    digits = 2),"%")
      })
      
      output$naive_precision <- renderText({
        paste(round(perf_bayes$precision*100,
                    digits = 2),"%")
      })
      
      output$naive_sensitivity <- renderText({
        paste(round(perf_bayes$sensitivity*100,
                    digits = 2),"%")
      })
      
      output$naive_specificity <- renderText({
        paste(round(perf_bayes$specificity*100,
                    digits = 2),"%")
      })
      
    }
  })
  
  # Machine Learning Performance -------------------
  output$perf_bayes <- renderTable({
    perf_bayes
    })
  
  output$perf_rf <- renderTable({
    perf_rf
  })
  
  output$perf_dt <- renderTable({
    perf_dt
  })
  
  # Dataset --------------------
  output$datadf <- renderDataTable({
    df
  })
  
}

shinyApp(ui, server)


