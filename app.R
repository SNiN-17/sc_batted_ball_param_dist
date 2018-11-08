require(cowplot)
library(shiny)
require(DT)
require(baseballr)
require(tidyverse)
library(e1071) # skewnessの計算に使用

data <- read_csv("https://github.com/SNiN-17/sc_batted_ball_param_dist/raw/master/SC_data_for_SC_bb_param_dist.csv")

# Classify batted balls into 3 categories based on the hand_adj_spray variable
data <- data %>%
  mutate(Pull_FL = ifelse(hand_adj_spray <= -17, 1,0),
         Oppo_FL = ifelse(hand_adj_spray >= 17, 1,0),
         Cent_FL = ifelse(hand_adj_spray < 17 & hand_adj_spray > -17, 1,0),
         Spray_type = ifelse(hand_adj_spray <= -17, "Pull",
                             ifelse(hand_adj_spray >= 17, "Oppo", "Cent")))
data$Spray_type <- factor(data$Spray_type,levels = c("Pull", "Cent", "Oppo"))

# Make a variable indicating year.
data$Year <- str_sub(data$game_date, start = 1L, end = 4L)

# Player lists for selectInput()
players <- unique(data$name)

# Define UI ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Statcast batted ball parameter distribution app."),
  
  # Define the sidebar
    sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # some comments
      helpText("Statcast dataから各選手のlauch angle, launch speedの分布などを可視化する."),
      helpText("MLB16-18 レギュラーシーズンでlauch angle, speedが記録 (or推定) されているデータのみ.ファウルを除外し200以上の打球が記録された選手が対象.バントもだいたい除いているはず."),
      helpText("Statcast data are property of MLBAM. Data were downroaded from baseballsavant by using the baseballr package."),
      # parameter selection
      radioButtons("stat", "Variable type:",
                   c("Launch angle" = "Angle",
                     "Launch speed" = "Velocity",
                     "Handedness-adjusted spray angle" = "Handedness-adjusted spray angle")),
      helpText("Handedness-adjusted spray angle <= -17がpull. hc_xとhc_yから計算."),
      br(),
      # input widgets for hitter's names
      # textInput("name", label = "Enter name 1 (or All):", value = "Votto, Joey"),
      # textInput("name2", label = "Enter name 2 for comparison:", value = "Schimpf, Ryan"),
      selectInput("selected_name", "Hitter 1:", 
                  choices=players,
                  selected = "Votto, Joey"),
      selectInput("selected_name2", "Hitter 2:", 
                  choices=players,
                  selected = "Stanton, Giancarlo"),

      
      # Time period. input$dates[1] or [2]
      dateRangeInput("dates", "Date範囲:",
                     start = "2016-04-06",
                     end   = "2018-10-31"),
      
      numericInput("bin_width", "ヒストグラムのbin幅:", "3", min = 1, max = 10, step = 1),
      
      width = 3
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset for a summary table and plots ----
      tabsetPanel(type = "tabs",
                  tabPanel("Leaders", DT::dataTableOutput("mytable")),
                  tabPanel("Hitter 1 (splits)", plotOutput("hist")),
                  tabPanel("Hitter 1 & 2 (Histogram)", plotOutput("hist2")),
                  tabPanel("Hitter 1 & 2 (Density plot)", plotOutput("dens")),
                  tabPanel("Reference", htmlOutput("refs"))
      )
    )
  )
)

# Define server logic  ----
server <- function(input, output) {
  
  # inputs: 
  # input$stat, input$selected_name, input$selected_name2, 
  # input$dates, input$bin_width
  
  # Initial parameter selection
  # The col of the selected stat will be named as "stat"
  stats.df <-  reactive({
    
    if(input$stat == "Angle"){
      names(data)[3] <- "stat"
    }else if(input$stat == "Velocity"){
      names(data)[4] <- "stat"
    }else{
      names(data)[7] <- "stat"
    }
    
    data <- data %>% 
      filter(game_date >= input$dates[1], game_date <= input$dates[2])
    
    return(data)
  })
  
  
  # player1はallを受け付ける
  # d <- reactive({
  #   if(input$name == "All"){
  #     stats.df()
  #   }else{
  #     stats.df() %>% filter(name == input$name)
  #   }
  # })
  d <- reactive({
    stats.df() %>% filter(name == input$selected_name)
  })
  
  # Generate leaders table ----
  output$mytable <- DT::renderDataTable({
    data <- stats.df()
    table <- dplyr::summarise(group_by(data%>% 
                                         filter(is.na(bb_type) == FALSE), name),
                              BBE = n(),
                              wOBAcon = round(sum(woba_value, na.rm = TRUE) / BBE,3),
                              # `Barrel/BBE` = round(sum(barrel, na.rm = TRUE) / BBE,3),
                              mean = round(mean(stat, na.rm = TRUE),1),
                              median = round(median(stat, na.rm = TRUE),1),
                              sd = round(sd(stat, na.rm = TRUE),1),
                              skewness = round(skewness(stat, na.rm = TRUE), 3))%>%
      rename(Player = name)%>%
      arrange(desc(wOBAcon))
    DT::datatable(table, 
                  options = list(# lengthMenu = c(100, 500, 2000), 
                    pageLength = 1000))
  })
  
  # Generate plots for hitter1 (splits) ----
  output$hist <- renderPlot({ 
    data <- d()
    
    # 図のxlim用パラメータ
    if(input$stat == "Velocity"){
      upper2 <- 140
      lower2 <- 0
    }else if(input$stat == "Angle"){
      upper2 <- 90
      lower2 <- -90
    }else{
      upper2 <- 90
      lower2 <- -90
    }
    
    # レジェンドを設定
    text1 <- paste(input$selected_name, ", N=", nrow(data %>% filter(is.na(stat) == FALSE)),".")
    text2 <- paste("Mean=", 
                   round(mean(data$stat, na.rm = TRUE),1), 
                   ", median=", round(median(data$stat, na.rm = TRUE),1), 
                   ", sd=", round(sd(data$stat, na.rm = TRUE),1),
                   ", skewness=", round(skewness(data$stat, na.rm = TRUE),3),
                   ".")
    
    hist1 <- ggplot(data, aes(x = stat, y = ..density..)) +
      geom_histogram(alpha = 0.7, binwidth = input$bin_width, colour="black", fill="gray")+ # 
      # xlim(0, 1)+
      theme_bw() +
      theme(axis.text.x = element_text(size=15),
            axis.text.y = element_text(size=15),
            axis.title = element_text(size = 18),
            title = element_text(size = 15)) +
      labs( title = text1, subtitle = text2,
            x = input$stat, y = "Prob")
    
    
    plot2 <- ggplot(data, aes(
      x = stat,          
      # colour = name,   
      fill = Year     
    )) +
      geom_density(
        stat = "density",
        position = "identity",
        alpha = 0.5,
        adjust = 1)+ 
      xlim(lower2, upper2)+
      theme_bw() +
      theme(axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            axis.title = element_text(size = 10),
            title = element_text(size = 12)) +
      labs(# subtitle = text2,
            x = input$stat, y = "Density", fill = "Year")
    
    # 打球タイプごと 
    plot3 <- ggplot(data, aes(
      x = stat,          
      # colour = name,   
      fill = bb_type     
    )) +
      geom_density(
        stat = "density",
        position = "identity",
        alpha = 0.5,
        adjust = 1)+ 
      xlim(lower2, upper2)+
      theme_bw() +
      theme(axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            axis.title = element_text(size = 10),
            title = element_text(size = 12)) +
      labs(# subtitle = text2,
        x = input$stat, y = "Density", fill = "bb_type")
    

    # 打球方向ごと
    plot4 <- data%>%
      filter(is.na(Spray_type) == 0)%>%
      ggplot(aes(
      x = stat,          
      # colour = name,   
      fill = Spray_type     
    )) +
      geom_density(
        stat = "density",
        position = "identity",
        alpha = 0.5,
        adjust = 1)+ 
      xlim(lower2, upper2)+
      theme_bw() +
      theme(axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            axis.title = element_text(size = 10),
            title = element_text(size = 12)) +
      labs(# subtitle = text2,
        x = input$stat, y = "Density", fill = "Spray")
    

    # plotをつなげる
    plot_grid(hist1, plot2, plot3, plot4, ncol = 1,
              labels="AUTO", # labels = c("B", "C"), 
              scale = 0.9) # , scale = 0.9
    
  }, height = 1200, width = 700, res = 90)
  
  #  2選手を比較するhist 2 ----
  output$hist2 <- renderPlot({ 
    
    # 一人目のdata
    data1 <- d()
    
    # 二人目のdata
    data2 <- stats.df() %>% filter(name == input$selected_name2)
    # 図のxlim用パラメータ
    if(input$stat == "Velocity"){
      upper <- 130
      lower <- 0
    }else if(input$stat == "Angle"){
      upper <- 90
      lower <- -90
    }else{
      upper <- 60
      lower <- -60
    }
    
    # レジェンドを設定
    text1 <- paste(input$selected_name, ", N=", length(data1$name),".")
    text2 <- paste("Mean=", 
                   round(mean(data1$stat),1), 
                   ", median=", round(median(data1$stat),1), 
                   ", sd=", round(sd(data1$stat),1),
                   ", skewness=", round(skewness(data1$stat),3),
                   ".")
    
    text3 <- paste(input$selected_name2, ", N=", length(data2$name),".")
    text4 <- paste("Mean=", 
                   round(mean(data2$stat),1), 
                   ", median=", round(median(data2$stat),1), 
                   ", sd=", round(sd(data2$stat),1),
                   ", skewness=", round(skewness(data2$stat),3),
                   ".")
    
    hist1 <- ggplot(data1, aes(x = stat, y = ..density..)) +
      geom_histogram(alpha = 0.7, binwidth = input$bin_width, colour="black", fill="gray")+ # 
      xlim(lower, upper)+
      theme_bw() +
      theme(axis.text.x = element_text(size=15),
            axis.text.y = element_text(size=15),
            axis.title = element_text(size = 18),
            title = element_text(size = 15)) +
      labs( title = text1, subtitle = text2,
            x = input$stat, y = "Prob")
    
    hist2 <- ggplot(data2, aes(x = stat, y = ..density..)) +
      geom_histogram(alpha = 0.7, binwidth = input$bin_width, colour="black", fill="gray")+ # 
      xlim(lower, upper)+
      theme_bw() +
      theme(axis.text.x = element_text(size=15),
            axis.text.y = element_text(size=15),
            axis.title = element_text(size = 18),
            title = element_text(size = 15)) +
      labs( title = text3, subtitle = text4,
            x = input$stat, y = "Prob")
    
    plot_grid(hist1, hist2, ncol = 1,
              labels="AUTO", # labels = c("B", "C"), 
              scale = 0.9) # , scale = 0.9
    
  }, height = 600, width = 700, res = 90)
  
  #  2選手を比較するdensitiy plot ----
  output$dens <- renderPlot({ 
    
    # 図のxlim用パラメータ
    if(input$stat == "Velocity"){
      upper2 <- 140
      lower2 <- 0
    }else if(input$stat == "Angle"){
      upper2 <- 90
      lower2 <- -90
    }else{
      upper2 <- 90
      lower2 <- -90
    }
    
    # 一人目のdata
    data1 <- d()
    
    # 二人目のdata
    data2 <- stats.df() %>% filter(name == input$selected_name2)
    data <- rbind(data1, data2)
    
    text <- paste(data1$name[1], "vs", data2$name[1])
    
    ggplot(data, aes(
      x = stat,          
      # colour = name,   
      fill = name      
    )) +
      geom_density(
        stat = "density",
        position = "identity",
        alpha = 0.5,
        adjust = 1)+ 
      xlim(lower2, upper2)+
      theme_bw() +
      theme(axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            axis.title = element_text(size = 10),
            title = element_text(size = 12)) +
      labs( title = text, 
            # subtitle = text2,
            x = input$stat, y = "Density", fill = "Player")
    
  }, height = 400, width = 600, res = 120)
  
  # 参考文献等 ----
  output$refs <- renderUI({
    str1 <- "[1] basevallsavant https://baseballsavant.mlb.com/statcast_leaderboard"
    str2 <- "[2] baseballr https://billpetti.github.io/baseballr/"
    str3 <- "[3] 打球方向の計算 https://baseballwithr.wordpress.com/2018/01/22/spray-charts-from-statcast-data/"
    str4 <- "[4] 中の人 https://twitter.com/sleep_in_nmbrs"
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
  })
  
}



# Create Shiny app ----
shinyApp(ui, server)