
library(shiny) 
library(shinythemes)

#ui kod [baslangic]


ui <- fluidPage(theme=shinytheme("journal"),
                h1("Merkezi Limit Teoremi - CLT"),   #baslik
                
                #sidebar baslangic
                
                sidebarLayout(
                  
                  sidebarPanel(
                    
                    #sidebar input kod
                    sliderInput(inputId = "n",
                                label = "Normallik testi Icin Orneklem Genisligini Seciniz",
                                min = 1, max = 50, value = 15,
                                animate = T),
                    
                    sliderInput(inputId ="p_buyukluk",
                                label = "Popusyon Buyuklugu Seciniz",
                                value = 2000, min = 1000, max = 100000, animate = T),
                    
                    
                    textInput(inputId ="o_buyukluk", 
                              label = "Orneklem Buyuklugu Seciniz",
                              value = 50),
                    
                    
                    selectInput(inputId ="dagilim", 
                                label = "Dagilim Seciniz ",
                                choices = c("Duzgun" = "unif",
                                            "Normal" = "norm", 
                                            "Ustel" = "exp",
                                            "Beta" = "beta",
                                            "Ki-kare" = "kikare",
                                            "F" = "f",
                                            "T" = "t",
                                            "Gamma" = "gamma",
                                            "Lognormal" = "logn",
                                            "Weibull" = "weibull"
                                ), 
                                
                                selected = "norm"),
                    
                    sliderInput(inputId ="o_yineleme",
                                label = "Orneklem Yinelemesi",
                                value = 200, min = 100, max = 1000, animate = T)
                    
                  ),
                  #sidebar kod [son]
                  
                  #main panel kod [baslangic]
                  
                  mainPanel(
                    
                    tabsetPanel(
                      
                      #plot tab
                      
                      tabPanel("Grafik",
                               plotOutput("plot_p"),   
                               plotOutput("plot_o_ort")),  
                      
                      #data tab
                      
                      tabPanel("Data", 
                               h4("Populasyon"), 
                               verbatimTextOutput("p_summary"), 
                               verbatimTextOutput("p_yineleme"), 
                               h4("Orneklem"), 
                               verbatimTextOutput("o_ort_summary"), 
                               verbatimTextOutput("o_ort_yineleme")),
                      
                      tabPanel("Normallik Testi",
                               
                               plotOutput("hist"),
                               
                               textOutput("hipotez0"),
                               
                               textOutput("hipotez1"),
                               
                               textOutput("shapiro"),
                               
                               plotOutput("qq"))
                      
                      
                      
                      
                    )
                    
                    
                  )
                )
                
                #main panel kod[son]
)
#ui kod [son]

#server side kod [baslangic]

server <- function(input, output, session){
  
  sayi <- reactive({
    as.numeric(input$n)
  })
  
  data <- reactive({
    as.numeric(rpois(n = sayi(), lambda = 3))
  })
  
  output$hist <- renderPlot({
    hist(data(),col = "Navy blue" )
  })
  
  output$hipotez0 <- renderText({
    paste("H0 : Dagilim Normaldir")
  })
  
  output$hipotez1 <- renderText({
    paste("H1 : Dagilim Normal Degildir")
  })
  
  output$shapiro <- renderPrint({
    shapiro.test(data())
  })
  
  output$qq <- renderPlot({
    qqnorm(data())
    qqline(data())
  })
  
  
  
  
  populasyon <- reactive({
    
    
    
    if (input$dagilim == "norm") {rnorm(input$p_buyukluk)} 
    else if (input$dagilim == "unif") {runif(input$p_buyukluk)} 
    else if (input$dagilim == "exp") {rexp(input$p_buyukluk)} 
    else if (input$dagilim == "beta") {rbeta(input$p_buyukluk,shape=5, shape2=1, ncp = 0)}
    else if (input$dagilim == "kikare") {rchisq(input$p_buyukluk,df=10)}
    else if (input$dagilim == "f") {rf(input$p_buyukluk, df1=2, df2=12)}
    else if (input$dagilim == "t") {rt(input$p_buyukluk, df=5, ncp=0)}
    else if (input$dagilim == "gamma") {rgamma(input$p_buyukluk, shape=5, rate = 1, scale = 1)}
    else if (input$dagilim == "logn") {rlnorm(input$p_buyukluk, meanlog = 0, sdlog = 1)}
    else if (input$dagilim == "weibull") {rweibull(input$p_buyukluk,shape=1, scale = 1)}
    
    
    
  })
  
  
  
  o_ort <- reactive({ 
    
    for (i in 1:input$o_yineleme) {
      if (i==1) {
        o_ort <- c(mean(sample(populasyon(), input$o_buyukluk, replace = TRUE ))) 
      } else {
        o_ort <- c(o_ort,mean(sample(populasyon(), input$o_buyukluk, replace = TRUE ))) 
      }
    }
    
    o_ort 
    
  })
  
  
  
  
  output$p_summary <- renderPrint({summary(populasyon())})
  output$p_yineleme <- renderPrint({str(populasyon())})
  output$o_ort_summary <- renderPrint({summary(o_ort())})
  output$o_ort_yineleme <- renderPrint({str(o_ort())})
  
  
  
  
  output$plot_p <-renderPlot({
    
    plot(density(populasyon()), axes=FALSE, xlab="", ylab="", main="",lwd=2,col="blue") 
    par(new=TRUE)  
    hist(populasyon(),main="Populasyon histogrami ve yogunluk grafigi", xlab = "") 
    abline(v = mean(populasyon()),col = "red",  lwd=2)  
    
  })
  
  
  
  output$plot_o_ort <-renderPlot({
    
    plot(density(o_ort()),axes=FALSE,xlab="",ylab="",main="",lwd=2,col="blue")
    par(new=TRUE)
    hist(o_ort(),main="Ornek ortalama histogrami ve yogunluk grafigi", xlab = "")
    abline(v = mean(o_ort()), col = "red", lwd = 2)
    
  })
  
  
  
  
  
  
}

shinyApp(ui, server)
