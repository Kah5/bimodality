#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("PLS density"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
      
        sliderInput("precip",
                      "Precipitation",
                      min = 200,
                      max = 1350,
                      value = c(250,400)),

      
        sliderInput("sand",
                      "Sand Percentage",
                     min = 0,
                     max = 100,
                    value = c(0,100)),
        sliderInput("BC_interval",
                    "Bimodality Coefficient Binwidth",
                    min = 0,
                    max = 1150,
                    value = 150)
   
      ),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         #plotOutput('densityPlot'),
         plotOutput('mapPlot'),
         plotOutput('precipPlot'),
         plotOutput('bimodalPlot')
         
      )
   )
)
)


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      
      filtered <-
        dens.pr %>%
        filter(MAP1910 >= input$precip[1],
               MAP1910 <= input$precip[2],
               sandpct >= input$sand[1],
               sandpct <= input$sand[2]
        )
      x    <- filtered$PLSdensity 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, probability = TRUE ,breaks = bins, col = 'darkgray', border = 'white')
      lines(density(x), col = 'darkblue')
         
    
   }) 
   output$precipPlot <- renderPlot({
     # generate bins based on input$bins from ui.R
     
     filtered <-
       dens.pr %>%
       filter(MAP1910 >= input$precip[1],
              MAP1910 <= input$precip[2],
              sandpct >= input$sand[1],
              sandpct <= input$sand[2]
       )
    
     
     # draw the histogram with the specified number of bins
     ggplot(filtered, aes(MAP1910, PLSdensity))+geom_point()
     
     
   }) 
   output$mapPlot <- renderPlot({
     # generate bins based on input$bins from ui.R
     
     filtered <-
       dens.pr %>%
       filter(MAP1910 >= input$precip[1],
              MAP1910 <= input$precip[2],
              sandpct >= input$sand[1],
              sandpct <= input$sand[2]
       )
     cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
     ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
       geom_raster(data=filtered, aes(x=x, y=y, fill = PLSdensity))+
       geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
       labs(x="easting", y="northing", title="PLS tree density") + 
       scale_fill_gradientn(colours = cbpalette, limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
       coord_equal()+theme_bw()
     
     
   }) 
   output$densityPlot <- renderPlot({filtered <-
     dens.pr %>%
     filter(MAP1910 >= input$precip[1],
            MAP1910 <= input$precip[2],
            sandpct >= input$sand[1],
            sandpct <= input$sand[2]
     )
   plot(density(filtered$PLSdensity, kernel="gaussian"), col = 'blue')
   lines(density(filtered$PLSdensity, kernal = 'biweight'), col = 'grey')
   
   })
   
   output$bimodalPlot <- renderPlot({filtered <-
     dens.pr %>%
     filter(MAP1910 >= input$precip[1],
            MAP1910 <= input$precip[2],
            sandpct >= input$sand[1],
            sandpct <= input$sand[2]
     )
      
      ordered <- filtered[order(filtered$MAP1910),]
      
      rollBC_by_10_r = function(x,y,xout,width) {
        out = 1:length(seq(200, 1350, by = 10) )
        for( i in 1:length(seq(200, 1350, by = 10))) {
          window = x >= (xout[i]-width) & x <= (xout[i]+width)
          out[i] = bimodality_coefficient( y[window] )
        }
        ggplot()+geom_point(aes(x = xout, y = out))+
          geom_hline( yintercept = 5/9)+ylim(0,1)+theme_bw()+
          xlab('interval center') + ylab('Bimodality Coefficient') +ggtitle(paste0( 'Bimodality coefficient for binwidth = ', width))
      }   
      
      rollBC_r(ordered$MAP1910, ordered$PLSdensity, seq(200, 1350, by = 10) , input$BC_interval)
})
   output$bimodalMAP <- renderPlot({filtered <-
     dens.pr %>%
     filter(MAP1910 >= input$precip[1],
            MAP1910 <= input$precip[2],
            sandpct >= input$sand[1],
            sandpct <= input$sand[2]
     )
   
   ordered <- filtered[order(filtered$MAP1910),]
   
   rollBC_by_10_r = function(x,y,xout,width) {
     out = 1:length(seq(200, 1350, by = 10) )
     for( i in 1:length(seq(200, 1350, by = 10))) {
       window = x >= (xout[i]-width) & x <= (xout[i]+width)
       out[i] = bimodality_coefficient( y[window] )
     }
     
     
     ggplot()+geom_point(aes(x = xout, y = out))+
       geom_hline( yintercept = 5/9)+ylim(0,1)+theme_bw()+
       xlab('interval center') + ylab('Bimodality Coefficient') +ggtitle(paste0( 'Bimodality coefficient for binwidth = ', width))
   }   
   
   rollBC_by_10_r(ordered$MAP1910, ordered$PLSdensity, seq(200, 1350, by = 10) , input$BC_interval)
   })
})
# Run the application 
shinyApp(ui = ui, server = server)

