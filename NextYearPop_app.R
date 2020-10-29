##############################################################################################################################
##############################################################################################################################
##R CODE FOR POP NEXT YEAR SHINY APP
##
##EDDIE HUNSINGER (AFFILIATION: ALASKA DEPARTMENT OF LABOR AND WORKFORCE DEVELOPMENT), MARCH 2017 (UPDATED OCTOBER 2020)
##http://u.demog.berkeley.edu/~eddieh/
##edyhsgr@gmail.com
##
##THIS IS BASED ON R CODE AVAILABLE AT: https://raw.githubusercontent.com/AppliedDemogToolbox/Hunsinger_NextYearPop/master/NextYearPop.R
##
##THERE IS NO WARRANTY FOR THIS CODE
##THIS CODE HAS NOT BEEN CAREFULLY REVIEWED
##############################################################################################################################
##############################################################################################################################

library(shiny)
ui<-fluidPage(

	tags$h3("Next Year Population Possibilities Reviewer"),
	p("Related information and ",
	tags$a(href="https://www.r-project.org/", "R"),
	"code available at: ",
	tags$a(href="https://raw.githubusercontent.com/AppliedDemogToolbox/Hunsinger_NextYearPop/master/NextYearPop.R", 
	"Applied Demography Toolbox Google+ post")

),
  
hr(),

sidebarLayout(
sidebarPanel(

	numericInput(inputId = "StartingPop",	
		label = "Put the starting population estimate here",
		value = 10000, min = 0, max = 10000000000,step=100),

 sliderInput("Births", 
                label = "Births as a share of the starting population (uniform distribution bounds)",
                min = 0, max = 3, post  = "%", value = c(1.1,1.5),step=.01),

 sliderInput("Deaths", 
                label = "Deaths as a share of the starting population (uniform distribution bounds)",
                min = 0, max = 1.5, post  = "%", value = c(.7,.85),step=.01),

 sliderInput("InMigration", 
                label = "In-migration as a share of the starting population (uniform distribution bounds)",
                min = 0, max = 10, post  = "%", value = c(2.5,4),step=.1),
	
 sliderInput("OutMigration", 
                label = "Out-migration as a share of the starting population (uniform distribution bounds)",
                min = 0, max = 10, post  = "%", value = c(2.5,3.5),step=.1),

tags$small(paste0(        
	"This interface was made with Shiny for R (shiny.rstudio.com).       
	Eddie Hunsinger, March 2017 (updated October 2020)."
	)),

width=3
),

mainPanel(
	
	plotOutput("plots")
))
)

server<-function(input, output) {	
	output$plots<-renderPlot({
par(mfrow=c(1,1))

##############################################################################################################################
##############################################################################################################################
# Number of iterations
iter<-100000

# Starting population
StartingPop<-input$StartingPop

# Components of change for the period (year) 
# Uniform distribution guesses for components (iter, low bound, highbound)
Deaths<-runif(iter,input$Deaths[1]/100,input$Deaths[2]/100)
Births<-runif(iter,input$Births[1]/100,input$Births[2]/100)
InMigration<-runif(iter,input$InMigration[1]/100,input$InMigration[2]/100)
OutMigration<-runif(iter,input$OutMigration[1]/100,input$OutMigration[2]/100)

# Sampling
NextYearPop<-array(0,iter)
for(i in 1:iter){NextYearPop[i]<-(StartingPop - Deaths[i]*StartingPop 
+ Births[i]*StartingPop - OutMigration[i]*StartingPop + InMigration[i]*StartingPop)}

# Output
#quantile(NextYearPop-StartingPop,c(.005,.05,.1,.25,.5,.75,.90,.95,.995))
#quantile(NextYearPop,c(.005,.05,.1,.25,.5,.75,.90,.95,.995))

#hist(NextYearPop-StartingPop)

hist(NextYearPop,50,main=paste("Histogram of Next Year Population (100,000 possibles)"),xlab = "Next Year Population",col="lightgreen")

mtext(side=1,line=-31,adj=.025,text="Min: ",font=1,cex=1,col=1)
mtext(side=1,line=-31,adj=.25,text=round(quantile(NextYearPop,0),0),font=1,cex=1,col=1)
mtext(side=1,line=-30,adj=.025,text="5th Percentile: ",font=1,cex=1,col=1)
mtext(side=1,line=-30,adj=.25,text=round(quantile(NextYearPop,.05),0),font=1,cex=1,col=1)
mtext(side=1,line=-29,adj=.025,text="25th Percentile: ",font=1,cex=1,col=1)
mtext(side=1,line=-29,adj=.25,text=round(quantile(NextYearPop,.25),0),font=1,cex=1,col=1)
mtext(side=1,line=-28,adj=.025,text="Median: ",font=1,cex=1,col="red")
mtext(side=1,line=-28,adj=.25,text=round(quantile(NextYearPop,.5),0),font=1,cex=1,col="red")
mtext(side=1,line=-27,adj=.025,text="75th Percentile: ",font=1,cex=1,col=1)
mtext(side=1,line=-27,adj=.25,text=round(quantile(NextYearPop,.75),0),font=1,cex=1,col=1)
mtext(side=1,line=-26,adj=.025,text="95th Percentile: ",font=1,cex=1,col=1)
mtext(side=1,line=-26,adj=.25,text=round(quantile(NextYearPop,.95),0),font=1,cex=1,col=1)
mtext(side=1,line=-25,adj=.025,text="Max: ",font=1,cex=1,col=1)
mtext(side=1,line=-25,adj=.25,text=round(quantile(NextYearPop,1),0),font=1,cex=1,col=1)
},height=600,width=600)
		
}

shinyApp(ui = ui, server = server)

