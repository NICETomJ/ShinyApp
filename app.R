library("rhandsontable")
library("shiny")
library("ggplot2")
library("data.table")
library("shinyjs")

DF<-data.table(Parameter=c("ComplicationCost", "TreatmentCostControl", "ComplicationRateControl", "TreatmentCostTreatment", "ComplicationRateTreatment"),Distribution= c("Normal", "Normal", "Beta", "Normal", "Beta"),PSAparameter1= c(300, 300, 11, 350, 13), PSAparameter2=c(50, 50, 91, 50, 87))

TextDist<-data.table(Distribution=c("Normal","Lognormal","Beta","Uniform","Triangular"),Rcommand=c("rnorm","rlnorm","rbeta","runif","rtri"))

GetPSAValues<-function(Dataset,n)
{
    Dataset<-as.data.table(merge(Dataset,TextDist,by="Distribution",all.x=T,all.y=F))
    Dataset[Distribution%in% c("Normal","Lognormal","Beta","Uniform"),SampleCommand:=paste0(Rcommand,"(",n,",",PSAparameter1,",",PSAparameter2,")")]
    Dataset[Distribution%in% c("Triangular"),SampleCommand:=paste0(Rcommand,"(",n,",",PSAparameter1,",",PSAparameter3,",",PSAparameter2,")")]
    
    OutputTable<-data.table(matrix(rep(0,nrow(Dataset)*n),nrow=n))
    names(OutputTable)<-Dataset$Parameter
    for (i in names(OutputTable))
    {OutputTable[[i]]<-eval(parse(text=Dataset[Parameter==i]$SampleCommand))}
    
    
    
    return(OutputTable)
}



ui <- shinyUI(fluidPage(
    
    titlePanel("Interactive R Shiny Economic Model"),
    sidebarLayout(
        sidebarPanel(
            helpText("Example"),
            
            wellPanel(
                h3("Table options"),
                actionButton("rerunmodel", "Run Model")
            ),
            br(), 
            
            wellPanel(
                h3("Reset options"), 
                actionButton("reset", "Reset Defaults")
            )        
            
        ),
        
        mainPanel(
            
            rHandsontableOutput("hot"),
            plotOutput("graph")
            
        )
    )
))

server <- shinyServer(function(input, output) {
    
    values <- reactiveValues()
    observeEvent(input$reset,{
        
        values$DF<- DF # will this reset?
    },ignoreInit = T)
    
    
    
    
    
    
    ## Handsontable
    observe({
        if (!is.null(input$hot)) {
            DF = hot_to_r(input$hot)
        } else {
            if (is.null(values[["DF"]]))
                DF <- DF
            else
                DF <- values[["DF"]]
        }
        values[["DF"]] <- DF
    })
    
    output$hot <- renderRHandsontable({
        DF <- values[["DF"]]
        if (!is.null(DF))
            rhandsontable(DF, useTypes = 1, stretchH = "all")
    })
    
    ##insert a graph
    
    data <- eventReactive(input$rerunmodel, {
        
        #oldw <- getOption("warn")
        #options(warn = -1)
        
        
        n<-1000
        Dataset<-as.data.table(merge(values$DF,TextDist,by="Distribution",all.x=T,all.y=F))
        Dataset[Distribution%in% c("Normal","Lognormal","Beta","Uniform"),SampleCommand:=paste0(Rcommand,"(",n,",",PSAparameter1,",",PSAparameter2,")")]
        Dataset[Distribution%in% c("Triangular"),SampleCommand:=paste0(Rcommand,"(",n,",",PSAparameter1,",",PSAparameter3,",",PSAparameter2,")")]
        
        OutputTable<-data.table(matrix(rep(0,nrow(Dataset)*n),nrow=n))
        names(OutputTable)<-Dataset$Parameter
        for (i in names(OutputTable))
        {OutputTable[[i]]<-eval(parse(text=Dataset[Parameter==i]$SampleCommand))}
        
        
        ## now run the actual model
        #assume complications are worth 2 QALYS
        OutputTable[,TotalCostTreatment:=TreatmentCostTreatment+ComplicationRateTreatment*ComplicationCost]
        OutputTable[,TotalCostControl:=TreatmentCostControl+ComplicationRateControl*ComplicationCost]
        OutputTable[,TotalQALYControl:=(1-ComplicationRateControl)*1] # assume 0 QALYs if complication, 1 if no complication
        OutputTable[,TotalQALYTreatment:=(1-ComplicationRateTreatment)*1] # assume 0 QALYs if complication, 1 if no complication
        
        
        CostQALYoutput<-data.table(Intervention=character(),TotalCost=numeric(),QALYs=numeric())
        for (treatment in c("Control","Treatment"))
        {
            selectvec<-c(paste0(c("TotalCost","TotalQALY"),treatment))
            keep<-OutputTable[,selectvec,with=F]
            setnames(keep,c("TotalCost","QALYs"))
            keep$Intervention<-treatment
            
            CostQALYoutput<-rbind(CostQALYoutput,keep)
        }
        #options(warn = oldw)
        
        return(CostQALYoutput)
    })
    
    
    
    output$graph<-renderPlot({
        ggplot(data(),aes(TotalCost,QALYs,colour=Intervention))+geom_point()})
    
    
    ## Save 
    
    
})





shinyApp(ui, server)