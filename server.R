#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

library(shiny)
library(dplyr)
library(ggplot2)

# Define server logic required
shinyServer(function(input, output) {

  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath, na.strings = c(""," ",NA), stringsAsFactors = FALSE)
  })
   
  output$summary <- renderText({
    str1 <- paste("Summary of dataset:")
    str2 <- paste("Feature 1. Loan_ID: Unique loan ID of the applicant.")
    str3 <- paste("Feature 2. Gender: Gender (Male/Female).")
    str4 <- paste("Feature 3. Married: Marital status (Y/N).")
    str5 <- paste("Feature 4. Dependents: Number of dependents (0/1/2/3+).")
    str6 <- paste("Feature 5. Education: Education level (Graduate/ NotGraduate).")
    str7 <- paste("Feature 6. Self_Employed: Self employed (Y/N).")
    str8 <- paste("Feature 7. ApplicantIncome: Applicant's income.")
    str9 <- paste("Feature 8. CoapplicantIncome: Coapplicant's income.")
    str10 <- paste("Feature 9. LoanAmount: Loan amount applied (in thousands).")
    str11 <- paste("Feature 10. Loan_Amount_Term: Term of loan (in months).")
    str12 <- paste("Feature 11. Credit_History: Credit history meets guidelines.")
    str13 <- paste("Feature 12. Property_Area: Location of Property (Urban/ Semi Urban/ Rural).")
    str14 <- paste("Feature 13. Loan_Status: Loan approved (Y/N).")
    HTML(paste(str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str12, str13, str14,sep = '<br/>'))
 
  })
  
  output$barplot1 <- renderPlot({
    df <- filedata()
    df<-df[complete.cases(df),]
    #Rename column names:
    ColName <- c("Gender","Marital Status", "No of Dependent","Education Level",
                 "Self Employed","Applicants Income","Coapplicants Income",
                 "Loan Amount","Term of Loan","Credit History","Property Area",
                 "Loan Status")
    colnames(df)[2:ncol(df)] <- ColName
    
    #Plot graphs:
    x <- input$attribute 
    y1 <- input$aincomerange[1]
    y2 <- input$aincomerange[2]
    w1<- input$cincomerange[1]
    w2<- input$cincomerange[2]
    z1 <- input$lincomerange[1]
    z2 <- input$lincomerange[2]
    df<-df%>%filter(df[7]>=y1 & df[7]<=y2 & df[8]>=w1 & df[8]<=w2 & df[9]>=z1 & df[9]<=z2)
    if (x == "Gender")
    {barplot(table(df[x]), col=rainbow(10), main = "Loan Data based on Gender")}
    else if(x == "Marital Status")
    {barplot(table(df[x]), col=rainbow(10), main = "Loan Data based on Marital Status")}
    else if(x == "No of Dependent")
    {barplot(table(df[x]), col=rainbow(10), main = "Loan Data based on Number of Dependents")}
    else if(x == "Education Level")
    {barplot(table(df[x]), col=rainbow(10), main = "Loan Data based on Education Level")}
    else if(x == "Self Employed")
    {barplot(table(df[x]), col=rainbow(10), main = "Loan Data based on Self-employment Status")}
    
    else if(x == "Applicants Income")
    {boxplot(df[x],
     names = c("Applicant Income"), 
     main = "Loan Data based on Applicant Income",col=rainbow(10))}
    else if(x == "Coapplicants Income")
    {boxplot(df[x],
             names = c("Coapplicants Income"), 
             main = "Loan Data based on Coapplicant Income",col=rainbow(10))}
    else if(x == "Loan Amount")
    {boxplot(df[x], main = "Loan Data based on Loan Amount",col=rainbow(10))}
    else if(x == "Term of Loan")
    {hist(as.numeric(unlist(df[x])), main = "Loan Data based on Terms of Loan",col='powderblue',
          xlab = "Term of Loan")}
    else if(x == "Credit History")
    {barplot(table(df[x]), col=rainbow(10), main = "Loan Data based on Credit History")}
    else if(x == "Property Area")
    {barplot(table(df[x]), col=rainbow(10), main = "Loan Data based on Location of Property")}
    else if(x == "Loan Status")
    {barplot(table(df[x]), col=rainbow(10), main = "Loan Approval")}
    else {return (NULL)}
  })
  
  output$barplot2 <- renderPlot({
    df <- filedata()
    #Rename column names
    ColName <- c("Gender","Marital Status", "No of Dependent","Education Level",
                 "Self Employed","Applicants Income","Coapplicants Income",
                 "Loan Amount","Term of Loan","Credit History","Property Area",
                 "Loan Status")
    colnames(df)[2:ncol(df)] <- ColName
    df$Loan_Status <- filedata()$Loan_Status
   
    #Plot graphs:
    x <- input$attribute 
    y1 <- input$aincomerange[1]
    y2 <- input$aincomerange[2]
    w1<- input$cincomerange[1]
    w2<- input$cincomerange[2]
    z1 <- input$lincomerange[1]
    z2 <- input$lincomerange[2]
    
    df<-df[complete.cases(df),]
    df<-df%>%filter(df[7]>=y1 & df[7]<=y2 & df[8]>=w1 & df[8]<=w2 & df[9]>=z1 & df[9]<=z2)
    if (x == "Gender")
    {ggplot(df, aes(x = df$Loan_Status,fill=df$Loan_Status) )+geom_bar() + 
        geom_text(stat='count',aes(label=..count..),vjust=1)+
        facet_grid(. ~ df[,2]) +
        labs(title = "Loan Status according to Applicant's Gender",
             x = "Loan Status", y = "Count") + 
        scale_fill_discrete(name = x)
    }
    else if(x == "Marital Status")
    {ggplot(df, aes(x = df$Loan_Status,fill=df$Loan_Status) ) +
        geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=1)+
        facet_grid(. ~ df[,3]) +
        labs(title = "Loan Status according to Applicant's Marital Status",
             x = "Loan Status", y = "Count") + 
        scale_fill_discrete(name = x)
    }
    else if(x == "No of Dependent")
    {ggplot(df, aes(x = df$Loan_Status,fill=df$Loan_Status)) + 
        geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=1)+
        facet_grid(. ~ df[,4]) +
        labs(title = "Loan Status according to Applicant's Number of Dependents",
             x = "Loan Status", y = "Count") + 
        scale_fill_discrete(name = x)
    }
    else if(x == "Education Level")
    {ggplot(df, aes(x = df$Loan_Status,fill=df$Loan_Status)) + 
        geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=1)+
        facet_grid(. ~ df[,5]) +
        labs(title = "Loan Status according to Applicant's Education Level",
             x = "Loan Status", y = "Count") + 
        scale_fill_discrete(name = x)
    }
    else if(x == "Self Employed")
    {ggplot(df, aes(x = df$Loan_Status,fill=df$Loan_Status)) + 
        geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=1)+
        facet_grid(. ~ df[,6]) + 
        labs(title = "Loan Status according to Applicant's Self-employment Status",
             x = "Loan Status", y = "Count") + 
        scale_fill_discrete(name = x)
    }
    
    else if(x == "Applicants Income")
    {ggplot(df, aes(x = df$Loan_Status, y = df[x],fill=df$Loan_Status)) + 
      geom_boxplot() + 
      labs(title = "Loan Status according to Applicant's Income",
           x = "Loan Status", y = "Applicant Income") + 
        scale_fill_discrete(name = x)
    }
    
    else if(x == "Coapplicants Income")
    {ggplot(df, aes(x = df$Loan_Status, y = df[x],fill=df$Loan_Status)) + 
        geom_boxplot() + 
        labs(title = "Loan Status according to Coapplicant Income",
             x = "Loan Status", y = "CoApplicant Income") + 
        scale_fill_discrete(name = x)
    }
      
    else if(x == "Loan Amount")
    {ggplot(df, aes(x = df$Loan_Status, y = df[x],fill=df$Loan_Status)) + 
        geom_boxplot() + 
        labs(title = "Loan Status according to Loan Amount",
             x = "Loan Status", y = "Loan Amount") + 
        scale_fill_discrete(name = x)
    }
    else if(x == "Term of Loan")
    {ggplot(df, aes(x = df$Loan_Status,fill=df$Loan_Status)) + 
        geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=1)+
        facet_grid(. ~ df[,10]) + 
        labs(title = "Loan Status according to Terms of Loan",
             x = "Loan Status", y = "Count") + 
        scale_fill_discrete(name = x)
    }
    else if(x == "Credit History")
    {ggplot(df, aes(x = df$Loan_Status,fill=df$Loan_Status)) + 
        geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=1)+
        facet_grid(. ~ df[,11]) + 
        labs(title = "Loan Status according to Credit History",
             x = "Loan Status", y = "Count") + 
        scale_fill_discrete(name = x)
    }
    else if(x == "Property Area")
    {ggplot(df, aes(x = df$Loan_Status,fill=df$Loan_Status)) + 
        geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=1)+
        facet_grid(. ~ df[,12]) + 
        labs(title = "Loan Status according to Location of Property",
             x = "Loan Status", y = "Count") + 
        scale_fill_discrete(name = x)
    }
    else if(x == "Loan Status")
    {ggplot(df, aes(x=df$Loan_Status)) + geom_bar(fill=c("#FF6666","#00CCFF")) + 
        geom_text(stat='count',aes(label=..count..),vjust=1) +
        labs(title = "Loan Status", x = "Loan Status", y = "Count") +
        scale_fill_discrete(name = x)
    }
    else {return (NULL)}
  })
  
  output$rate <- renderText({
    df <- filedata()
    x <- input$attribute 
    
    y1 <- input$aincomerange[1]
    y2 <- input$aincomerange[2]
    w1<- input$cincomerange[1]
    w2<- input$cincomerange[2]
    z1 <- input$lincomerange[1]
    z2 <- input$lincomerange[2]
    df<-df[complete.cases(df),]
    df<-df%>%filter(df[7]>=y1 & df[7]<=y2 & df[8]>=w1 & df[8]<=w2 & df[9]>=z1 & df[9]<=z2)
    
    if(x == "Loan Status")
    {rate<-prop.table(table(df$Loan_Status))
     rate<-round(rate*100,2)
     sprintf("The probability of loan approval is %s percent.",rate[2])}
  })
})
