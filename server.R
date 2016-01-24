library(shiny)
library(plyr)
library(ggplot2)
library(reshape)

source("general.R")

#Calculate The Mortgage Formula
CalculateTheMortgageFormula <-function(propertyPrice,downPayment,loanPeriod,interestRate) {
  #Formula obtain from: EMI = [(p*r/12) (1+r/12)^n]/[(1+r/12)^n – 1 ]  'http://www.authorcode.com/how-to-make-emi-calculator-using-c/
  loanAmount <- propertyPrice - downPayment
  interestRate <-interestRate/100
  loanPeriod <- loanPeriod * 12 
  payment <-(loanAmount) * (((1 + interestRate/12)^ loanPeriod) * interestRate)/(12 * (((1+interestRate/12)^ loanPeriod) - 1))
  return(payment)
}

CalculateTheMortgage<-function(propertyPrice,downPayment,loanPeriod,interestRate) {
  if (propertyPrice==0 || is.na(propertyPrice)) 
    return("Please enter property price!") 
  else if (downPayment==0 || is.na(downPayment)) 
    return("Please enter down payment!") 
  else if (loanPeriod ==0 || is.na(loanPeriod)) 
    return("Please enter loan period!") 
  else if (interestRate ==0|| is.na(interestRate)) 
    return("Please enter interest rate!") 
  else  
  {
    payment <- CalculateTheMortgageFormula(propertyPrice,downPayment,loanPeriod,interestRate)
  }
  return(round(payment,2))
}

CalculateAmortization<-function(propertyPrice,downPayment,loanPeriod,interestRate) {
  monthlyPayment <- CalculateTheMortgageFormula(propertyPrice,downPayment,loanPeriod,interestRate)
  endingBalance<- propertyPrice - downPayment
  accumulateInterest <- 0
  accumulatePrincipal <- 0
  loanPeriod <- loanPeriod*12
  objlist <- list()
  for(a in 1: loanPeriod)
  { 
    monthlyInterestPayment <- monthlyPayment - (endingBalance * interestRate/1200)
    
    if(a == loanPeriod &&  (endingBalance - monthlyInterestPayment) <0)
    {
      monthlyInterestPayment <- monthlyInterestPayment + (endingBalance - monthlyInterestPayment)
    }
    
    endingBalance <- endingBalance - monthlyInterestPayment
    accumulatePrincipal <- accumulatePrincipal + monthlyInterestPayment
    accumulateInterest <- accumulateInterest + (monthlyPayment - monthlyInterestPayment)
    
    if(a%%12 == 0)
    {
      data <- (c(a/12, round(accumulatePrincipal,2),round(accumulateInterest,2),round(endingBalance,2)))   
      objlist[[a/12]] <- data 
    }
  }
  dt<-do.call(rbind.data.frame, objlist)
  names(dt)=c("YEARS", "PRINCIPAL","INTEREST","BALANCE")
  return(dt)
}

shinyServer(function(input, output) {
  output$oiCalculateTheMortgage <- renderPrint({
    input$goButton
    isolate(paste("RM",as.character(CalculateTheMortgage(input$propertyPrice,input$downPayment,input$loanPeriod,input$interestRate))))
  })
  ##output$oiCalculateAmortization <- renderPrint({
  ##  input$goButton
  ##  isolate(CalculateAmortization(input$propertyPrice,input$downPayment,input$loanPeriod,input$interestRate))
  ##})
  output$newPie <- renderPlot({
    #http://www.statmethods.net/graphs/pie.html
    input$goButton    
    if (input$propertyPrice!=0 && !is.na(input$propertyPrice) &&
          input$downPayment!=0 && !is.na(input$downPayment) &&
          input$loanPeriod !=0 && !is.na(input$loanPeriod) &&
          input$interestRate !=0 && !is.na(input$interestRate)) 
    {
      getTotalLoanCost <- CalculateTheMortgageFormula(input$propertyPrice,input$downPayment,input$loanPeriod,input$interestRate) * input$loanPeriod *12     
      getTotalInterest <- getTotalLoanCost-(input$propertyPrice-input$downPayment)
      getTotalPrincipal <- getTotalLoanCost-getTotalInterest
      slices <- c(getTotalInterest, getTotalPrincipal)
      lbls <- c("Interest", "Principal")
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels 
      lbls <- paste(lbls,"%",sep="") # ad % to labels 
      pie(slices, labels = lbls,  col=rainbow(length(lbls)),main="Payment Breakdown")
    }
  })
  output$newPaymentSchedule <- renderPlot({
    input$goButton  
    if (input$propertyPrice!=0 && !is.na(input$propertyPrice) &&
          input$downPayment!=0 && !is.na(input$downPayment) &&
          input$loanPeriod !=0 && !is.na(input$loanPeriod) &&
          input$interestRate !=0 && !is.na(input$interestRate)) 
    {
      df<-(CalculateAmortization(input$propertyPrice,input$downPayment,input$loanPeriod,input$interestRate))
      ggplot(data =df, aes(x = df$YEARS, )) + 
        #geom_line(aes(y=df$PRINCIPAL),colour="red") +
        #geom_line(aes(y=df$INTEREST),colour="blue") +  
        #geom_line(aes(y=df$BALANCE),colour="yellow") +    
        geom_point(aes(y = df$INTEREST, col = "INTEREST")) + 
        geom_point(aes(y = df$PRINCIPAL, col = "PRINCIPAL")) + 
        geom_point(aes(y = df$BALANCE, col = "BALANCE")) + 
        xlab("Years") + 
        ylab("RM") +            
        labs(color="Legend")+
        scale_x_continuous(breaks=seq(1, input$loanPeriod, 3)) +
        scale_y_continuous(breaks=seq(0, 100000000, 50000))
    }
  })  
  output$oiCalculateAmortization2 <- renderDataTable({
    input$goButton
    if (input$propertyPrice!=0 && !is.na(input$propertyPrice) &&
          input$downPayment!=0 && !is.na(input$downPayment) &&
          input$loanPeriod !=0 && !is.na(input$loanPeriod) &&
          input$interestRate !=0 && !is.na(input$interestRate)) 
    {
      isolate(CalculateAmortization(input$propertyPrice,input$downPayment,input$loanPeriod,input$interestRate))
    }
  }, options = list(paging = FALSE,searching = FALSE))
  output$newPlot1 <- renderPlot({
    input$goButton
    df1<-GetLoansAppliedbyPurpose()
    #df1$Type <- "AppliedPropertyLoan"
    df2<-GetLoansApprovedbyPurpose()
    #df2$Type <- "ApprovedPropertyLoan"
    df <- merge(df1,df2,by="Years", all = TRUE)
    ##df[is.na(df)] <-0 
    names(df) <- c("Years","AppliedPropertyLoan","ApprovedPropertyLoan") 
    ggplot(data =df, aes(x = df$Years, )) + 
      geom_line(aes(y=df$AppliedPropertyLoan),colour="red") +
      geom_line(aes(y=df$ApprovedPropertyLoan),colour="blue") +     
      geom_point(aes(y = df$AppliedPropertyLoan, col = "Applied Property Loan")) + 
      geom_point(aes(y = df$ApprovedPropertyLoan, col = "Approved Property Loan")) + 
      geom_text(aes(y = df$AppliedPropertyLoan, label=df$AppliedPropertyLoan))+
      geom_text(aes(y = df$ApprovedPropertyLoan, label=df$ApprovedPropertyLoan))+
      xlab("Years (2006-2013)") + 
      ylab("Totals Application") +
      ggtitle("Banking System: Total Loans Applied VS Total Loan Approved")+        
      labs(color="Legend")+
      scale_x_continuous(breaks=seq(2006, 2015, 1))    
  })
  output$newPlot2 <- renderPlot({
    input$goButton
    df <- GetNPICData()
    names(df)
    ggplot(data =df, aes(x = df$Years)) +
      geom_bar(data=df, aes(y = df$VolumnOfTrans),stat = "identity", fill="lightblue", colour="darkgreen",position = "dodge") +
      ##geom_bar(data=df, aes(y= df$Value), stat = "identity", fill="yellow", colour="darkgreen",position = "dodge") +
      geom_line(data=df, aes(x=df$Years, y=df$VolumnOfTrans))+
      geom_point(aes(y = df$VolumnOfTrans, col = "Applied Property Loan")) + 
      geom_text(aes(y = df$VolumnOfTrans, label=df$ChangeInVol)) +
      xlab("Years (1990-2014)") +                  
      ylab("RM('000)")+
      guides(colour=FALSE)+
      geom_label(aes(y = df$VolumnOfTrans,label=df$ChangeInVol))+
      ggtitle("Volume of Property Transaction and Annual Changes from 1990 to 2014")+
      scale_x_continuous(breaks=seq(1990, 2014, 1))   
    ##stat_summary_bin(aes(y = df$VolumnOfTrans), fun.y = "mean", geom = "point")
  })
  output$newPaymentSchedule2 <- renderPlot({
    if (input$propertyPrice!=0 && !is.na(input$propertyPrice) &&
          input$downPayment!=0 && !is.na(input$downPayment) &&
          input$loanPeriod !=0 && !is.na(input$loanPeriod) &&
          input$interestRate !=0 && !is.na(input$interestRate)) 
    {
      interestRate <- input$mu
      df<-(CalculateAmortization(input$propertyPrice,input$downPayment,input$loanPeriod,interestRate))
      ggplot(data =df, aes(x = df$YEARS, )) + 
        #geom_line(aes(y=df$PRINCIPAL),colour="red") +
        #geom_line(aes(y=df$INTEREST),colour="blue") +  
        #geom_line(aes(y=df$BALANCE),colour="yellow") +    
        geom_point(aes(y = df$INTEREST, col = "INTEREST")) + 
        geom_point(aes(y = df$PRINCIPAL, col = "PRINCIPAL")) + 
        geom_point(aes(y = df$BALANCE, col = "BALANCE")) + 
        xlab("Years") + 
        ylab("RM") +            
        labs(color="Legend")+
        scale_x_continuous(breaks=seq(1, input$loanPeriod, 3)) +
        scale_y_continuous(breaks=seq(0, 100000000, 50000)) 
    }
  }) 
  output$newPie2 <- renderPlot({
    #http://www.statmethods.net/graphs/pie.html
    if (input$propertyPrice!=0 && !is.na(input$propertyPrice) &&
          input$downPayment!=0 && !is.na(input$downPayment) &&
          input$loanPeriod !=0 && !is.na(input$loanPeriod) &&
          input$interestRate !=0 && !is.na(input$interestRate)) 
    {
      interestRate <- input$mu
      getTotalLoanCost <- CalculateTheMortgageFormula(input$propertyPrice,input$downPayment,input$loanPeriod,interestRate) * input$loanPeriod *12     
      getTotalInterest <- getTotalLoanCost-(input$propertyPrice-input$downPayment)
      getTotalPrincipal <- getTotalLoanCost-getTotalInterest
      slices <- c(getTotalInterest, getTotalPrincipal)
      lbls <- c("Interest", "Principal")
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels 
      lbls <- paste(lbls,"%",sep="") # ad % to labels 
      pie(slices, labels = lbls,  col=rainbow(length(lbls)),main="Payment Breakdown")
    }    
  })
})
