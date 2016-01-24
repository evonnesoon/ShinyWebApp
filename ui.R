library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Malaysia Home Loan Calculator"),
  sidebarPanel(
    numericInput(inputId="propertyPrice", label="Property Price (RM):", value= 500000,min=0),
    numericInput(inputId="downPayment", label="Down Payment (RM):", value= 50000,min=0),
    numericInput(inputId="loanPeriod", label="Loan Period (Years):", value= 35,min=0),
    numericInput(inputId="interestRate", label="Interest Rate (%):", value= 4.25,min=0),
    actionButton("goButton", "Calculate"),
    br(),    
    p(strong(em("Github repository:",a("Developing Data Products - Peer Assessment Project; Shiny App",href="https://github.com/CrazyFarang/DevelopingDataProducts"))))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel('Your Monthly Repayment',
               h4('Monthly Repayment'),
               verbatimTextOutput("oiCalculateTheMortgage"),
               ##verbatimTextOutput("oiCalculateAmortization"),             
               plotOutput("newPie"),
               plotOutput("newPaymentSchedule"),
               h4('Payment Schedule'),
               dataTableOutput("oiCalculateAmortization2")
               ##textOutput("oiCalculateAmortization2")
               ##img(src="WHOBMI.png", height = 600, width =600),
               ##p("Source: ", a("WHO BMI classification", href = "http://apps.who.int/bmi/index.jsp?introPage=intro_3.html"))
               ),
    tabPanel('Data Summary from Banking System',
             p("Source:", a("Malaysia, Bank Negara - Banking System: Loans Applied by Purpose(2006-2013)", href = "http://www.bnm.gov.my/files/publication/msb/2011/8/xls/1.10.xls")),
             p("Source:", a("Malaysia, Bank Negara - Banking System: Loans Approved by Purpose(2006-2011)", href = "http://www.bnm.gov.my/files/publication/msb/2011/8/xls/1.12.xls")),
             br(),
             br(),
             plotOutput("newPlot1")
             ),
    tabPanel('Volume of Property Transaction and Annual Changes 1990 - 2014',
            p("Source:", a("Malaysia, National Property Information Centre - Volume of Property Transaction and Annual Changes from 1990 to 2014", href = "http://napic.jpph.gov.my/portal/web/guest/main-page?p_p_id=ViewStatistics_WAR_ViewStatisticsportlet&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_resource_id=fileDownload&p_p_cacheability=cacheLevelPage&p_p_col_id=column-2&p_p_col_count=2&fileURI=3264")),
            br(),
            plotOutput("newPlot2")
            ),
    tabPanel('Supply and Demand Properties Trend in year 2015 Quarter 2',
             h5('Existing Stock, Incoming Supply and Planned Supply of Residential, Shop and Industrial Propeties as at Q2 2015'),             
             img(src="images/Capture.JPG", height = 600, width =600),
             p("Source: ", a("National Property Information Centre Research", 
              href = "http://napic.jpph.gov.my/portal/web/guest/main-page?p_p_id=ViewStatistics_WAR_ViewStatisticsportlet&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_resource_id=fileDownload&p_p_cacheability=cacheLevelPage&p_p_col_id=column-2&p_p_col_count=2&fileURI=3676"))
            ),
    tabPanel('Payment Schedule Simulation',
             sidebarPanel(
               sliderInput('mu', 'Current Interest Rate(%)',value =4 , min = 1, max = 15, step = 0.25),width = 800 
             ),           
             br(),
             plotOutput("newPaymentSchedule2"),
             br(),                   
             plotOutput("newPie2")
          )
      )
    )
  )
)

