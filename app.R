#' ============================================================================
#' Author: CWM
#' Purpose: Loan payoff optimization
#' Date: 11.18.2021
#' Notes:
#'  -- loan payoff function
#'  -- optimizaion using nloptr
#'  -- ui
#'  -- server
#'  -- runApp
#'
#'  To get this to run, click Run App in the window above or run the last
#'  line "shinyApp(ui = ui, server = server)"
#'  ===========================================================================

library(shiny)
library(shinybusy)
library(shinythemes)
library(tidyverse)
library(nloptr)
library("scales")
library(ggpmisc)

# =============================================================================
# The loan payoff function
#
# The first argument, x, is what is optimized later:
#   x[1] = monthly revenue % going to loans directly
#   x[2] = investment skim threshold. if investment amt above x[2], then skim off
#   x[3] = % of asset in investment to payout if investments > x[2]

my_loan_payoff <- function(x,
                           annual_interest_rate,
                           annual_growth_rate,
                           total_monthly_revenue,
                           asset_in_investments,
                           loan_to_payoff,
                           capital_gains_tax,
                           max_months,
                           pslf,
                           skim_check,
                           make_plot) {

  # annual interest rate
  # https://studentaid.gov/understand-aid/types/loans/interest-rates
  daily_interest_rate <- annual_interest_rate / 365
  monthly_interest_rate <- daily_interest_rate * 30

  # annual_growth_rate
  # updated to include correct ROI formula for ytd growth
  # thanks to Tiff B
  # https://www.calculator.net/investment-calculator.html
  monthly_growth_rate <- (1 + annual_growth_rate) ^ (1/12) - 1

  # calculate additional revenue payouts
  monthly_revenue_to_loan <- x[1] * total_monthly_revenue
  monthly_revenue_to_investment <- total_monthly_revenue - monthly_revenue_to_loan

  # investment skim,
  # basically saying, if investment amt is > threshold, then skim off some
  # this is subject to capital gains tax, assessed later
  investment_skim_threshold <- x[2]
  investment_skim_p <- x[3]

  # output datasets
  if (make_plot) {
    MAX_MONTH_NUM <- max_months
    loan_df <- vector("list", MAX_MONTH_NUM)
    investments_df <- vector("list", MAX_MONTH_NUM)
    amt_paid_df <- vector("list", MAX_MONTH_NUM)
  }

  # reset for each simulation
  month_i <- 1
  initial_loan_amt <- loan_to_payoff
  initial_investment_amt <- asset_in_investments
  month_paid_off <- NA

  # while (loan_to_payoff > 0 & month_i < max_months) {
  while (month_i <= max_months) {

    # -- STEP 1 --
    # calculate interest and growth
    loan_to_payoff <- loan_to_payoff * (1 + monthly_interest_rate)
    asset_in_investments <- asset_in_investments * (1 + monthly_growth_rate)
    investments_surplus <- 0
    if (make_plot) amt_paid_df[[month_i]] <- 0

    # -- STEP 2 --
    # now make some decisions
    # calculate how much you are paying out to the loan DIRECTLY
    if (loan_to_payoff > monthly_revenue_to_loan) {
      amt_paying_to_loan <- monthly_revenue_to_loan
      loan_to_payoff <- loan_to_payoff - amt_paying_to_loan

      if (make_plot) {
        amt_paid_df[[month_i]] <- amt_paying_to_loan
      }
    } else {

      # add surplus to investments
      investments_surplus <- monthly_revenue_to_loan - loan_to_payoff

      if (make_plot) {
        amt_paid_df[[month_i]] <-
          monthly_revenue_to_loan - investments_surplus
      }

      loan_to_payoff <- 0
    }

    # -- STEP 3 --
    # if there are loans remaining?
    # check the skim check
    if(skim_check) {
      if (loan_to_payoff > 0) {
  
        # is the investment amt above the threshold?
        if (asset_in_investments > investment_skim_threshold) {
  
          # what is the max that you could do from skim
          amount_from_skim <- investment_skim_p *
            asset_in_investments * (1 - capital_gains_tax)
  
          # if the max is less than remaining loan balance
          if (amount_from_skim < loan_to_payoff) {
  
            # adjust the amt you paid
            if (make_plot) {
              amt_paid_df[[month_i]] <-
                amt_paid_df[[month_i]] + asset_in_investments * investment_skim_p
            }
  
            # reduce loan by that much
            loan_to_payoff <- loan_to_payoff - amount_from_skim
  
            # reduce investments by the full amount
            asset_in_investments <- asset_in_investments * (1 - investment_skim_p)
          } else {
  
            # this means that you just have to take less
            lower_skim_p <- loan_to_payoff / (
              asset_in_investments * (1 - capital_gains_tax))
  
            stopifnot(lower_skim_p < investment_skim_p)
  
            # adjust the amt you paid
            if (make_plot) {
              amt_paid_df[[month_i]] <-
                amt_paid_df[[month_i]] + asset_in_investments * lower_skim_p
            }
  
            # pay off loan in full
            loan_to_payoff <- 0
  
            # reduce asset in investment by lower skim amt
            asset_in_investments <- asset_in_investments * (1 - lower_skim_p)
          }
        }
      }
    }
    
    # -- STEP 5 --
    # add investment amt and surplus to investments
    asset_in_investments <- asset_in_investments +
      monthly_revenue_to_investment + investments_surplus

    # if month_i = max_months, payoff loan in full
    # or if PSFL, forgive the remainder
    if (pslf) {
      if (month_i == 120) {
        loan_to_payoff <- 0
      }
    }

    # if you are at max_months, pay it out from investments
    if (month_i == max_months & loan_to_payoff > 0) {
      asset_in_investments <- asset_in_investments - loan_to_payoff / (1 - capital_gains_tax)
      if (make_plot) {
        amt_paid_df[[month_i]] <-
          amt_paid_df[[month_i]] + loan_to_payoff / (1 - capital_gains_tax)
      }
      loan_to_payoff <- 0
    }

    # track the month when it was paid off
    if (loan_to_payoff == 0 & is.na(month_paid_off)) {
      month_paid_off <- month_i
    }

    # store current loan and investment amts
    if (make_plot) {
      loan_df[[month_i]] <- loan_to_payoff
      investments_df[[month_i]] <- asset_in_investments
    }

    # next month
    month_i <- month_i + 1
  }

  ## ============================================================
  # some penalty if you don't pay if off in a certain time
  # penalty <- 2 ^ (loan_to_payoff / (0.005 * initial_loan_amt))
  # penalty <- 2 ^ (month_i / 5)
  # penalty <- 2^(100 * (month_i/max_months))
  # penalty <- 2 ^ (100 * loan_to_payoff / initial_loan_amt)
  # penalty <- 0
  ## ============================================================

  # Make the plot
  if (make_plot) {
    plot_df <- data.frame(
      month = 0:(month_i - 1),
      loan_balance = c(initial_loan_amt, do.call(rbind, loan_df)),
      investment_amt = c(initial_investment_amt, do.call(rbind, investments_df)),
      amt_paid = cumsum(c(0, do.call(rbind, amt_paid_df)))
    )

    amt_paid_total <- max(plot_df$amt_paid)
    highest_investment_amt <- max(plot_df$investment_amt)

    table_labels <- c("Amt paid", "Loan remainder", "Loan paid in",
                      "Amt in investments")
    table_values <- c( dollar_format(accuracy = 1)(amt_paid_total), 
                       dollar_format(accuracy = 1)(loan_to_payoff),
                       sprintf("%.1fyrs", month_paid_off / 12),
                       dollar_format(accuracy = 1)(asset_in_investments))
    my_table <- data.frame(desc = table_labels, value = table_values)
                       
    plot_df <- plot_df %>%
      pivot_longer(cols = c(loan_balance, investment_amt, amt_paid))
    
    plot_df$name <- factor(
      plot_df$name,
      levels = c("amt_paid", "investment_amt", "loan_balance"),
      labels = c("Amt. paid", "Investment amt.", "Loan balance")
    )

    p <- ggplot(plot_df, aes(x = month, y = value, color = name)) +
      theme_grey() + 
      geom_point() +
      geom_line() +
      scale_color_discrete(name = NULL) +
      ylab(NULL) +
      xlab("Month") +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 15, face = "plain"),
        legend.position = "bottom",
        title = element_text(size = 15)
      ) + 
      annotate(geom = "table",
               x = 1,
               y = highest_investment_amt,
               label = list(my_table),
               size = 5,
               table.colnames = F,
               table.hjust = 1)

    return(p)
  }

  # this is what you are minimizing
  # aka maximizing
  return(-(asset_in_investments))
}

# =============================================================================
# using NLOPTR to optimze for the % to put to loans directly or into investments
# for a later payout
# chose the simplest global optimizer that was fastest
# /////////////////////////////////////////////////////////////
# plug into nloptr, see what happens
# which algorithm to choose?
# https://nlopt.readthedocs.io/en/latest/NLopt_Algorithms/
#
# has to be global and non gradient, so any of the _GN_
# good previous success with IRES, so using that

payoff_optimizer <- function(input) {

  # min payment amount so you have to meet
  lb_x1 <- input$min_IDR / input$total_monthly_revenue

  res1 <- nloptr(
    x0 = as.double(c(
      max(input$p_revenue_direct, lb_x1),
      input$investment_skim_threshold,
      input$investment_skim_p
    )),
    eval_f = my_loan_payoff,
    lb = c(lb_x1, 0, 0),
    ub = c(1, input$skim_thresh_max, 1),
    opts = list(
      "algorithm" = "NLOPT_GN_ISRES",
      "maxeval" = input$max_eval,
      "ftol_abs" = input$ftol_abs,
      "print_level" = 3
    ),
    annual_interest_rate = input$annual_interest_rate,
    annual_growth_rate = input$annual_growth_rate,
    total_monthly_revenue = input$total_monthly_revenue,
    asset_in_investments = input$asset_in_investments,
    loan_to_payoff = input$loan_to_payoff,
    capital_gains_tax = input$capital_gains_tax,
    max_months = input$max_months,
    pslf = input$pslf,
    skim_check = input$skim_check,
    make_plot = F
  )

  print(res1)

  # make sure these are numeric, not integers
  output <- res1$solution * 1.00

  # if output[3] = 0, then set output[2] = 0
  # if you never skim, it doesn't matter what the threshold is
  if (output[3] < 0.001) output[2] <- 0.

  return(output)
}

# =============================================================================
# Define UI for Shiny application
# https://shiny.rstudio.com/articles/tag-glossary.html
# https://shiny.rstudio.com/articles/dynamic-ui.html

ui <- fluidPage(
  
  # theme
  theme = shinytheme("flatly"),
  
  # max width
  style = "max-width: 760px;",

  # Application title
  h2("Loan payoff decision tool",align = 'center'),
  
  h4("Helping you make decisions about investing versus paying off loans.", 
     a("Github", href="https://github.com/cmilando/loan-payoff"),
     align = 'center'),
  
  # plot
  plotOutput("distPlot"),
  
  hr(),

  fluidRow(
    # Non optimized inputs
    column(
      width = 6,
      tags$label("Non-optimized inputs", 
                 style = "color:blue; vertical-align: top"),
      numericInput("annual_interest_rate",
        "Annual Loan Interest Rate (0.0675 = 6.75%)",
        value = 0.0675, min = 0
      ),
      numericInput("min_IDR",
        "Minimum monthly loan payment ($)",
        value = 300, min = 0
      ),
      numericInput("annual_growth_rate",
        "Annual Investment Growth Rate (0.15 = 15%)",
        value = 0.10, min = 0, step = 0.01
      ),
      helpText("Converted to a monthly rate by (1 + rate)^(1/12) - 1",parse = T),
      numericInput("capital_gains_tax",
        "Capital gains tax (0.20 = 20%)",
        value = 0.20, min = 0, step = 0.01
      ),
      helpText("Incurred when investments are used to pay the loan"),
      numericInput("total_monthly_revenue",
        "Total monthly amount to be spent on investments or loan payoff ($)",
        value = 1200, min = 0
      ),
      numericInput("asset_in_investments",
        "Total initial investment ($)",
        value = 2600, min = 0
      ),
      numericInput("loan_to_payoff",
        "Loan payoff amount ($)",
        value = 50200, min = 0,
      ),
      checkboxInput("pslf", "Apply PSLF?", FALSE),
      helpText("Assumes that loans are forgiven after 10yrs"),
      numericInput("max_months",
        "Months to simulate", 120,
        min = 0, step = 1
      ),
      helpText("What is the max months before mandatory payoff")
    ),

    # Optimized inputs
    column(
      width = 6,
      tags$label("Optimized inputs", 
                 style = "color:blue; vertical-align: top"),br(),
      actionButton("optimize", "Maximize net worth!",
                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      actionButton("reset", "Reset values"),
      hr(),
      sliderInput("p_revenue_direct",
                  "% of monthly payoff budget going directly to loans:",
                  min = 0.0, max = 1., value = 0.5
      ),
      helpText(paste(
        "0% means its going all towards investments,",
        "for a later payout. 100% means its",
        "going all towards loans directly"
      )),
      hr(),
      checkboxInput("skim_check", "Check for skim threshold?", FALSE),
      helpText(paste(
        "If my investment amt is greater than this,",
        "then skim some off and use it to pay loans"
      )),
      conditionalPanel(
        condition = "input.skim_check == true",
        numericInput("investment_skim_threshold",
                     "investment skim threshold ($)", 3000,
                     min = 0
        ),
        helpText(paste(
          "If my investment amt is greater than this..."
        )),
        numericInput("skim_thresh_max",
                     "Max skim threshold to check? ($)", 5e4,
                     min = 0
        ),
        sliderInput("investment_skim_p",
                    "investment skim percentage", 0.5,
                    min = 0.0,
                    max = 1.
        ),
        helpText("what % of my total investment amt should I skim off?"),
      ),
      hr(),
      h5("Optimization parameters"),
      numericInput("max_eval", "Max. iterations", 2e5, min = 1),
      helpText("should be at least 2e5"),
      numericInput("ftol_abs", "ftol_abs", 1e-4, min = 1e-12),
      helpText("the cutoff for how much net worth changes before stopping"),
      hr(),
      helpText("penalty placeholder"),
      helpText(paste("an additional function that penalizes not paying off",
                     "loan quickly, call it quantized anxiety")),
    )
  )
)

# =============================================================================
# Define reactive server logic
server <- function(input, output, session) {
  
  # what happens when the optimize button is clicked
  observeEvent(input$optimize, {
    show_modal_spinner(
      text = "Running optimizer - see console",
      spin = "fading-circle"
    )
    
    output <- payoff_optimizer(input)
    
    remove_modal_spinner()
    
    updateNumericInput(session, "p_revenue_direct", value = output[1])
    updateNumericInput(session, "investment_skim_threshold", value = output[2])
    updateNumericInput(session, "investment_skim_p", value = output[3])
  })
  
  # what happens when the reset button is clicked
  observeEvent(input$reset, {
    updateSliderInput(session, "p_revenue_direct", value = 0.5)
    updateNumericInput(session, "investment_skim_threshold", value = 3000)
    updateSliderInput(session, "investment_skim_p", value = 0.5)
  })
  
  # draws the plot
  # because reactive, it updates each time
  output$distPlot <- renderPlot({
    my_loan_payoff(
      x = c(
        input$p_revenue_direct,
        input$investment_skim_threshold,
        input$investment_skim_p
      ),
      annual_interest_rate = input$annual_interest_rate,
      annual_growth_rate = input$annual_growth_rate,
      total_monthly_revenue = input$total_monthly_revenue,
      asset_in_investments = input$asset_in_investments,
      loan_to_payoff = input$loan_to_payoff,
      capital_gains_tax = input$capital_gains_tax,
      max_months = input$max_months,
      pslf = input$pslf,
      skim_check = input$skim_check,
      make_plot = T
    )
  })
}

# =============================================================================
shinyApp(ui = ui, server = server)
