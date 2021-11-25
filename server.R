library(shinybusy)
library(shiny)

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
