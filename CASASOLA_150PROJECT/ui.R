#ui code

library(shiny)
library(shinyjs)

# Define the UI
ui <- tagList(
  useShinyjs(), # Enable shinyjs 
  tags$head(
    # CSS styling
    tags$style(HTML("
      /* Global Styling */
      body {
        font-family: 'Roboto', sans-serif;
        background-color: #f9f9f9;
        margin: 0;
        padding: 0;
      }

      /* Styling for Welcome Screen */
      #welcome_screen {
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        height: 100vh;
        background-color: #f9f9f9;
      }
      #welcome_box {
        padding: 40px;
        border: 3px solid #0D3311;  
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        text-align: center;
        background-color: transparent;
      }
      #welcome_box h1 {
        font-weight: bold;
        font-size: 48px;
        color: #0D3311;
        margin-bottom: 20px;
      }
      #welcome_box .btn {
        font-size: 20px;
        font-weight: bold;
        padding: 10px 20px;
        color: #0D3311;
        background-color: white;
        border: none;
        border-radius: 5px;
        transition: background-color 0.3s ease, color 0.3s ease;
      }
      #welcome_box .btn:hover {
        background-color: #064d1d;
        color: white;
      }
      #vegetable_icon {
        margin-top: 20px;
        width: 80px;
        height: 80px;
      }

      /* Styling for Main UI */
      #main_ui h2 {
        font-weight: bold;
        font-size: 32px;
        color: #0D3311;
        text-align: center;
        margin-top: 20px;
      }
      .checkbox-group {
        color: #0D3311;
        font-size: 16px;
      }
      #main_ui .btn {
        margin-top: 10px;
        font-size: 16px;
        font-weight: bold;
        color: white;
        background-color: #0D3311;
        border: none;
        border-radius: 5px;
        transition: background-color 0.3s ease;
      }
      #main_ui .btn:hover {
        background-color: #064d1d;
        color: white;
      }
    "))
  ),
  # Welcome Screen
  div(
    id = "welcome_screen",
    div(
      id = "welcome_box",
      h1("WELCOME TO YOUR DIET PLANNER"),
      h2("We help choose the best diet meals for you."),
      actionButton("start_button", "LET'S GET STARTED", class = "btn-primary btn-lg")
    ),
    img(
      id = "vegetable_icon",
      src = "vegetable_icon.png", 
      alt = "Vegetable Icon"
    )
  ),
  # select foods ui
  hidden(
    div(
      id = "main_ui",
      fluidPage(
        h2("Choose meals that you want for the day"),
        br(),
        fluidRow(
          column(
            width = 12,
            div(
              class = "checkbox-group",
              checkboxGroupInput(
                "selected_meals",
                label = "Pick Meals",
                choices = c(
                  "Frozen Broccoli", "Carrots, Raw", "Celery, Raw", "Frozen Corn",
                  "Lettuce, Iceberg, Raw", "Peppers, Sweet, Raw", "Potatoes, Baked",
                  "Tofu", "Roasted Chicken", "Spaghetti with Sauce",
                  "Tomato, Red, Ripe, Raw", "Apple, Raw, with Skin", "Banana",
                  "Grapes", "Kiwifruit, Raw, Fresh", "Oranges", "Bagels",
                  "Wheat Bread", "White Bread", "Oatmeal Cookies", "Apple Pie",
                  "Chocolate Chip Cookies", "Butter,Regular", "Cheddar Cheese",
                  "3.3% Fat, Whole Milk", "2% Lowfat Milk", "Skim Milk",
                  "Poached Eggs", "Scrambled Eggs", "Bologna, Turkey",
                  "Frankfurter, Beef", "Ham, Sliced, Extralean", "Kielbasa, Prk",
                  "Cap'N Crunch", "Cheerios", "Corn Flks,Kellog's", "Raisin Brn,Kellog's",
                  "Rice Krispies", "Special K", "Oatmeal", "Malt O' Meal, Choc",
                  "Pizza W/ Peperoni", "Taco", "Hamburger W/ Toppings", "Hotdog, Plain",
                  "Couscous", "White Rice", "Macaroni,Ckd",
                  "Peanut Butter", "Pork", "Sardines in Oil", "White Tuna in Water",
                  "Popcorn,Air- Popped", "Potato Chips,Bbqflvr", "Pretzels",
                  "Tortilla Chip", "Chicknoodl Soup", "Splt Pea&Hamsoup",
                  "Vegetbeef Soup", "Neweng Clamchwd", "Tomato Soup",
                  "New E Clamchwd,W/ Mlk", "Crm Mshrm Soup,W/Mlk", "Beanbacn Soup,W/Watr"
                ),
                selected = NULL
              )
            ),
            br(),
            actionButton("submit", "Submit", class = "btn-primary"),
            actionButton("reset", "Reset", class = "btn-primary"),
            actionButton("all", "Include All", class = "btn-primary"),
            br(),
            verbatimTextOutput("output"),
            verbatimTextOutput("resultString"),
            dataTableOutput("resultingTable"),
            dataTableOutput("initialTableau"),
            verbatimTextOutput("iterationTableauSol")
          )
        )
      )
    )
  )
)
