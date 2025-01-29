#server

server <- function(input, output, session) {
  observeEvent(input$start_button, {
    hide("welcome_screen") # Hide the welcome screen
    show("main_ui")     
  })
  values = reactiveValues(foodlist = c())
  
  observeEvent(input$submit, {
    selectedmeals = input$selected_meals
    if (is.null(selectedmeals) || length(selectedmeals) == 0) {
      showNotification("Please select at least one meal before submitting.", type = "error")
      return()
    }
    
    mealList = as.list(input$selected_meals)
    if (length(mealList) == 0) {
      showNotification("INFEASIBLE. Please try again.", type = "error")
      return()
    }
    
    minimization = tryCatch({
      Minimization(mealList)
    }, error = function(e) {
      showNotification("INFEASIBLE. Try again.", type = "error")
      NULL
    })
    
    if (is.null(minimization) || is.null(minimization$optimalCost)) {
      showNotification("INFEASIBLE. Please try again.", type = "error")
      return()
    }
    
    # Proceed with rendering outputs
    output$output = renderPrint({
      p("You selected: ")
      list(fooddata = selectedmeals)
    })
    
    output$resultString = renderPrint({
      minimization$optimalCost
    })
    
    output$resultingTable = renderDataTable({
      minimization$resultingTable
    })
    
    output$initialTableau = renderDataTable({
      minimization$it
    }, options = list(scrollX = TRUE, pageLength = 10))
    
    output$iterationTableauSol = renderPrint({
      iterationOutput = capture.output({
        Minimization(mealList)
      })
      return(HTML(paste(iterationOutput, collapse = "\n")))
    })
  })
  
  
  
  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "selected_meals", choices = c(
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
      "Couscous", "White Rice","Macaroni,Ckd",
      "Peanut Butter",
      "Pork",
      "Sardines in Oil",
      "White Tuna in Water",
      "Popcorn,Air- Popped",
      "Potato Chips,Bbqflvr",
      "Pretzels",
      "Tortilla Chip",
      "Chicknoodl Soup",
      "Splt Pea&Hamsoup",
      "Vegetbeef Soup",
      "Neweng Clamchwd",
      "Tomato Soup",
      "New E Clamchwd,W/ Mlk",
      "Crm Mshrm Soup,W/Mlk",
      "Beanbacn Soup,W/Watr"), selected = NULL)
    values$foodlist = NULL
    output$output = NULL
  })
  
  observeEvent(input$all, {
    updateCheckboxGroupInput(session, "selected_meals", selected = c(
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
      "Couscous", "White Rice","Macaroni,Ckd",
      "Peanut Butter",
      "Pork",
      "Sardines in Oil",
      "White Tuna in Water",
      "Popcorn,Air- Popped",
      "Potato Chips,Bbqflvr",
      "Pretzels",
      "Tortilla Chip",
      "Chicknoodl Soup",
      "Splt Pea&Hamsoup",
      "Vegetbeef Soup",
      "Neweng Clamchwd",
      "Tomato Soup",
      "New E Clamchwd,W/ Mlk",
      "Crm Mshrm Soup,W/Mlk",
      "Beanbacn Soup,W/Watr"))
  })
}