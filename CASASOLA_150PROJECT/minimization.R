#minimization code

foods = read.csv("C:/Users/accas/OneDrive/Documents/CASASOLA_150PROJECT/nutrition.csv", header = FALSE, sep = ",")
names(foods) = c("Foods", "Price/Serving", "Serving Size", "Calories", "Cholesterol mg", "Total_Fat g", "Sodium mg", "Carbohydrates g", "Dietary_Fiber g", "Protein g", "Vit_A IU", "Vit_C IU", "Calcium g", "Iron mg")
options(max.print=1000000)
MinimumNegativeIndex = function(vector){
  copy = c(vector)
  negative = c()
  for(i in 1 :length(copy)){
    for(j in 1:length(copy)){
      if(copy[i] < copy[j]){
        temp = copy[i]
        copy[i]= copy[j]
        copy[j] = temp
      }
    }
  }
  
  for(k in 1:length(copy)){
    if(copy[k] < 0){
      negative =c(negative, copy[k])
    }
  }
  
  if(length(negative) > 0){
    for(l in 1:length(vector)){
      if(negative[1] == vector[l]){
        return(l)
      }
    }
    
  }
  return(NULL)
  
}

MinimumPositiveIndex = function(vector){
  copy = c(vector)
  positive = c()
  for(i in 1 :length(copy)){
    for(j in 1:length(copy)){
      if(copy[i] < copy[j]){
        temp = copy[i]
        copy[i]= copy[j]
        copy[j] = temp
      }
    }
  }
  
  for(k in 1:length(copy)){
    if(copy[k] > 0){
      positive =c(positive, copy[k])
    }
  }
  
  if(length(positive) > 0){
    for(l in 1:length(vector)){
      if(positive[1] == vector[l]){
        return(l)
      }
    }
  }
  return(NULL)
}


Minimization = function(foodlist){
  minCal = 1900
  maxCal = 2250
  minChol = 0
  maxChol = 300
  minFat = 0
  maxTotFat = 65
  minSodium = 0
  maxSodium = 2400
  minCarb = 0
  maxCarb = 300
  minFiber = 25
  maxFiber = 100
  minProtein = 50
  maxProtein = 100
  minVitA = 5000
  maxVitA = 50000
  minVitC = 50
  maxVitC = 20000
  minCalcium = 800
  maxCalcium = 1600
  minIron = 10
  maxIron = 30
  nutrientCount = length(foods[1,]) - 3
  foodCount = length(foodlist)
  
  
  foodMatrix = data.frame(matrix(nrow = length(foodlist), ncol = 14))
  names(foodMatrix) = c("Foods", "Price/Serving", "Serving Size", "Calories", "Cholesterol mg", "Total_Fat g", "Sodium mg", "Carbohydrates g", "Dietary_Fiber g", "Protein g", "Vit_A IU", "Vit_C IU", "Calcium g", "Iron mg")
  for(i in 1:length(foodlist)){ #creates a dataframe that consists only of the food that was chosen by the user
    for(j in 1:length(foods[,1])){
      if(foods[i, 1] == foodlist[j]){
        foodMatrix[i,] = foods[i,]
      }
    }
  }
  
  varList = c() #creates variable strings
  for(i in 1:length(foodlist)){
    var = paste("x",i, sep="")
    varList = c(varList, var)
  }
  
  
  slackVarList = c() #creates slack variable strings
  for(j in 1:((nutrientCount * 2)+foodCount)){
    slackVar = paste("S", j, sep="")
    slackVarList = c(slackVarList, slackVar)
  }
  dimNames = c(varList, slackVarList, "Z" ,"Solution")
  initialTableau = matrix(ncol = length(dimNames), nrow = ((nutrientCount*2) + foodCount + 1), dimnames = list(1:((nutrientCount*2)+foodCount + 1), dimNames))
  
  nutCount = 4
  for(i in seq(1, (nrow(initialTableau)-foodCount - 1), 2)){
    initialTableau[i, 1:foodCount] = foodMatrix[1:length(foodlist), nutCount] * -1
    initialTableau[i+1, 1:foodCount] = (foodMatrix[1:length(foodlist), nutCount])
    nutCount=nutCount +1
    #print(initialTableau)
  }
  
  
  initialTableau[1, ncol(initialTableau)] = (minCal) * -1 
  initialTableau[2, ncol(initialTableau)] = maxCal
  initialTableau[3, ncol(initialTableau)] = (minChol) * -1
  initialTableau[4, ncol(initialTableau)] = maxChol
  initialTableau[5, ncol(initialTableau)] = (minFat) * -1
  initialTableau[6, ncol(initialTableau)] = maxTotFat
  initialTableau[7, ncol(initialTableau)] = (minSodium) * -1
  initialTableau[8, ncol(initialTableau)] = maxSodium
  initialTableau[9, ncol(initialTableau)] = (minCarb) * -1
  initialTableau[10, ncol(initialTableau)] = maxCarb
  initialTableau[11, ncol(initialTableau)] = (minFiber) * -1
  initialTableau[12, ncol(initialTableau)] = maxFiber
  initialTableau[13, ncol(initialTableau)] = (minProtein) * -1
  initialTableau[14, ncol(initialTableau)] = maxProtein
  initialTableau[15, ncol(initialTableau)] = (minVitA) * -1
  initialTableau[16, ncol(initialTableau)] = maxVitA
  initialTableau[17, ncol(initialTableau)] = (minVitC) * -1
  initialTableau[18, ncol(initialTableau)] = maxVitC
  initialTableau[19, ncol(initialTableau)] = (minCalcium) * -1
  initialTableau[20, ncol(initialTableau)] = maxCalcium
  initialTableau[21, ncol(initialTableau)] = (minIron) * -1
  initialTableau[22, ncol(initialTableau)] = maxIron
  
  for(x in 23:nrow(initialTableau)){
    for(y in 1:foodCount){
      if(x == (y+(nutrientCount*2))){
        initialTableau[x, y] = 1
      }
      else{
        initialTableau[x, y] = 0
      }
    }
    initialTableau[x, ncol(initialTableau)] = 10
  }
  
  for(j in 1:nrow(initialTableau)){
    for(k in 1:nrow(initialTableau)){
      if(j == k && j!=nrow(initialTableau) && k != nrow(initialTableau)){
        if(k %% 2 == 0){
          initialTableau[j,k+foodCount] = 1
        }
        else{
          initialTableau[j,k+foodCount] = 1
        }
      }
      else if(j==nrow(initialTableau) && k == nrow(initialTableau)){
        initialTableau[j,k+foodCount] = 1
      }
      else{
        initialTableau[j,k+foodCount] = 0
      }
    }
  }
  
  
  for(l in 1:foodCount){
    initialTableau[nrow(initialTableau), l] = (foodMatrix[l,2])
  }
  initialTableau[nrow(initialTableau), ncol(initialTableau)] = 0
  
  
  # print(initialTableau[ 1:(nrow(initialTableau)-1),ncol(initialTableau) ])
  # print(min(initialTableau[,(nrow(initialTableau)-1)]))
  
  initialTab = as.data.frame(initialTableau)
  counter = 0
  #print(initialTableau)
  
  
  #ELIMINATE NEGATIVE AT RHS FIRST
  while(is.null(MinimumNegativeIndex(initialTableau[1:(nrow(initialTableau)-1), ncol(initialTableau)])) == FALSE){ #while there is a negative value in the RHS
    for(i in 1:(nrow(initialTableau)-1)){ #accesses every rows in the RHS
      if(initialTableau[i, ncol(initialTableau)] < 0){ #if current RHS element is negative
        pivotRowIndex = i #current RHS element is the pivotRow
        pivotColIndex = MinimumNegativeIndex(initialTableau[pivotRowIndex, 1:(ncol(initialTableau)-1)]) #pivot column is determined by getting the minimum in the pivot row
        pivotElement = initialTableau[pivotRowIndex, pivotColIndex] 
        normPivotRow = c((initialTableau[pivotRowIndex, ]) / pivotElement)
        initialTableau[pivotRowIndex, ] = normPivotRow
        for(j in 1:(nrow(initialTableau))){ #eliminate every row in pivotCol aside from value in pivotRow
          if(j != pivotRowIndex){
            elimElement = initialTableau[j, pivotColIndex]
            initialTableau[j, ] = initialTableau[j, ] - (normPivotRow * elimElement)
          }
        }
      }
    }
    basicSolution = matrix(0, ncol=ncol(initialTableau), nrow = 1, dimnames = list("Values", dimNames))
    for(i in 1:ncol(initialTableau)){ #store basic solution
      zeroCounter = 0
      oneCounter = 0
      oneIndex = 0
      for(j in 1:nrow(initialTableau)){
        if(initialTableau[j, i] == 0){
          zeroCounter = zeroCounter + 1
        }
        else if(initialTableau[j, i] == 1){
          oneCounter = oneCounter + 1
          oneIndex = j
        }
      }
      
      if(zeroCounter == (nrow(initialTableau) - 1) && oneCounter == 1){
        basicSolution[1, i] = initialTableau[oneIndex, ncol(initialTableau)]
      }
    }
    
    counter = counter+1
    print(paste("ITERATION ", counter, ":", sep=""))
    print(initialTableau)
    cat("\n")
    print("SOLUTION: ")
    print(basicSolution)
    cat("\n")
    cat("\n")
  }
  
  #THEN ELIMINATE ALL NEGATIVE IN BOTTOM ROW/BASIC SOLUTION ROW
  while(is.null(MinimumNegativeIndex(initialTableau[nrow(initialTableau), 1:(ncol(initialTableau)-1)])) == FALSE) {#while there is negative at the objective function row
    pivotColIndex = which.min(initialTableau[nrow(initialTableau), 1:(ncol(initialTableau)-1)])
    testRatio = (initialTableau[,ncol(initialTableau)]) / (initialTableau[, pivotColIndex])
    
    for(i in 1:length(testRatio)){
      if(is.infinite(testRatio[i]) || is.nan(testRatio[i])){
        testRatio[i] = 99999
      }
    }

    testRatioMin = MinimumPositiveIndex(testRatio)
    pivotRowIndex = testRatioMin
    pivotElement = initialTableau[pivotRowIndex, pivotColIndex]
    normalizedPivotRow = c((initialTableau[pivotRowIndex, ])/ pivotElement)
    initialTableau[pivotRowIndex, ] = normalizedPivotRow
    
    for(x in 1:nrow(initialTableau)){
      if(x != pivotRowIndex){
        elimElement = initialTableau[x, pivotColIndex]
        initialTableau[x, ] = initialTableau[x, ] - (normalizedPivotRow * elimElement)
      }
    }
    
    basicSolution = matrix(0, ncol=ncol(initialTableau), nrow = 1, dimnames = list("Values", dimNames))
    for(i in 1:ncol(initialTableau)){
      zeroCounter = 0
      oneCounter = 0
      oneIndex = 0
      for(j in 1:nrow(initialTableau)){
        if(initialTableau[j, i] == 0){
          zeroCounter = zeroCounter + 1
        }
        else if(initialTableau[j, i] == 1){
          oneCounter = oneCounter + 1
          oneIndex = j
        }
      }
      
      if(zeroCounter == (nrow(initialTableau) - 1) && oneCounter == 1){
        basicSolution[1, i] = initialTableau[oneIndex, ncol(initialTableau)]
      }
    }
    counter = counter + 1
    print(paste("ITERATION ", counter, ":", sep=""))
    print(initialTableau)
    cat("\n")
    print("SOLUTION: ")
    print(basicSolution)
    cat("\n")
    cat("\n")
  }
  #print(foodMatrix)
  optimalFoodCount = 0
  optimalFoodList = c()
  optimalServing = c()
  optimalCost = c()
  for(i in 1:length(foodlist)){
    if(basicSolution[1,i] != 0){
      optimalFoodCount = optimalFoodCount + 1
      optimalFoodList = c(optimalFoodList, foodMatrix[i, 1])
      optimalServing = c(optimalServing, basicSolution[1, i])
      cost = foodMatrix[i, 2]
      optimalCost = c(optimalCost, cost*optimalServing[optimalFoodCount])
    }
  }
  
  resultingTable = data.frame(matrix(nrow = length(optimalFoodList), ncol = 3, dimnames = list(c(1:length(optimalFoodList)), c("Food", "Servings", "Cost($)"))))
  resultingTable[,1] = optimalFoodList
  resultingTable[,2] = optimalServing
  resultingTable[,3] = optimalCost
  
  optimalCostString = paste("The cost of this diet is $", (initialTableau[nrow(initialTableau), ncol(initialTableau)]*-1), " per day.", sep="")
  
  return(list(it = initialTab, basicSolution = basicSolution, optimalCost = optimalCostString, resultingTable = resultingTable))
}
#food = list("Frozen Broccoli", "Carrots, Raw", "Celery, Raw", "Frozen Corn",
 #           "Lettuce, Iceberg, Raw", "Roasted Chicken", "Baked Potato",
#             "Tofu","Peppers, Sweet, Raw" ,"Spaghetti with Sauce", "Tomato, Red, Ripe, Raw", "Apple, Raw, with Skin", "Banana",
 #            "Grapes", "Kiwifruit, Raw, Fresh", "Oranges", "Bagels",
#             "Wheat Bread", "White Bread", "Oatmeal Cookies")
#pleasework_ayoko_na = Minimization(food)
