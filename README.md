# Pricing-Cars

## Problem Statement
Build a predictive model on R to forecast price of cars based on certain attributes

## Attributes
- trim
- subTrim
- condition
- isOneOwner
- mileage
- year
- color
- displacement
- fuel
- state
- region
- soundSystem
- wheelType
- wheelSize
- featureCount

Target variable - price

## Comparison of results obtained by different models
- MLR Stepwise : 8641
- MLR on Lasso variables : 8893
- Random Forest and Bagging : 7065
- Random forest with (mtry = 5) : 6269
- Random Forest on Lasso variables : 6390

Final model chosen: Random forest with 300 trees, node_size=20, and mtry = 5