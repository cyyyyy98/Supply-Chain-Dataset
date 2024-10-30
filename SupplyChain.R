chain <- read.csv('supply_chain_data.csv')
sum(is.na(chain))
str(chain)

chain$Product.type <- as.factor(chain$Product.type)
SKU_unique <- length(chain$SKU) == length(unique(chain$SKU))
chain$Customer.demographics <- as.factor(chain$Customer.demographics)
chain$Shipping.carriers <- as.factor(chain$Shipping.carriers)
chain$Supplier.name <- as.factor(chain$Supplier.name)
chain$Location <- as.factor(chain$Location)
chain$Inspection.results <- as.factor(chain$Inspection.results)
chain$Transportation.modes <- as.factor(chain$Transportation.modes)
chain$Routes <- as.factor(chain$Routes)
str(chain)

levels(chain$Location)
install.packages(dplyr)
library(dplyr)

#####Ample Products by Locations
Mumbai_products <- chain %>%
  filter(Location == 'Mumbai') %>%
  select(Product.type, Customer.demographics, Price, Availability) %>%
  mutate(Sales = Price * Availability) %>%
  group_by(Product.type) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))
# Cosmetics are high available in Mumbai

Delhi_products <- chain %>%
  filter(Location == 'Delhi') %>%
  select(Product.type, Customer.demographics, Price, Availability) %>%
  mutate(Sales = Price * Availability) %>%
  group_by(Product.type) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))
# Cosmetics are high available in Delhi

Chennai_products <- chain %>%
  filter(Location == 'Chennai') %>%
  select(Product.type, Customer.demographics, Price, Availability) %>%
  mutate(Sales = Price * Availability) %>%
  group_by(Product.type) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))
# Skincare is high available in Chennai

Kolkata_products <- chain %>%
  filter(Location == 'Kolkata') %>%
  select(Product.type, Customer.demographics, Price, Availability) %>%
  mutate(Sales = Price * Availability) %>%
  group_by(Product.type) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))
# Skincare is high available in Kolkata

Bangalore_products <- chain %>%
  filter(Location == 'Bangalore') %>%
  select(Product.type, Customer.demographics, Price, Availability) %>%
  mutate(Sales = Price * Availability) %>%
  group_by(Product.type) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))
# Haircare is high available in Bangalore

#####Profitable Products by Locations
Mumbai_Revenue <- chain %>%
  filter(Location == 'Mumbai') %>%
  select(Product.type, Revenue.generated, Order.quantities) %>%
  mutate(Average_Revenue = Revenue.generated / Order.quantities) %>%
  group_by(Product.type) %>%
  summarise(Average_Revenue = mean(Average_Revenue))
# Haircare high profitable in Mumbai

Delhi_Revenue <- chain %>%
  filter(Location == 'Delhi') %>%
  select(Product.type, Revenue.generated, Order.quantities) %>%
  mutate(Average_Revenue = Revenue.generated / Order.quantities) %>%
  group_by(Product.type) %>%
  summarise(Average_Revenue = mean(Average_Revenue))
# Haircare high profitable in Delhi

Chennai_Revenue <- chain %>%
  filter(Location == 'Chennai') %>%
  select(Product.type, Revenue.generated, Order.quantities) %>%
  mutate(Average_Revenue = Revenue.generated / Order.quantities) %>%
  group_by(Product.type) %>%
  summarise(Average_Revenue = mean(Average_Revenue))
# Cosmetics high profitable in Chennai

Kolkata_Revenue <- chain %>%
  filter(Location == 'Kolkata') %>%
  select(Product.type, Revenue.generated, Order.quantities) %>%
  mutate(Average_Revenue = Revenue.generated / Order.quantities) %>%
  group_by(Product.type) %>%
  summarise(Average_Revenue = mean(Average_Revenue))
# Cosmetics high profitable in Kolkata

Bangalore_Revenue <- chain %>%
  filter(Location == 'Bangalore') %>%
  select(Product.type, Revenue.generated, Order.quantities) %>%
  mutate(Average_Revenue = Revenue.generated / Order.quantities) %>%
  group_by(Product.type) %>%
  summarise(Average_Revenue = mean(Average_Revenue))
# Haircare high profitable in Bangalore

supplier_cost <- chain %>%
  select(Supplier.name, Shipping.costs, Shipping.times, Transportation.modes) %>%
  mutate(Transportation_cost = Shipping.costs * Shipping.times) %>%
  group_by(Supplier.name) %>%
  summarise(Average_shipping = mean(Transportation_cost)) %>%
  arrange(desc(Average_shipping))
# Supplier 4 has the highest average shipping costs @35.53
# Supplier 3 has the lowest average shipping costs @23.88

transportation_cost_highest <- chain %>%
  select(Supplier.name, Shipping.costs, Shipping.times, Transportation.modes) %>%
  mutate(Transportation_cost = Shipping.costs * Shipping.times) %>%
  group_by(Transportation.modes, Supplier.name) %>%
  summarise(Average_shipping = mean(Transportation_cost)) %>%
  ungroup() %>%
  group_by(Transportation.modes) %>%
  slice_max(Average_shipping, n=1)
# Air: supplier 5 @49.87
# Rail: supplier 1 @57.09
# Road: supplier 1 @32.37
# Sea: supplier 5 @48.44

transportation_cost_lowest <- chain %>%
  select(Supplier.name, Shipping.costs, Shipping.times, Transportation.modes) %>%
  mutate(Transportation_cost = Shipping.costs * Shipping.times) %>%
  group_by(Transportation.modes, Supplier.name) %>%
  summarise(Average_shipping = mean(Transportation_cost)) %>%
  ungroup() %>%
  group_by(Transportation.modes) %>%
  slice_min(Average_shipping, n=1)
# Air: supplier 3 @6.5
# Rail: supplier 5 @21.68
# Road: supplier 4 @22.91
# Sea: supplier 3 @25.22

levels(chain$Product.type)
inspection_cosmetics <- chain %>%
  filter(Product.type == 'cosmetics') %>%
  count(Inspection.results) %>%
  mutate(Percentage = (n/sum(n)) *100) %>%
  arrange(desc(Percentage))
# Fail: 38.46%; Pending: 38.46%; Pass: 23.08%
inspection_haircare <- chain %>%
  filter(Product.type == 'haircare') %>%
  count(Inspection.results) %>%
  mutate(Percentage = (n/sum(n)) *100) %>%
  arrange(desc(Percentage))
# Pending: 44.12%; Fail: 38.24%; Pass: 17.65%

inspection_skincare <- chain %>%
  filter(Product.type == 'skincare') %>%
  count(Inspection.results) %>%
  mutate(Percentage = (n/sum(n)) *100) %>%
  arrange(desc(Percentage))
# Pending: 40%; Fail: 32.5%; Pass: 27.5%

estimated_defect <- chain %>%
  select(Defect.rates, Order.quantities, Product.type, Production.volumes) %>%
  mutate(Not_Defected_est = Production.volumes - (Defect.rates * Production.volumes)) %>%
  group_by(Product.type, Order.quantities) %>%
  reframe(Not_Defected_est)

adjusted_chain <- chain %>%
  mutate(Not_Defected_est = Production.volumes - (Defect.rates * Production.volumes))

