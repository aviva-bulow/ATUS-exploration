respondents <- read.csv("atusresp_2018.dat")
actsum <- read.csv("atussum_2018.dat")

##the following values are based on ATUS
income_min = 0
income_max = 288461

explore_data <- function(time_use_catagories = c("t020201", "t020202", "t020203"),
                         income_divider = 300,
                         income_type = "TEERN"){
  income_divider = income_divider*100 ##account for the 2 implied decimal places

  #explore basic results with low vs high income divider
  get.results(income_divider, income_type, time_use_catagories, respondents)

  ##remove people who work in the food industry
  ##TEIO1ICD == industry work code
  ##8680 == industry: Restaurants and other food services
  ##8690 == industry: Drinking places, alcoholic beverages
  respondents <- respondents[-which(respondents[["TEIO1ICD"]] %in% c(8680, 8690)),]
  print("got respondents outside of food")
  get.results(income_divider, income_type, time_use_catagories, respondents)

  ##TEIO1OCD == occupation code
  ## 4000: Chefs and head cooks; 4020: Cooks;
  ## 4030: Food preparation workers; 4040; Bartenders;
  ## 4050: Combined food preparation and serving workers, including fast food;
  ## 4140: Dishwashers;
  food_codes = c(4000, 4020, 4030, 4040, 4050, 4140)
  print(names(respondents)[1:5])
  respondents <- respondents[-which(respondents[["TEIO1OCD"]] %in% food_codes),]
  ##print(respondents[1:5])

  print("got respondents with occupations outside of the food industry")
  get.results(income_divider, income_type, time_use_catagories, respondents)

}

get.results <- function(income_divider, income_type, cates, resp){
  low_income_min = income_min
  high_income_max = income_max

  low_income_max = income_divider
  high_income_min = income_divider

	par(mfrow = c(2,2))
	high_income_max <- resp[which(resp[[income_type]] < high_income_max),]$TUCASEID
	high_income_min <- resp[which(resp[[income_type]] > high_income_min),]$TUCASEID
	high_inc <- resp[which(resp$TUCASEID %in% intersect(high_income_max, high_income_min)),]
	print(high_inc[1:5, 1:5])
	low_income_max <- resp[which(resp[[income_type]] < low_income_max),]$TUCASEID
	low_income_min <- resp[which(resp[[income_type]] >= low_income_min),]$TUCASEID

	low_inc <- resp[which(resp$TUCASEID %in% intersect(low_income_max, low_income_min)),]
	print("working1")
	resmat_high <- matrix(nrow = length(high_inc[,1]), ncol = length(cates) + 1)
  print("working2")
	resmat_low <- matrix(nrow = length(low_inc[,1]), ncol = length(cates) + 1)
	resmat_low[,1] <- low_inc$TUCASEID
	resmat_high[,1] <- high_inc$TUCASEID
	print("working3")


	i <- 1
	for(n in cates){

		for(j in 1:length(high_inc$TUCASEID)){
			resmat_high[j,(i+1)] <- actsum[which(actsum$TUCASEID == high_inc[j,]$TUCASEID),which(names(actsum) == n)]
		}
		for(j in 1:length(low_inc$TUCASEID)){
			resmat_low[j,(i+1)] <- actsum[which(actsum$TUCASEID == low_inc[j,]$TUCASEID),which(names(actsum) == n)]
		}
		i <- i + 1
	}
	actsum_high <- vector(length = length(resmat_high[,1]))
	for(i in 1:length(actsum_high)){
		actsum_high[i] <- sum(resmat_high[i, 2:length(resmat_high[1,])])
	}
	actsum_low <- vector(length = length(resmat_low[,1]))
	for(i in 1:length(actsum_low)){
		actsum_low[i] <- sum(resmat_low[i, 2:length(resmat_low[1,])])
	}
	plot(sort(high_inc[[income_type]]), main = "High Income Wages", xlab = NULL, ylab = income_type)
	plot(sort(low_inc[[income_type]]), main = "Low Income Wages", ylab = income_type)
	boxplot(actsum_high, main = paste("High Income Results"))
	boxplot(actsum_low, main = paste("Low Income Results"))
	print(paste("High earner count", length(actsum_high)))
	print(summary(actsum_high))
	print(paste("Low earner count", length(actsum_low)))
	print(summary(actsum_low))
}