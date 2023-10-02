q1 = function() { 

	pExact = choose(9,2)*choose(23,4)/choose(32,6)

	

	meat = paste(1:9, "M", sep="")

	sea = paste(1:12, "S", sep="")

	veg = paste(1:11, "V", sep="")



	toppings = c(meat, sea, veg)

	

	length(index[index <= 9])



	totalPizzas = 200

	twoMeat = 0 

	

	for(i in 1:totalPizzas) {

		index = sample(1:32, 6, replace=FALSE)

		meatToppings = length(index[index <= 9])

		

		if(meatToppings == 2)

			twoMeat = twoMeat + 1

	}

	

	pSim = twoMeat/totalPizzas

	

	round(c(pExact, pSim), 4)

}



q2 = function() {

	pExact = 1-(choose(6,3)*factorial(3))/6**3



	toppings = c("T1", "T2", "T3", "T4", "T5", "T6")



	totalPizzas = 200

	duplicate = 0



	for(i in 1:totalPizzas) {

		index = sample(1:6, 3, replace=TRUE)



		if(toppings[index[1]] == toppings[index[2]] || toppings[index[1]] == toppings[index[3]] || toppings[index[2]] == toppings[index[3]])

			duplicate = duplicate + 1

	}



	pSim = duplicate/totalPizzas



	round(c(pExact, pSim), 4)

}



q3 = function() {

	pExact = pbinom(3, 140, 1/choose(7, 4), lower.tail = FALSE)

	

	toppings = c("T1", "T2", "T3", "T4", "T5", "T6", "T7")



	totalRuns = 200

	fourToppings = 0

	pizzaCounter = 0



	for(i in 1:totalRuns) {

		for(i in 1:140) {

			topSample = c(sample(toppings, 4, replace=FALSE))



			if(any("T1" == topSample) & any("T2" == topSample) & any("T3" == topSample) & any("T4" == topSample))

				pizzaCounter = pizzaCounter + 1

		} 

		if(pizzaCounter >= 4)

			fourToppings = fourToppings + 1



		pizzaCounter = 0



	}



	pSim = fourToppings / totalRuns



	round(c(pExact, pSim), 4)

		

}



q4 = function() {

	pExact = 1-ppois(4, 4.55)



	accidents = c(rpois(200, 4.55))

	

	totalAccidents = 200

	fiveAcc = 0

	counter = 1



	for(i in 1:totalAccidents) {

		if(accidents[counter] >=  5)

			fiveAcc = fiveAcc + 1

		counter = counter + 1

	}



	pSim = fiveAcc/totalAccidents



	round(c(pExact, pSim), 4)	

}



q5 = function() {

	pExact =  pnorm(167, mean = 134, sd = 41, lower.tail = TRUE) - pnorm(72.999999, mean = 134, sd = 41, lower.tail = TRUE) 



	pizzasDaily = c(rnorm(200, 134, 41))



	totalDays = 200

	withinBounds = 0

	counter = 1



	for(i in 1:totalDays) {

		if(pizzasDaily[counter] >= 73 & pizzasDaily[counter] <= 167)

			withinBounds = withinBounds + 1

		counter = counter + 1

	}



	pSim = withinBounds/totalDays



	round(c(pExact, pSim), 4)

}