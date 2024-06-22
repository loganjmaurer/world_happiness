# world_happiness
An Analysis of the Happiness Rankings by the Gallop World Poll
https://www.kaggle.com/datasets/unsdsn/world-happiness
Which country has the happiest people, and what factors influence this?
This data set has survey, economic and health data for most countries in the workld. All of this is available for the years 2015 to 2019.
It uses results from a Gallup Poll that asked people in each country to rate their happiness.
This data is presented with per capital GDP, life expectancy and some sort of measure of freedom, family support, generosity, and absence of corruption, though I am not fully clear on how some of that data is collected, measured and reported. 

What I really have now are some visualizations and two models.
I created another variable that discretizes happiness into five ranks. This allowed me to compare the distribution of predictors between bins using violin plots.
I also used the original continuous happiness variable to create a correlation matrix.
I now have my response in two separate variables of two distinct classes, so I fit different models on each of them, proportional odds logistic regression and linear regression specifically. 

My next steps are to compare the performance of these two models to test the merits of my discretization. I suspect is was pointless besides allowing me to make pretty plots.
I am thinking of some way to compare by country and show trends between years as well.
