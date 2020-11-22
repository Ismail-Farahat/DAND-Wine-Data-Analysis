**TITLE by Ismail M.Farahat**
========================================================
# **The Red Wine Data Exploration**  

## **Info about data**: 

> This tidy data set contains 1,599 red wines with 11 variables on the chemical properties of the wine. At least 3 wine experts rated the quality of each wine, providing a rating between 0 (very bad) and 10 (very excellent).

> Variables of the data:  
   Input variables (based on physicochemical tests):  
   1 - fixed acidity (tartaric acid - g / dm^3)  
   2 - volatile acidity (acetic acid - g / dm^3)  
   3 - citric acid (g / dm^3)  
   4 - residual sugar (g / dm^3)  
   5 - chlorides (sodium chloride - g / dm^)  
   6 - free sulfur dioxide (mg / dm^3)  
   7 - total sulfur dioxide (mg / dm^3)  
   8 - density (g / cm^3)  
   9 - pH  
   10 - sulphates (potassium sulphate - g / dm3)  
   11 - alcohol (% by volume)  
   Output variable (based on sensory data):  
   12 - quality (score between 0 and 10)

```{r echo=FALSE, message=FALSE, warning=FALSE, packages}

# loadibg the required libraries
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(RColorBrewer)
library(knitr)

```

## **The Data Summary**

```{r echo=FALSE, Load_the_Data}

# Load the Data
red_wine <- read.csv("wineQualityReds.csv")

# summary the data
str(red_wine)
summary(red_wine)

```



# **Univariate Plots Section**

```{r echo=FALSE, Univariate_Plots_0}

# Function we need to plot histograms for the data variables.

hists_red_wines <- function(column_name ,plot_name ,x_name ,
                            binwidth = NULL ,n = 1){
  
  max <- as.numeric(max(column_name))
  min <- as.numeric(min(column_name))
  
  q1 <- qplot(x = column_name ,data = red_wine ,
        binwidth = binwidth ,xlab = x_name,
        ylab = "count" ,main = plot_name) +
    scale_x_continuous(
      breaks = seq(min,max,round(((max - min) / 10),n)))
  
  return(q1)
  # arguments <- hists_red_wines(variable,plot_name,x_axis_name,
  #   binwidth[default = NULL],n[degree of Rounding for seq of breakes])
}

```

## **Fixed acidity**

```{r echo=FALSE, Univariate_Plots_1}

# summary the variable fixed.acidity
summary(red_wine$fixed.acidity)

# plot histogram of fixed.acidity. (PLOT 1)
hists_red_wines(red_wine$fixed.acidity,
                "PLOT 1: Fixed acidity",
                "fixed.acidity",0.4)

```

### Observations:
> From this plot,we could see that fixed.acidity distribution is right skewed and there are some few outliers after value of 14 .The fixed.acidity histogram has a peak at a value around 6.8 .And from the summary we could see that most red wines have fixed.acidity values in range from 7.1 to 9.2 with mean of 8.32 and median of 7.9

## **Volatile acidity**

```{r echo=FALSE, Univariate_Plots_2}

# summary the variable volatile.acidity
summary(red_wine$volatile.acidity)

# plot histogram of volatile.acidity (PLOT 2)
hists_red_wines(red_wine$volatile.acidity,
                "PLOT 2: Volatile acidity",
                "volatile.acidity",0.05,2)

```

### Observations:
> From this plot,we could see that volatile.acidity distribution is approximately normal distrbution and there are some few outliers after value of 1.17 .The volatile.acidity histogram has a peak at a value around 0.57 and and another one around 0.42 .And from the summary we could see that most red wines have volatile.acidity values in range from 0.39 to 0.64 with mean of 0.528 and median of 0.52

## **Citric acid**

```{r echo=FALSE, Univariate_Plots_3}

# summary the variable citric.acid
summary(red_wine$citric.acid)

# plot histogram of citric.acid (PLOT 3)
hists_red_wines(red_wine$citric.acid ,
                "PLOT 3: Citric acid" ,
                "citric.acid", 0.03)

```

### Observations:

> From this plot,we could see that there are some few outliers after value of 0.8 and the citric.acid histogram has a peak at the value 0 and another one at the value 0.5 .And from the summary we could see that most red wines have citric.acid values in range from 0.09 to 0.42 with mean of 0.26 and median of 0.27

## **Residual sugar**

```{r echo=FALSE, Univariate_Plots_4}

# summary the variable residual.sugar
summary(red_wine$residual.sugar)

# plot histogram of residual.sugar (PLOT 4)
hists_red_wines(red_wine$residual.sugar ,
                "PLOT 4.1: Residual sugar" ,
                "residual.sugar" ,0.2)

# log10() transformation
qplot(x = residual.sugar ,data = red_wine ,
      ylab = "count" ,main = "PLOT 4.2: log10(residual.sugar)" ,
      binwidth = 0.05) +
  scale_x_log10(breaks = seq(0.9,15.5,2))

```

### Observations:

> From the plot 4.1 we see that the distribution has a long tail so we need to apply a transformation like log10 function on the plot .From the plot 4.2 after appling log10() transformation,we could see that log10(residual.sugar) distribution is right skewed and there are some few outliers after value of 8.9 and the log10(residual.sugar) histogram has a peak at value between 1.9,2.5 .And from the summary we could see that most red wines have residual.sugar values in range from 1.9 to 2.6 with mean of 2.54 and median of 2.2

## **Chlorides**

```{r echo=FALSE, Univariate_Plots_5}

# summary the variable chlorides
summary(red_wine$chlorides)

# plot histogram of chlorides PLOT (PLOT 5)
hists_red_wines(red_wine$chlorides,
                "PLOT 5.1: Chlorides",
                "chlorides",0.01,3)

# log10() transformation
qplot(x = chlorides, data = red_wine ,ylab = "count",
      main = "PLOT 5.2: log10(chlorides)" ,binwidth = 0.05) +
  scale_x_log10()

```

### Observations:

> From the plot 5.1 we see that the distribution has a long tail so we need to apply a transformation like log10 function on the plot .From the plot 5.2 after appling log10() transformation,we could see that log10(chlorides) distribution is approximately normal distribution and there are some few outliers after value of 0.3 and the log10(chlorides) histogram has a peak at value between 0.07,0.1 .And from the summary we could see that most red wines have chlorides values in range from 0.07 to 0.09 with mean of 0.087 and median of 0.079

## **Free sulfur dioxide**

```{r echo=FALSE, Univariate_Plots_6}

# summary the variable free.sulfur.dioxide
summary(red_wine$free.sulfur.dioxide)

# plot histogram of free.sulfur.dioxide (PLOT 6)
hists_red_wines(red_wine$free.sulfur.dioxide ,
                "PLOT 6: Free sulfur dioxide" ,
                "free.sulfur.dioxide", 1)

```

### Observations:

> From this plot,we could see that free.sulfur.dioxide distribution is right skewed and there are outliers after value of 43.6 .The free.sulfur.dioxide histogram has a peak at value between 4.5,6.5 .And from the summary we could see that most red wines have free.sulfur.dioxide values in range from 7 to 21 with mean of 15.87 and median of 14

## **Total sulfur dioxide**

```{r echo=FALSE, Univariate_Plots_7}

# summary the variable total.sulfur.dioxide
summary(red_wine$total.sulfur.dioxide)

# plot histogram of total.sulfur.dioxide (PLOT 7)
hists_red_wines(red_wine$total.sulfur.dioxide ,
                "PLOT 7: Total sulfur dioxide" ,
                "total.sulfur.dioxide", 10)

```

### Observations:

> From this plot,we could see that total.sulfur.dioxide distribution is right skewed and there are some few outliers after value of 170 and the total.sulfur.dioxide histogram has a peak at a value around 20.15.And from the summary we could see that most red wines have total.sulfur.dioxide values in range from 22 to 62 with mean of 46.47 and median of 38

## **Density**

```{r echo=FALSE, Univariate_Plots_8}

# summary the variable density
summary(red_wine$density)

# plot histogram of density (PLOT 8)
hists_red_wines(red_wine$density ,
                "PLOT 8: Density" ,
                "density" ,0.0005 ,4)

```

### Observations:

> From this plot,we could see that density distribution is normal distribution and the density histogram has a peak at a value around 0.99707.And from the summary we could see that most red wines have density values in range from 0.9956 to 0.9978 with mean of 0.9967 and median of 0.9968

## **pH**

```{r echo=FALSE, Univariate_Plots_9}

# summary the variable pH
summary(red_wine$pH)

# plot histogram of pH and summary it (PLOT 9)
hists_red_wines(red_wine$pH ,
                "PLOT 9: pH" ,
                "pH" ,0.03)

```

### Observations:

> From this plot,we could see that pH distribution is normal distribution there are some few outliers after value of 3.84 and the pH histogram has a peak at a value around 3.34 .And from the summary we could see that most red wines have pH values in range from 3.21 to 3.4 with mean of 3.311 and median of 3.31 which means that the red wines are strongly Acidic.

## **Sulphates**

```{r echo=FALSE, Univariate_Plots_10}

# summary the variable sulphates
summary(red_wine$sulphates)

# plot histogram of sulphates (PLOT 10)
hists_red_wines(red_wine$sulphates ,
                "PLOT 10: Sulphates" ,
                "sulphates" ,0.05)

```

### Observations:

> From this plot,we could see that sulphates distribution is right skewed and there are some few outliers after value of 1.33 and the sulphates histogram has a peak at a value around 0.63 .And from the summary we could see that most red wines have sulphates values in range from 0.55 to 0.73 with mean of 0.658 and median of 0.62

## **Alcohol**

```{r echo=FALSE, Univariate_Plots_11}

# summary the variable alcohol
summary(red_wine$alcohol)

# plot histogram of alcohol (PLOT 11)
hists_red_wines(red_wine$alcohol ,
                "PLOT 11: Alcohol" ,
                "alcohol" ,0.2)

```

### Observations:

> From this plot,we could see that the alcohol histogram has a peak at a value around 9.3 and there are some few outliers after value of 13.8. And from the summary we could see that most red wines have alcohol values in range from 9.5 to 11.1 with mean of 10.42 and median of 10.2

## **Quality**

```{r echo=FALSE, message=FALSE, warning=FALSE ,Univariate_Plots_12}

# count the number of every value of quality given by experts
table(red_wine$quality)

# plot histogram of quality (PLOT 12)
hists_red_wines(red_wine$quality ,
                "PLOT 12 : Quality" ,
                "quality")

```

### Observations:

> From this plot,we could see that most red wines have quality values 5 or 6 but 5 is the most value given by experts.And the higest value which red wine is rated by the experts is 8 and the lowest one is 3



# **Univariate Analysis**

## **What is the structure of the dataset?** 

> There are 1599 types of red wine with 12 features [fixed.acidity,volatile.acidity,citric.acid,residual.sugar,chlorides,free.sulfur.dioxide,total.sulfur.dioxide,density,pH,sulphates,alcohol,quality].  
All the features are numeric values but 
quality have values from 1:10 (from worst to best).

## **What were the the Observations i noticed?**

> From previous analysis , i observe :  
1- most red wines have quality of 5 or 6  
2- from the data ,The red wines are strongly Acidic.(ph = 3.21 : 3.4)  
3- most red wines have density from 0.9956 to 0.9978  
4- most red wines have alcohol from 9.5 to 11.1  
5- most red wines have sulphates from 0.55 to 0.73  
6- most red wines have total.sulfur.dioxide from 22 to 62  
7- most red wines have free.sulfur.dioxide from 7 to 21  
8- most red wines have chlorides from 0.07 to 0.09  
9- most red wines have residual.sugar from 1.9 to 2.6  
10- most red wines have citric.acid from 0.09 to 0.42  
11- most red wines have volatile.acidity from 0.39 to 0.64  
12- most red wines have fixed.acidity from 7.1 to 9.2  

## **What is/are the main feature(s) of interest in the dataset?** 

> The quality ,because costomers don't care about the chemical properties of the wine, they care about the quality that the experts give to the wine, and like many things in our world there is always one measurement which represents the value of some thing and this measurement is depended on some properties...so i will study these chemical properties of wines to know what are the chemical properties that affect on the quality.

## **What other features in the dataset do i think will help support the investigation into the feature(s) of interest?**

> All the chemical properties of the wines are important to help me in this investigation beacuse until now ,i don't know what are the  chemical properties that affect on the quality...of course i will find some chemical properties strongly raleted to the quality and some other chemical properties not.

## **There is no new variables i made in this dataset.**

## **There were no any unusual distributions in the data ,the data is organised and completely ready to be investigated.**

=======================================================================

# **Bivariate Plots Section**


```{r echo=FALSE, message=FALSE, warning=FALSE, Bivariate_Plots_0}

# converting quality to a fcator
red_wine$quality <- as.factor(red_wine$quality)

# plot the matrix of all variables in the data
ggpairs(red_wine ,columns = 2:13 ,title = "Matrix for All Variables" ,
        upper = list(continuous = wrap("cor", size = 3)),
        columnLabels = c('f.acidity','v.acidity','c.acid',
                         'r.sugar','chlori','free.SO2',
                         'total.SO2','g/cm^3','pH','sulph',
                         'alcohol','quality'))

```

### **Observations:**

> From this matrix, we could notice the relationships between the variables. It seems that fixed.acidity feature has strong realtionships with variables (citric.acid ,density ,ph) AND volatile.acidity with citric.acid AND free.SO4 with total.SO4 (that makes sence because free.SO4 is part of total.SO4).(we could see that from scatter plots)

> For **quality**, it seems from the boxplots that quality is related to variables of volatile.acidity ,citric.acid ,chlorides ,density ,pH ,sulphates and alcohol

## **Analysing quality with the other variables**

### **Correlations of all variables with quality**

```{r echo=FALSE, Bivariate_Plots_1}

# correlations of quality with all variables of the data
cor(x = red_wine[2:12], y = as.numeric(red_wine$quality))

```

### **Quality and Fixed acidity**

```{r echo=FALSE, Bivariate_Plots_2}

# plot quality and fixed.acidity

ggplot(aes(x = quality,y = fixed.acidity) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  ggtitle("PLOT 1.1: Quality and Fixed acidity")

ggplot(aes(x = quality,y = fixed.acidity) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  coord_cartesian(ylim = c(4,12)) +
  ggtitle("PLOT 1.2: Quality and Fixed acidity")
  

```

#### Observations:

> It seems from this plot that most red wines is given quality with 5 or 6 for most of fixed.acidity values ,and the median is almost constant for the lower ratings then increases by a little amount from rating 4 to 7 but then drop from rating 7 to 8...All these observations mean that quality is not strongly related to fixed.acidity

### **Quality and Volatile acidity**

```{r echo=FALSE, Bivariate_Plots_3}

# plot quality and volatile.acidity

ggplot(aes(x = quality,y = volatile.acidity) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  ggtitle("PLOT 2: Quality and Volatile acidity")

```

#### Observations:

> It seems from this plot that most red wines is given quality with 5 or 6 for most of volatile.acidity values ,and the median decreases by increasing of the quality and from that, quality is related strongly to volatile.acidity (higher volatile.acidity means lower quality)

### **Quality and Citric acid**

```{r echo=FALSE, Bivariate_Plots_4}

# plot quality and citric.acid

ggplot(aes(x = quality,y = citric.acid) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  ggtitle("PLOT 3: Quality and Citric acid")

```

#### Observations:

> It seems from this plot that most red wines is given quality with 5 or 6 for most of citric.acid values ,and the median increases by increasing of the quality and from that, quality is related strongly to citric.acid (higher citric.acid means higher quality)

### **Quality and Residual sugar**

```{r echo=FALSE, Bivariate_Plots_5}

# plot quality and residual.sugar

ggplot(aes(x = quality,y = residual.sugar) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  ggtitle("PLOT 4.1: Quality and Residual sugar")

ggplot(aes(x = quality,y = residual.sugar) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  coord_cartesian(ylim = c(0,4)) +
  ggtitle("PLOT 4.2: Quality and Residual sugar")

```

#### Observations:

> It seems from this plot that most red wines is given quality with 5 or 6 for most of residual.sugar values ,and the median is almost constant for all the rating except rating of 7 so from that, quality is not strongly related to residual.sugar

### **Quality with Chlorides**

```{r echo=FALSE, Bivariate_Plots_6}

# plot quality and chlorides

ggplot(aes(x = quality,y = chlorides) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  ggtitle("PLOT 5.1: Quality and Chlorides")

ggplot(aes(x = quality,y = chlorides) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  coord_cartesian(ylim = c(0.025,0.15)) +
  ggtitle("PLOT 5.2: Quality and Chlorides")

```

#### Observations:

> It seems from this plot that most red wines is given quality with 5 or 6 for most of chlorides values ,and the median decreases by a little amount with increasing of the quality (except for rating 5) and from that, quality is quite related to chlorides (higher chlorides means lower quality)

### **Quality and Free sulfur dioxide**

```{r echo=FALSE, Bivariate_Plots_7}

# plot quality and free.sulfur.dioxide

ggplot(aes(x = quality,y = free.sulfur.dioxide) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  ggtitle("PLOT 6.1: Quality and Free sulfur dioxide")

ggplot(aes(x = quality,y = free.sulfur.dioxide) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  coord_cartesian(ylim = c(0,30)) +
  ggtitle("PLOT 6.2: Quality and Free sulfur dioxide")

```

#### Observations:

> It seems from this plot that most red wines is given quality with 5 or 6 for most of free.sulfur.dioxide values ,and the median increases with lower ratings then reaches to the maximum (rating 5) then decreases untill the higer quality and from that, quality is not related to  free.sulfur.dioxide with a linear realationship (y = mx+c) but the relationship is Quadratic function (y = ax^2+bX+c)

### **Quality and Total sulfur dioxide**

```{r echo=FALSE, Bivariate_Plots_8}

# plot quality and total.sulfur.dioxide

ggplot(aes(x = quality,y = total.sulfur.dioxide) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  ggtitle("PLOT 7.1: Quality and Total sulfur dioxide")

ggplot(aes(x = quality,y = total.sulfur.dioxide) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  coord_cartesian(ylim = c(0,100)) +
  ggtitle("PLOT 7.2: Quality and Total sulfur dioxide")

```

#### Observations:

> It seems from this plot that most red wines is given quality with 5 or 6 for most of total.sulfur.dioxide values ,and the median increases with lower ratings then reaches to the maximum (rating 5) then decreases untill the higer quality and from that, quality is not related to  total.sulfur.dioxide with a linear realationship (y = mx+c) but the relationship is Quadratic function (y = ax^2+bX+c) [like free.sulfur.dioxide feature]

### **Quality and Density**

```{r echo=FALSE, Bivariate_Plots_9}

# plot quality and density

ggplot(aes(x = quality,y = density) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  ggtitle("PLOT 8: Quality and Density")

```

#### Observations:

> It seems from this plot that most red wines is given quality with 5 or 6 for most of density values ,and the median decreases by increasing of the quality (except for rating 4) and from that, quality is related to density (higher density means lower quality)

### **Quality and pH**

```{r echo=FALSE, Bivariate_Plots_10}

# plot quality and pH

ggplot(aes(x = quality,y = pH) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  ggtitle("PLOT 9.1: Quality and pH")

ggplot(aes(x = quality,y = pH) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  coord_cartesian(ylim = c(3,3.7)) +
  ggtitle("PLOT 9.2: Quality and pH")

```

#### Observations:

> It seems from this plot that most red wines is given quality with 5 or 6 for most of pH values ,and the median decreases by increasing of the quality (except for rating 5) and from that, quality is related to pH (higher pH means lower quality)


### **Quality and Sulphates**

```{r echo=FALSE, Bivariate_Plots_11}

# plot quality and sulphates

ggplot(aes(x = quality,y = sulphates) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  ggtitle("PLOT 10.1: Quality and Sulphates")

ggplot(aes(x = quality,y = sulphates) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  coord_cartesian(ylim = c(0.25,1)) +
  ggtitle("PLOT 10.2: Quality and Sulphates")
```

#### Observations:

> It seems from this plot that most red wines is given quality with 5 or 6 for most of sulphates values ,and the median increases by increasing of the quality and from that, quality is related to sulphates (higher sulphates means higher quality)

### **Quality and Alcohol**

```{r echo=FALSE, Bivariate_Plots_12}

# plot quality and alcohol

ggplot(aes(x = quality,y = alcohol) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  ggtitle("PLOT 11.1: Quality and Alcohol")

ggplot(aes(x = quality,y = alcohol) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  coord_cartesian(ylim = c(8,13)) +
  ggtitle("PLOT 11.2: Quality and Alcohol")

```

#### Observations:

> It seems from this plot that most red wines is given quality with 5 or 6 for most of alcohol values ,and the median increases by increasing of the quality (except for rating 5) and from that quality is related to alcohol (higher alcohol means higher quality)


## **Analysing the variables and the relationships between them**

### **Fixed acidity and Citric acid**

```{r echo=FALSE, message=FALSE, warning=FALSE, Bivariate_Plots_13}

# correlation of fixed.acidity and citric.acid
cor.test(red_wine$fixed.acidity , red_wine$citric.acid)


# plot fixed.acidity and citric.acid
ggplot(aes(x = fixed.acidity ,y = citric.acid) ,data = red_wine) +
  geom_jitter(alpha = 1/5 ,color = "orange") +
  ggtitle("PLOT 12: Fixed acidity and Citric acid") +
  geom_smooth()

```

#### Observations:

> It seems that fixed.acidity is strongly related to citric.acid with
correlation of 0.672 (higher fixed.acidity means higher citric.acid)

### **Fixed acidity and Density**

```{r echo=FALSE, message=FALSE, warning=FALSE, Bivariate_Plots_14}

# correlation of fixed.acidity and density
cor.test(red_wine$fixed.acidity , red_wine$density)


# plot fixed.acidity and density
ggplot(aes(x = fixed.acidity ,y = density) ,data = red_wine) +
  geom_jitter(alpha = 1/5 ,color = "orange") +
  ggtitle("PLOT 13: Fixed acidity and Density") +
  geom_smooth()

```

#### Observations:

> It seems that fixed.acidity is strongly related to density with
correlation of 0.668 (higher fixed.acidity means higher density)

#### **Observations about Fixed acidity:**

> From plots 12,13, we could see that Fixed acidity is related to citric.acid and Density.


### **Fixed acidity and pH**

```{r echo=FALSE, message=FALSE, warning=FALSE, Bivariate_Plots_15}

# correlation of fixed.acidity and pH
cor.test(red_wine$fixed.acidity , red_wine$pH)


# plot fixed.acidity and pH
ggplot(aes(x = pH ,y = fixed.acidity) ,data = red_wine) +
  geom_jitter(alpha = 1/5 ,color = "orange") +
  ggtitle("PLOT 14: Fixed acidity and pH") +
  geom_smooth()

```

#### Observations:

> It seems that fixed.acidity is strongly related to pH with
correlation of - 0.683 (higher fixed.acidity means lower pH)

### **Volatile acidity and pH**

```{r echo=FALSE, message=FALSE, warning=FALSE, Bivariate_Plots_16}

# correlation of volatile.acidity and pH
cor.test(red_wine$volatile.acidity , red_wine$pH)


# plot volatile.acidity and pH
ggplot(aes(x = pH ,y = volatile.acidity) ,data = red_wine) +
  geom_jitter(alpha = 1/5 ,color = "orange") +
  ggtitle("PLOT 15: Volatile acidity and pH") +
  geom_smooth()

```

#### Observations:

> It seems that volatile.acidity is not related to pH in a strong clear relationship.

### **Citric acid and pH**

```{r echo=FALSE, message=FALSE, warning=FALSE, Bivariate_Plots_17}

# correlation of citric.acid and pH
cor.test(red_wine$citric.acid , red_wine$pH)


# plot pH and citric.acid
ggplot(aes(x = pH ,y = citric.acid) ,data = red_wine) +
  geom_jitter(alpha = 1/5 ,color = "orange") +
  ggtitle("PLOT 16: Citric acid and pH") +
  geom_smooth()

```

#### Observations:

> It seems that citric.acid is strongly related to pH with
correlation of - 0.542 (higher citric.acid means lower pH)


### **Density and pH**

```{r echo=FALSE, message=FALSE, warning=FALSE, Bivariate_Plots_18}

# correlation of density and pH
cor.test(red_wine$density , red_wine$pH)


# plot pH and density
ggplot(aes(x = pH ,y = density) ,data = red_wine) +
  geom_jitter(alpha = 1/5 ,color = "orange") +
  ggtitle("PLOT 17: Density and pH") +
  geom_smooth()

```

#### Observations:

> It seems that density is quite related to pH with
correlation of - 0.345 (higher density means lower pH)

#### **Observations about pH:**

> From plots 14,15,16,17 , we could see that pH is related to fixed.acidity and citric.acid [that makes sence because scientifcally lower values of pH means higher content of h2 Molecules (acids contains h2 Molecules) which makes the wine acidic] and also with density in a weaker relationship than the first two features

### **Alcohol and pH**

```{r echo=FALSE, message=FALSE, warning=FALSE, Bivariate_Plots_19}

# correlation of alcohol and pH
cor.test(red_wine$alcohol , red_wine$pH)


# plot pH and alcohol
ggplot(aes(x = pH ,y = alcohol) ,data = red_wine) +
  geom_jitter(alpha = 1/5 ,color = "orange") +
  ggtitle("PLOT 18: Alcohol and pH") +
  geom_smooth()

```

#### Observations:

> It seems that alcohol is weakly related to pH with
correlation of 0.206

### **Alcohol and Density**

```{r echo=FALSE, message=FALSE, warning=FALSE, Bivariate_Plots_20}

# correlation of alcohol and density
cor.test(red_wine$alcohol , red_wine$density)


# plot density and alcohol
ggplot(aes(x = density ,y = alcohol) ,data = red_wine) +
  geom_jitter(alpha = 1/5 ,color = "orange") +
  ggtitle("PLOT 19: Alcohol and Density") +
  geom_smooth()

```

#### Observations:

> It seems that alcohol is related to density with
correlation of - 0.496 (higher density means lower alcohol)

### **Alcohol and Residual sugar**

```{r echo=FALSE, message=FALSE, warning=FALSE, Bivariate_Plots_21}

# correlation of alcohol and residual.sugar
cor.test(red_wine$alcohol , red_wine$residual.sugar)


# plot residual.sugar and alcohol
ggplot(aes(x = residual.sugar ,y = alcohol) ,data = red_wine) +
  geom_jitter(alpha = 1/5 ,color = "orange") +
  ggtitle("PLOT 20.1: Alcohol and Residual sugar") +
  geom_smooth()

ggplot(aes(x = residual.sugar ,y = alcohol) ,data = red_wine) +
  geom_jitter(alpha = 1/5 ,color = "orange") +
  coord_cartesian(xlim = c(1,3)) +
  ggtitle("PLOT 20.2: Alcohol and Residual sugar") +
  geom_smooth()

```

#### Observations:

> It seems that alcohol is not related to residual.sugar in a strong clear relationship.(which is not expected because the term **alcohol** originally referred to the primary **alcohol ethanol** (ethyl alcohol) which is naturally produced by the fermentation of **sugars**.[sourse:wikipedia])

#### **Observations about Alcohol: **

> From plots 18,19,20 , we could see that Alcohol is strongly related to density.

### **Total sulfur dioxide and Free sulfur dioxide**

```{r echo=FALSE, message=FALSE, warning=FALSE, Bivariate_Plots_22}

# correlation of total.sulfur.dioxide and free.sulfur.dioxide
cor.test(red_wine$total.sulfur.dioxide , red_wine$free.sulfur.dioxide)


# plot total.sulfur.dioxide and free.sulfur.dioxide
ggplot(aes(x = total.sulfur.dioxide ,y = free.sulfur.dioxide) ,
       data = red_wine) +
  geom_jitter(alpha = 1/5 ,color = "orange") +
  ggtitle("PLOT 21: Total sulfur dioxide and Free sulfur dioxide") +
  geom_smooth()

```

#### **Observations about free SO4 and total SO4:**

> It seems that total.sulfur.dioxide is strongly related to free.sulfur.dioxide with correlation of 0.668 (which makes sence because free SO4 is a part of total SO4) (higher total.sulfur.dioxide means higher free.sulfur.dioxide)


### **Density and Residual sugar**

```{r echo=FALSE, message=FALSE, warning=FALSE, Bivariate_Plots_23}

# correlation of density and residual.sugar
cor.test(red_wine$density , red_wine$residual.sugar)


# plot residual.sugar and density
ggplot(aes(x = residual.sugar ,y = density) ,data = red_wine) +
  geom_jitter(alpha = 1/5 ,color = "orange") +
  ggtitle("PLOT 22.1: Density and Residual sugar") +
  geom_smooth()

ggplot(aes(x = residual.sugar ,y = density) ,data = red_wine) +
  geom_jitter(alpha = 1/5 ,color = "orange") +
  coord_cartesian(xlim = c(0,6)) +
  ggtitle("PLOT 22.2: Density and Residual sugar") +
  geom_smooth()

```

#### Observations about Density and Residual sugar:

> It seems that density is quite related to residual.sugar with correlation of 0.355



# **Bivariate Analysis**


## **How did the feature(s) of interest vary with other features in the dataset?**

> I obseved that :the quality is  related to volatile.acidity ,citric.acid ,chlorides ,density ,pH ,sulphates and alcohol(some of these relationships are strong and some not) and these observations are from the boxplots...for correlation coefficients, we could observe that the strongest relationships are between quality and volatile.acidity, citric.acid ,sulphates ,alcohol

## **what were the interesting relationships between the other features (not the main feature(s) of interest)?** 

> From previous analysis, i observed that :  
1- Fixed acidity is strongly related to citric.acid and Density  
2- pH is related to fixed.acidity and citric.acid  
3- total.sulfur.dioxide is strongly related to free.sulfur.dioxide  
4- Alcohol is strongly related to density  
5- density is quite related to residual.sugar  
6- alcohol is not related to residual.sugar in a strong clear relationship (which is not expected)

## **What was the strongest relationship found?**

> Quality is strongly related to alcohol with corr. 0.476 (the strongest relationship)

=======================================================================

# **Multivariate Plots Section**

## **Matrix of Correlation Cofficients**

```{r echo=FALSE, Multivariate_Plots_0}

# Plot the matrix of correlation cofficients.

ggcorrplot(cor(red_wine[2:12]), 
           p.mat = cor_pmat(red_wine[2:12]),
           type='upper')

```

### Observations :

> From this matrix, we could see that the most poweful relationships are between (density and fixed.acidity) , (pH and fixed.acidity) , (citric.acid, fixed.acidity) , (citric.acid , volatile.acidity) , (pH and citric.acid) ,
(alcohol , density)

> From these orbservations , we will explore these variables and the relationships between them


## **Fixed acidity and Density with Quality**

```{r echo=FALSE, Multivariate_Plots_1}

ggplot(aes(x = fixed.acidity, y = density, color = quality),
       data = red_wine) +
  geom_jitter() +
  scale_color_brewer(type = 'div') +
  ggtitle("PLOT 1: Fixed acidity and Density with Quality")

```

### Observations:

> From this plot , we could see that most higer ratings wines have higher fixed.acidity values with lower density values

## **Fixed acidity and pH with Quality**

```{r echo=FALSE, Multivariate_Plots_2}

ggplot(aes(x = fixed.acidity, y = pH, color = quality),
       data = red_wine) +
  geom_jitter() +
  scale_color_brewer(type = 'div') +
  ggtitle("PLOT 2: Fixed acidity and pH with Quality")

```

### Observations:

> We could not see any organized clear distribution here in this plot

## **Fixed acidity and Citric acid with Quality**

```{r echo=FALSE, Multivariate_Plots_3}

ggplot(aes(x = fixed.acidity, y = citric.acid, color = quality),
       data = red_wine) +
  geom_jitter() +
  scale_color_brewer(type = 'div') +
  ggtitle("PLOT 3: Fixed acidity and Citric acid with Quality")

```

### Observations:

> From this plot , we could see that most higer ratings wines have higher fixed.acidity values with higher citric.acid values

## **Citric acid and Volatile acidity  with Quality**

```{r echo=FALSE, Multivariate_Plots_4}

ggplot(aes(x = citric.acid, y = volatile.acidity, color = quality),
       data = red_wine) +
  geom_jitter() +
  scale_color_brewer(type = 'div') +
  ggtitle("PLOT 4: Citric acid and Volatile acidity  with Quality")

```

### Observaions:

> From this plot , we could see that most higer ratings wines have lower volatile.acidity values with higher citric.acid values


## **Sulphates and Total sulfur dioxide  with Quality**

> From the definition of sulphates: a wine additive which can contribute to sulfur dioxide gas (S02) levels, which acts as an antimicrobial and antioxidant...we need to study the relationship between sulphates and 
sulfur dioxide gas (S02) with quality

```{r echo=FALSE, Multivariate_Plots_5}

ggplot(aes(x = total.sulfur.dioxide, y = sulphates, color = quality),
       data = red_wine) +
  geom_jitter() +
  scale_color_brewer(type = 'div') +
  ggtitle("PLOT 5.1: Sulphates and Total sulfur dioxide  with Quality")

ggplot(aes(x = total.sulfur.dioxide, y = sulphates, color = quality),
       data = red_wine) +
  geom_jitter() +
  coord_cartesian(xlim = c(0,100) , ylim = c(0.25,1)) +
  scale_color_brewer(type = 'div') +
  ggtitle("PLOT 5.2: Sulphates and Total sulfur dioxide  with Quality")

```

### Observations:

> We could see from the plot 5.2 that higher quality ratings wine have lower values of total.sulfur.dioxide with higher values of sulphates


**According to our earlier analysing , we discovered that the quality is related to the alcohol with the strongest relationship so we will explore the alcohol values with some interesting variables and with quality**


## **Alcohol and Fixed acidity  with Quality**

```{r echo=FALSE, Multivariate_Plots_6}

ggplot(aes(x = alcohol, y = fixed.acidity, color = quality),
       data = red_wine) +
  geom_jitter() +
  scale_color_brewer(type = 'div') +
  ggtitle("PLOT 6: Alcohol and Fixed acidity  with Quality")

```

### Observations:

> From this plot , we could see that most higer quality ratings wines have lower fixed.acidity values with higher alcohol values (that is obvious because above the blue green degrees points line we could not see orange degrees points)...the relationship between fixed.acidity and alcohol at higher quality ratings is inverse relationship

## **Alcohol and Volatile acidity with Quality**

```{r echo=FALSE, Multivariate_Plots_7}

ggplot(aes(x = alcohol, y = volatile.acidity, color = quality),
       data = red_wine) +
  geom_jitter() +
  scale_color_brewer(type = 'div') +
  ggtitle("PLOT 7: Alcohol and Volatile acidity  with Quality")

```

### Observations:

> From this plot , we could see that most higer Quality ratings wines have lower volatile.acidity values with higher alcohol values

## **Alcohol and Residual sugar with Quality**

```{r echo=FALSE, Multivariate_Plots_8}

ggplot(aes(x = alcohol, y = residual.sugar, color = quality),
       data = red_wine) +
  geom_jitter() +
  scale_color_brewer(type = 'div') +
  ggtitle("PLOT 8: Alcohol and Residual sugar with Quality")

```

### Observations:

> From this plot , we could see that most higer quality ratings wines have higher alcohol with no big difference in residual.sugar values in the quality ratings.

## **Alcohol and Sulphates with Quality**

```{r echo=FALSE, Multivariate_Plots_9}

ggplot(aes(x = alcohol, y = sulphates, color = quality),
       data = red_wine) +
  geom_jitter() +
  scale_color_brewer(type = 'div') +
  ggtitle("PLOT 9.1: Alcohol and Sulphates with Quality")

ggplot(aes(x = alcohol, y = sulphates, color = quality),
       data = red_wine) +
  geom_jitter() +
  coord_cartesian(ylim = c(0.25,1)) +
  scale_color_brewer(type = 'div') +
  ggtitle("PLOT 9.2: Alcohol and Sulphates with Quality")

```

### Observations:

> From this plot , we could see that most higer quality ratings wines have higher sulphates values with higher alcohol values



# **Multivariate Analysis**

## **The relationships i observed in this part of the investigation:**

> 1- most higer ratings wines have higher fixed.acidity values with lower density values  
2- most higer ratings wines have higher fixed.acidity values with higher citric.acid values  
3- most higer ratings wines have lower volatile.acidity values with higher citric.acid values  
4- higher quality ratings wine have lower values of total.sulfur.dioxide with higher values of sulphates  
5- most higer ratings wines have lower fixed.acidity values with higher alcohol values  
6- most higer ratings wines have lower volatile.acidity values with higher alcohol values  
7- most higer ratings wines have higher sulphates values with higher alcohol values


## **Interesting relationships :**

> From the analysis in part two(Bivariate Plots Section) : we discoverd that quality is not related to total.sulfur.dioxide with a linear realationship (y = mx+c) but the relationship is Quadratic function (y = ax^2+bX+c).  
That means at higher and lower quality ratings, the total.sulfur.dioxide will be at the minimum values but at (5 & 6) quality ratings, the total.sulfur.dioxide will be at the maximum values.  
But when we explored the relationship between Sulphates and Total sulfur dioxide with Quality in this part (POLTs 5.1 , 5.2) , we discoverd that lower values of total.sulfur.dioxide with higher values of sulphates means higher quality but lower values of total.sulfur.dioxide with lower values of sulphates means lower quality.  
In the ealier analysis, we could not discover linear relationship between total.sulfur.dioxide and quality but in this section we discovered a relationship between them but with another variable sulphates.  
Because of the poweful relationship between free.sulfur.dioxide and total.sulfur.dioxide, The anaylsis of total.sulfur.dioxide could be applied to free.sulfur.dioxide (from sec.1 and sec.2, they are identical in most exprolations results)

> From the analysis in part two(Bivariate Plots Section) : we discoverd that quality is not related to fixed.acidity ,but in this section we discovered a relationship between them but with another variable alcohol.


------


# **Final Plots and Summary**

### **Plot One: **

```{r echo=FALSE, Plot_One}

qplot(x = quality ,data = red_wine ,
      main = "PLOT 1: The distribution of Quality Ratings",
      xlab = "Quality Rating [From 1 To 10]",
      ylab = "Count of the Observations")

```

### Description One

> From this plot of quality ratings distribution , we could see that ratings of 5 and 6 have the higher number of observations and it seems that the other ratings have a very few observations and the lowest quality rating is 3 and the highest quality is 8  

> This distribution is good but not perfect because the fewer observations of many ratings will make the median of the values of the chemical properties for these ratings not perfect which affect on our analysis later

### **Plot Two: The Relationship between Quality and Alcohol**

```{r echo=FALSE, Plot_Two}

ggplot(aes(x = quality,y = alcohol) ,data = red_wine) +
  geom_boxplot() +
  geom_jitter(alpha = 1/10 ,color = "blue") +
  coord_cartesian(ylim = c(9,13)) +
  xlab("Quality Rating [From 1 To 10]") +
  ylab("Wine Alcohol Content [% by volume]") +
  ggtitle("PLOT 1: The Relationship between Quality and Alcohol")

```

### Description Two

> From this plot: we could see that the alcohol content for the lower ratings is almost constent then from rating 5 increases highly until rating 8 which means with increasing of alcohol content, the wine quality rating increases.

### **Plot Three: The Effect of Alcohol and Volatile Acidity on Quality**
```{r echo=FALSE, Plot_Three}

ggplot(aes(x = alcohol, y = volatile.acidity, color = quality),
       data = red_wine) +
  geom_jitter() +
  geom_vline(xintercept = 10 ,linetype = 2) +
  geom_hline(yintercept = 0.5 ,linetype = 2) +
  coord_cartesian(ylim = c(0,1.2)) +
  xlab("Wine Alcohol Content [% by volume]") +
  ylab("Volatile Acidity [acetic acid - g / dm^3]") +
  scale_color_brewer(type = 'div') +
  ggtitle("PLOT 3: The Effect of Alcohol and Volatile Acidity on Quality")

```

### Description Three

> From this plot , we could see that most higer ratings wines are existed in the lower right quarter and that means with higher alcohol content and lower volatile.acidity ,the wine will be rated in high ratings with a high percentage.

------

# **Reflection**

> In this exploration of red wines data, I found many good insights to deal with the data like discovering the relationship between [fixed.acidity , volatile.acidity , citric.acid and pH],[total.sulfur.dioxide , free.sulfur.dioxide and sulphates],[residual.sugar , alcohol].  
These insights comes after a research about these variables which was a problem because i knew a little bit about wines and its features.  
In my exploration ,i found many surprising things while investigating this data like when i found that there is no relationship between residual.sugar and alcohol and when i found threre is no linear relationship between total.sulfur.dioxide and quality but later i found a relationship between them but with another variable alcohol.  
In my investigation ,i had some problems and also insights about the solutions and finally i could solve many of these problems and exploring more relationships between variables.  
In this dataset, there are many limitations which makes this analysis not perfect like:  
1- The observations are few for many ratings of quality.  
2- There is only one rating for the quality of the wine , it will be perfect if there are at least there ratings for the quality depending in different methods for rating the wine.  
3- Three experts is not enough to make our analysis denpended on thier ratings  
In the end, if i have another data with more observations and good distribution for quality ratings and more than one rating for every wine ,i am sure that this analysis will be perfect...  
Exploring this data made me have more insights about exploring red wines quality like : i want to compare the chemical properties of red wines with other types of wines to see that there is any difference or not in quality rating for each wine, and another insight like having another variable of the cost of the chemical matriels used in manufacturing the red wine to see that if we could produce a higher quality wine with the lowest possible cost...  
we saw in our investigation that the quality of the wine depended on many properties which comes from chemical matriels...so it will be perfect if tha data have the percentage of the chemical matriels which are the reasons of the features of red wines and thier cost.
