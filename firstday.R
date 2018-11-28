
a<-2

if(a > 5){
print("hello R") 
} else{
  print("less than")
}

test <- function(num1,num2,f){
  if (f==TRUE){
    sumof <- num1+num2
    print(sumof)
  
  }
  if(f ==FALSE)
  {
   mulof <-num1*num2
   print(mulof)
  
  }
  if(num1 == 0 | num2 ==0)
  {
    return(num1+num2)
  }
}
#test(1243,0,FALSE)

for(i in 1:10)
{
  test(i,42142,TRUE)
}


vasfdsf <- function(var1,var2){
  var1(var2)
}

vasfdsf(function(x){cars[1:1,1:1]},0)

blackjack <- function(inp1,inp2){
  if(inp1 > 0 && inp2 >0){
    if(inp1 >21 & inp2 >21){
      return(0)
    }else{
      if(inp1 > inp2){
        return(inp1)
      }else{
        return(inp2)
      }
    }
  }else{
    return("you forgot one or more numbers you twit")
  }
}
print(blackjack(11,19))


addunique <- function(num1,num2,num3){
  amount <- num1+num2+num3
  if(num1 == num2 & num2 == num3)
  {
    amount =  0
  }else{
    if(num1 == num2)
    {
      amount <- amount - (num1+num2)
    }
    if(num1== num3)
    {
      amount <- amount - (num1+num3)
    }
    if(num2==num3)
    {
      amount <- amount - (num2+num3)
    }
  }
  return (amount)
}
print(addunique(43,23,25))



x <-2016
if((x%%4 == 0)&&((x%%400 ==0)||(x%%100 >1))){ print("its a leep year")
}else{"not a leep year"}


newvector <-c(4,8,10,12,2)
write.csv(newvector,"even.csv")

table1 <-read.csv("c:/Users/admin/Documents/even.csv")
table1[,2]+1
summary(iris)
par(mfrow=c(2,2))
plot(Petal.Length ~ Sepal.Length, data=iris)
plot(iris$Sepal.Length,iris$Petal.Length)
plot(iris$Petal.Length, iris$Sepal.Length, pch=c(2,3,4)[iris$Species], col=c("red","blue","black")[iris$Species])

par(mfrow=c(1,1)) ## resetting it back to defult 


boxplot(iris$Petal.Length ~ iris$Species)

?par
?type
typeof(CO2[,2])
mean_uptake <-mean(CO2$uptake)
boxplot(CO2$uptake ~ CO2[CO2$Type == "Quebec"|| CO2$type == "Mississippi",])
quebec_CO2 <- CO2[CO2$Type == "Quebec",]
Mississippi_CO2 <- CO2[CO2$Type == "Mississippi",]
mean_checker <- function(x,y){
  if(x>y)
  {
    print("Mississippi has higher uptake")
  }
  else
  {
    print("quebec has higher uptake")
  }
  }
miss_mean_uptake <-mean(Mississippi_CO2$uptake)
qu_mean_uptake <- mean(quebec_CO2$uptake)

mean_checker(miss_mean_uptake,qu_mean_uptake)

summary (OrchardSprays)
max_decrease <- OrchardSprays[OrchardSprays$decrease == max(OrchardSprays$decrease),]
boxplot(OrchardSprays$decrease ~ OrchardSprays$treatment)

?pch
data(ChickWeight)
summary(ChickWeight)
boxplot(ChickWeight$weight~ ChickWeight$Diet)

d1 <-ChickWeight[ChickWeight$Diet == 1,]
d2 <-ChickWeight[ChickWeight$Diet == 2,] 
d3 <-ChickWeight[ChickWeight$Diet == 3,]
d4 <-ChickWeight[ChickWeight$Diet == 4,]

chickweight<- table(ChickWeight$weight,ChickWeight$Diet)
barplot(morecounts, main="chich weight",xlab="diets",col=c("darkblue","red"))
data(Orange)

regex<- "\\w+"
delta <-"hello my friend"
d <-regexpr(delta,vegex)
print(d)

Wine_Data_Unclean <- read.csv("C:/Users/Admin/Documents/Wine_Data_Unclean.csv",stringsAsFactors = FALSE)

first_new_col <- regmatches(Wine_Data_Unclean$variety_and_region, regexpr("/.+",Wine_Data_Unclean$variety_and_region))
fixers <- sub("/ ","",first_new_col) 
Wine_Data_Unclean$region <- fixers

secound_new_col <- regmatches(Wine_Data_Unclean$region, regexpr("/.+",Wine_Data_Unclean$region))
fixers2 <- sub("/ ","",secound_new_col)
Wine_Data_Unclean$region2 <- fixers2

fixer3 <- sub("/.+","",Wine_Data_Unclean$region)
Wine_Data_Unclean$region <- fixer3

fixer4 <-sub("/.+","",Wine_Data_Unclean$variety_and_region)
Wine_Data_Unclean$variety_and_region <- fixer4

#name(Wine_Data_Unclean) <- Wine_Data_Unclean
colnames(Wine_Data_Unclean)[10] <- "variety"


first_col <- "variety"
secound_col <- "region"
third_col <- "region2"



cleanerPrime <- function(first_col,secound_col,third_col){
  first_new_col <- regmatches(Wine_Data_Unclean$variety_and_region, regexpr("/.+",Wine_Data_Unclean$variety_and_region))
  fixers <- sub("/ ","",first_new_col) 
  Wine_Data_Unclean[secound_col] <- fixers
  
  secound_new_col <- regmatches(Wine_Data_Unclean[secound_col], regexpr("/.+",Wine_Data_Unclean[secound_col]))
  fixers2 <- sub("/ ","",secound_new_col)
  Wine_Data_Unclean[third_col] <- fixers2
  
  fixer3 <- sub("/.+","",Wine_Data_Unclean[secound_col])
  Wine_Data_Unclean[secound_col] <- fixer3
  
  fixer4 <-sub("/.+","",Wine_Data_Unclean$variety_and_region)
  Wine_Data_Unclean$variety_and_region <- fixer4
  
  colnames(Wine_Data_Unclean)[10] <- first_col
  return(Wine_Data_Unclean)
  
}
Wine_Data_Unclean <-cleanerPrime(first_col,secound_col,third_col)


cleanerSub <- function(first_col,secound_col,third_col){
  first_new_col <- regmatches(Wine_Data_Unclean$variety_and_region, regexpr("/.+",Wine_Data_Unclean$variety_and_region))
  fixers <- sub("/ ","",first_new_col) 
  Wine_Data_Unclean$region <- fixers
  
  secound_new_col <- regmatches(Wine_Data_Unclean$region, regexpr("/.+",Wine_Data_Unclean$region))
  fixers2 <- sub("/ ","",secound_new_col)
  Wine_Data_Unclean$region2 <- fixers2
  
  fixer3 <- sub("/.+","",Wine_Data_Unclean$region)
  Wine_Data_Unclean$region <- fixer3
  
  fixer4 <-sub("/.+","",Wine_Data_Unclean$variety_and_region)
  Wine_Data_Unclean$variety_and_region <- fixer4
  
  colnames(Wine_Data_Unclean)[10] <- first_col
  colnames(Wine_Data_Unclean)[11] <- secound_col
  colnames(Wine_Data_Unclean)[12] <- third_col
  
  #view (Wine_Data_Unclean)
  return(Wine_Data_Unclean)
}

Wine_Data_Unclean <- cleanerSub(first_col,secound_col,third_col)

genRename <- function(columNum,columName,tableName)
{
  colnames(tableName)[columNum] <- columName
  return(tableName)
}

Wine_Data_Unclean <- genRename(10,"testWine",Wine_Data_Unclean)
