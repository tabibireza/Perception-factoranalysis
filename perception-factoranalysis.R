# consumer perception - factor analysis  
getwd ()
cardata = read.csv(file = "cardata.csv")
head(cardata)

# Determaing the number of factors 
install.packages("nFactors")
library(nFactors)
nScree(cardata[,4:9], cor=TRUE)

#  run factor analysis  variance it is explained  varying number of factors
fit <- principal(cardata [,4:9], nfactors = 5, rotate="none")
fit$loadings 

# a rotated three-factor solution. 
fit <- principal(cardata[, 4:9], nfactors = 3, rotate = "varimax")
fit$loadings

# Naming the factors
colnames(fit$weights)= c("Appearance", "Performance", "Comfort")
fit$weights
# Appearance=0.45Trendy + 0.40Styling + … -0.09Comfort

# the factor scores of respondent
colnames(fit$scores)=c ("Appearance", "Performance", "Comfort")
reduce_data= cbind(cardata[,1:3], fit$scores)
head(reduce_data)

#  compute the average factor scores for each car brand
attach(reduce_data)
brand.mean= aggregate(reduce_data[,c(4:6)], by=list(Brand), FUN=mean, na.rm=TRUE)
detach(reduce_data)
head(brand.mean)

# perceptual map
attach(brand.mean)

# 3D Scatterplot
library(scatterplot3d)
s3d <- scatterplot3d(Appearance, Performance, Comfort, scale.y=1, type='h', asp=1, main="3D perceptual Map")
text(s3d$xyz.convert(Appearance, Performance, Comfort+ c(rep(0.05,5),0.1)), labels=(brand.mean[,1]), col='red')

#  respondents perceive cars differently
# Print the average factor scores by brand and education
detach(brand.mean)
attach(reduce_data)
brand_by_edu=aggregate(reduce_data[,c(4:6)], by=list(Brand, Education), FUN=mean, na.rm=TRUE)
colnames(brand_by_edu)= c("Brand", "Edu", "Appearance", "Performance", "Comfort")
detach(reduce_data)
brand_by_edu

# a joint map of MBA and undergraduate students  3D Scatterplot
attach(brand_by_edu)
library(scatterplot3d)
s3d1<- scatterplot3d(brand_by_edu[,3], brand_by_edu[,4], brand_by_edu[,5], xlab="Appearance", ylab="Performance", zlab="Comfort", 
                     scale.y=1, type='h', asp=1, main='3D Perceptual Map')
tmp <- brand_by_edu[which(brand_by_edu$Edu =='MBA'),]
text(s3d1$xyz.convert(tmp$Appearance, tmp$Performance, tmp$Comfort+ c(rep(0.05,5),0.1)), 
     labels=(tmp$Brands), col='darkgreen')
tmp <- brand_by_edu[which(brand_by_edu$Edu=='Undergrad'),]
text(s3d1$xyz.convert(tmp$Appearance, tmp$Performance, tmp$Comfort+ c(rep(0.05,5),0.1)), 
     labels=(tmp$Brands), col='red')
legend(-3,8, legend = c("MBA", "Undergrad"), col=c("red", "darkgreen"), lty=1, cex=0.8)

# the drivers of car preference using regression analysis
# Append preference data
red_data= cbind(cardata[,c(2,3,10)], fit$scores)
colnames= c("Brand", "Edu", "Preference", "Appearance", "Performance", "Comfort")
head(red_data)

# Multiple Linear Regression 
regfit <- lm(Preference ~ Appearance + Performance+ Comfort, data=red_data)
summary(regfit) # show results 






