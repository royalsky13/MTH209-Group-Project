##Generating a synthetic dataset for female cancer patients 
FRE_F <- c(13709, 94166, 425918, 215458)
dfem <- rep(c(15,40, 65, 80), FRE_F)

### first decide the bandwidth
bw1 = 4.89
density(dfem, bw = bw1)
f <- density(dfem, bw = bw1)$x #female age values
density_f <- density(dfem, bw = bw1)$y #corresponding densities

######Plotting the density against the values
plot(f, density_f, type = "l", xlab = "Age of Female Cancer Patients",
     ylab = "density")

########## Now we calculate the  empirical distribution function
sorted_vals_f <- sort(f)
#cumulative density
cum_density_f <- cumsum(density_f)

ecdf_f <- cum_density_f/sum(density_f) 
plot(sorted_vals_f, ecdf_f, type = "s", 
     main = "Empirical CDF", xlab = "Age of female cancer patients", ylab = "Cumulative Probability")


###### Plotting the normal cdf 
#plotting normal cdf
# Calculate normal CDF
mu <- mean(sorted_vals_f)
sigma <- sd(sorted_vals_f)
normal_cdf <- pnorm(sorted_vals_f, mean = mu, sd = sigma)
lines(sorted_vals_f, normal_cdf, col = "red")



######Calculating Gamma CDF
scale_f = (sigma^2)/mu
shape_f = mu/scale_f
gamma_cdf <- pgamma(sorted_vals_f, shape = shape_f , scale = scale_f)
lines(sorted_vals_f, gamma_cdf, col = "green")
legend("bottomright", legend = c("Empirical CDF", "Normal CDF", "Gamma CDF"), 
       lty = 1, col = c("black", "red", "green"))

###Cross Chescking using Shapiro Test whether they are from normal population
shapiro.test(f)

##### KS Test
ks.test(f, "pgamma", shape_f, scale_f) 








############## For males


##Generating a synthetic dataset for female cancer patients 
FRE_M <- c(21308, 74872, 341230, 274766)
dma <- rep(c(15,40, 65, 80), FRE_M)

### first decide the bandwidth
bw2 = 4.89
density(dma, bw = bw2)
m <- density(dma, bw = bw2)$x #male age values
density_m <- density(dma, bw = bw2)$y #corresponding densities

######Plotting the density against the values
plot(m, density_m, type = "l", xlab = "Age of male Cancer Patients",
     ylab = "density")

########## Now we calculate the  empirical distribution function
sorted_vals_m <- sort(m)
#cumulative density
cum_density_m <- cumsum(density_m)

ecdf_m <- cum_density_m/sum(density_m) 
plot(sorted_vals_m, ecdf_m, type = "s", 
     main = "Empirical CDF", xlab = "Age of male cancer patients", ylab = "Cumulative Probability")


###### Plotting the normal cdf 
#plotting normal cdf
# Calculate normal CDF
mu_m <- mean(sorted_vals_m)
sigma_m <- sd(sorted_vals_m)
normal_cdf_m <- pnorm(sorted_vals_m, mean = mu_m, sd = sigma_m)
lines(sorted_vals_m, normal_cdf_m, col = "red")



######Calculating Gamma CDF
scale_m = (sigma_m^2)/mu_m
shape_m = mu_m/scale_m
gamma_cdf_m <- pgamma(sorted_vals_m, shape = shape_m , scale = scale_m)
lines(sorted_vals_m, gamma_cdf_m, col = "green")
legend("bottomright", legend = c("Empirical CDF", "Normal CDF", "Gamma CDF"), 
       lty = 1, col = c("black", "red", "green"))

ks.test(f, "pgamma", shape_m, scale_m) #testing whether gamma dist or not

############# two empirical cdf's head ot head
plot <- plot(sorted_vals_m, ecdf_m, type = "s", 
             main = "Empirical CDF's", xlab = "Age of male & female cancer patients", 
             ylab = "Cumulative Probability")
lines(sorted_vals_f, ecdf_f, type = "s", col = "pink")
legend("topleft", legend = c("Empirical CDF females", "Empirical CDF females"), 
       lty = 1, col = c("black", "pink"))

########Do x and y come from same distribution?

ks.test(f, m) #testing whether Two dists are similar or not
#they have ideantical distribution


