context("ridgereg")

data("iris")



# ridgereg <- lm
# 

test_that("class is correct", {
  linreg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_s3_class(linreg_mod, "ridgereg")
})

test_that("print() works", {
  linreg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_output(print(linreg_mod),"ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris\\)")
  expect_output(print(linreg_mod),"\\Sepal\\.Width Sepal\\.Length")
})

test_that("predict() works", {
  linreg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_equal(round(unname(predict(linreg_mod)[c(1,5,7)]),2), c(1.85, 1.53, 1.09))    
})


test_that("coef() works", {
  linreg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_true(all(round(unname(coef(linreg_mod)),2) %in% c(-2.52, -1.34,  1.78)))
})
