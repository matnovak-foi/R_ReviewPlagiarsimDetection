# Nested specs:
## code
addition <- function(a, b) a + b
division <- function(a, b) a / b
## specs
describe("math library", {
  context("aa")
  describe("additionaa()", {
    it("can add two numbers", {
      expect_that(addition(1, 1), is_identical_to(1+2))
    })
  })
  describe("division()", {
    it("can divide two numbers", {
      expect_equivalent(10 / 2, division(10, 2))
    })
    it("can handle division by 0") #not yet implemented
  })
})
  
test_results <- test_dir("learning/", reporter="summary")