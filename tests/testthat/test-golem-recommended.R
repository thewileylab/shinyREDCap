test_that("app ui", {
  ui <- app_ui()
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(app_ui)
  for (i in c("request")){
    expect_true(i %in% names(fmls))
  }
})

test_that("app server", {
  server <- app_server
  expect_is(server, "function")
  # Check that formals have not been removed
  fmls <- formals(app_server)
  for (i in c("input", "output", "session")){
    expect_true(i %in% names(fmls))
  }
})

# Configure this test to fit your need
# Define function manually, until golem updated beyond v0.2.1 on CRAN
expect_running <- function(sleep){
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  x <- processx::process$new(
    "R", 
    c(
      "-e", 
      "pkgload::load_all(here::here());run_app()"
    )
  )
  Sys.sleep(sleep)
  expect_true(x$is_alive())
  x$kill()
} 
test_that(
  "app launches",{
    skip_if_not(interactive())
    # golem::expect_running(sleep = 5)
    expect_running(sleep = 5)
  }
)
