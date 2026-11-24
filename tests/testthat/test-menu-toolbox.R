test_that("download setup selects missing inputs and propagates quit", {
  state <- seq_menu_state()
  calls <- character()
  zone <- data.frame(id = 1)

  local_mocked_bindings(
    seq_select = function(...) 3L,
    seq_select_zone = function(state) {
      calls <<- c(calls, "zone")
      state$zone <- zone
      state$zone_file <- "extent.gpkg"
    },
    seq_select_folder = function(state, caption) {
      calls <<- c(calls, "folder")
      state$path <- tempdir()
    }
  )

  result <- menu_toolbox_setup(
    state,
    action = function(selected) {
      calls <<- c(calls, "download")
      expect_identical(selected$zone, zone)
      expect_identical(selected$path, tempdir())
      -1L
    },
    title = "Download", launch = "Continue", needs_zone = TRUE
  )

  expect_identical(calls, c("zone", "folder", "download"))
  expect_identical(result, -1L)
})

test_that("cancelling extent selection returns without running the tool", {
  state <- seq_menu_state()
  state$path <- tempdir()
  selections <- c(3L, 0L)
  calls <- character()

  local_mocked_bindings(
    seq_select = function(...) {
      selected <- selections[[1]]
      selections <<- selections[-1]
      selected
    },
    seq_select_zone = function(state) {
      calls <<- c(calls, "zone")
      invisible(NULL)
    },
    seq_select_folder = function(...) calls <<- c(calls, "folder")
  )

  result <- menu_toolbox_setup(
    state, action = function(...) calls <<- c(calls, "download"),
    title = "Download", launch = "Continue", needs_zone = TRUE
  )

  expect_identical(calls, "zone")
  expect_identical(state$path, tempdir())
  expect_null(state$zone)
  expect_identical(result, 0L)
})

test_that("cancelling output selection keeps the extent without downloading", {
  state <- seq_menu_state()
  state$zone <- data.frame(id = 1)
  state$zone_file <- "extent.gpkg"
  selections <- c(3L, 0L)
  calls <- character()

  local_mocked_bindings(
    seq_select = function(...) {
      selected <- selections[[1]]
      selections <<- selections[-1]
      selected
    },
    seq_select_zone = function(...) calls <<- c(calls, "zone"),
    seq_select_folder = function(...) {
      calls <<- c(calls, "folder")
      invisible(NULL)
    }
  )

  result <- menu_toolbox_setup(
    state, action = function(...) calls <<- c(calls, "download"),
    title = "Download", launch = "Continue", needs_zone = TRUE
  )

  expect_identical(calls, "folder")
  expect_null(state$path)
  expect_identical(state$zone, data.frame(id = 1))
  expect_identical(result, 0L)
})

test_that("other toolbox tools reuse the output folder without showing an extent", {
  for (tool in c(2L, 3L)) {
    state <- seq_menu_state()
    state$path <- tempdir()
    state$zone <- data.frame(id = 1)
    state$zone_file <- "extent.gpkg"
    selections <- c(tool, 2L)
    labels <- character()
    calls <- character()

    local_mocked_bindings(
      seq_select = function(info = NULL, ...) {
        if (is.function(info)) info()
        selected <- selections[[1]]
        selections <<- selections[-1]
        selected
      },
      seq_show_selection = function(value, label, ...) {
        labels <<- c(labels, label)
      },
      seq_select_zone = function(...) calls <<- c(calls, "zone"),
      seq_select_folder = function(...) calls <<- c(calls, "folder"),
      menu_pm = function(selected) {
        expect_identical(selected$path, tempdir())
        calls <<- c(calls, "search")
        -1L
      },
      menu_rp = function(selected) {
        expect_identical(selected$path, tempdir())
        calls <<- c(calls, "convert")
        -1L
      }
    )

    expect_identical(menu_toolbox(state), -1L)
    expect_identical(calls, if (tool == 2L) "search" else "convert")
    expect_identical(labels, "Dossier de sortie")
  }
})
