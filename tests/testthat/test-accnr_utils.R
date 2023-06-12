test_that("accnr parsing works", {
  accnr_strs <- c("A2022/12345", "B202398674", "C000000000")
  expected_parsed <- list(
    list(letter = "A", year = 2022, value = 12345),
    list(letter = "B", year = 2023, value = 98674),
    list(letter = "C", year = 0, value = 0)
  )

  for (i in seq_len(length(accnr_strs))) {
    accnr_list <- accnr_parse(accnr_strs[i])
    expect_equal(expected_parsed[[i]], accnr_list)
  }
})

test_that("accnr adding works", {
  accnr_lists <- list(
    list(letter = "A", year = 2022, value = 12345),
    list(letter = "B", year = 2023, value = 98674),
    list(letter = "C", year = 0, value = 0)
  )
  values_to_add <- c(1, 100, 12345)
  expected_parsed <- list(
    list(letter = "A", year = 2022, value = 12346),
    list(letter = "B", year = 2023, value = 98774),
    list(letter = "C", year = 0, value = 12345)
  )

  for (i in seq_len(length(accnr_lists))) {
    accnr_list <- accnr_add(accnr_lists[[i]], values_to_add[i])
    expect_equal(expected_parsed[[i]], accnr_list)
  }
})

test_that("accnr sprinting works", {
  accnr_lists <- list(
    list(letter = "A", year = 2022, value = 12345),
    list(letter = "B", year = 2023, value = 98674),
    list(letter = "C", year = 0, value = 0)
  )
  accnr_strs <- c("A2022/12345", "B2023/98674", "C0000/00000")

  for (i in seq_len(length(accnr_strs))) {
    accnr_str <- accnr_sprint(accnr_lists[[i]])
    expect_equal(accnr_strs[[i]], accnr_str)
  }
})
