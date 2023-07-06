test_that("accnr parsing works", {
  accnr_strs <- c("A2022/12345", "B202398674", "C000000000")
  expected_parsed <- tibble(
    letter = c("A", "B", "C"),
    year = c(2022, 2023, 0),
    value = c(12345, 98674, 0)
  )

  expect_equal(accnr_parse(accnr_strs), expected_parsed)
})

test_that("accnr adding works", {
  accnr_tib <- tibble(
    letter = c("A", "B", "C"),
    year = c(2022, 2023, 0),
    value = c(12345, 98674, 0)
  )
  values_to_add <- c(1, 100, 12345)
  expected_value <- tibble(
    letter = c("A", "B", "C"),
    year = c(2022, 2023, 0),
    value = c(12346, 98774, 12345)
  )

  expect_equal(accnr_add(accnr_tib, values_to_add), expected_value)
})

test_that("accnr sprinting works", {
  accnr_tib <- tibble(
    letter = c("A", "B", "C"),
    year = c(2022, 2023, 0),
    value = c(12345, 98674, 0)
  )
  accnr_strs <- c("A2022/12345", "B2023/98674", "C0000/00000")

  expect_equal(accnr_sprint(accnr_tib), accnr_strs)
})

test_that("accnr convert to db format", {
  # Test case 1: Accession number starting with 'A' before 2000
  parsed <- accnr_parse("A1999/12345")
  expect_equal(accnr_db_sprint(parsed), "199912345")

  # Test case 2: Accession number starting with 'B' after 2000
  parsed <- accnr_parse("B2005/67890")
  expect_equal(accnr_db_sprint(parsed), "200567890")

  # Test case 3: Accession number starting with 'C' before 2000
  parsed <- accnr_parse("C1998/45678")
  expect_equal(accnr_db_sprint(parsed), "399845678")

  # Test case 4: Accession number starting with 'D' after 2000
  parsed <- accnr_parse("D2003/98765")
  expect_equal(accnr_db_sprint(parsed), "400398765")

  # Test case 5: Accession number starting with 'G' before 2000
  parsed <- accnr_parse("G1997/24680")
  expect_equal(accnr_db_sprint(parsed), "799724680")

  # Test case 6: Accession number starting with 'H' after 2000
  parsed <- accnr_parse("H2002/13579")
  expect_equal(accnr_db_sprint(parsed), "800213579")

  # Test case 7: Accession number starting with 'L' before 2000
  parsed <- accnr_parse("L1995/54321")
  expect_equal(accnr_db_sprint(parsed), "599554321")

  # Test case 8: Accession number starting with 'P' after 2000
  parsed <- accnr_parse("P2007/98765")
  expect_equal(accnr_db_sprint(parsed), "900798765")

  # Test case 9: Accession number starting with 'X' before 2000
  parsed <- accnr_parse("X1994/67890")
  expect_equal(accnr_db_sprint(parsed), "699467890")
})

test_that("accnr convert from db format", {
  # Test case 1: Accession number starting with 'A' before 2000
  expected <- accnr_parse("A1999/12345") |> as.data.frame()
  actual <- accdb_parse("199912345") |> as.data.frame()
  expect_equal(actual, expected)

  # Test case 2: Accession number starting with 'B' after 2000
  expected <- accnr_parse("B2005/67890") |> as.data.frame()
  actual <- accdb_parse("200567890") |> as.data.frame()
  expect_equal(actual, expected)

  # Test case 3: Accession number starting with 'C' before 2000
  expected <- accnr_parse("C1998/45678") |> as.data.frame()
  actual <- accdb_parse("399845678") |> as.data.frame()
  expect_equal(actual, expected)

  # Test case 4: Accession number starting with 'D' after 2000
  expected <- accnr_parse("D2003/98765") |> as.data.frame()
  actual <- accdb_parse("400398765") |> as.data.frame()
  expect_equal(actual, expected)

  # Test case 5: Accession number starting with 'G' before 2000
  expected <- accnr_parse("G1997/24680") |> as.data.frame()
  actual <- accdb_parse("799724680") |> as.data.frame()
  expect_equal(actual, expected)

  # Test case 6: Accession number starting with 'H' after 2000
  expected <- accnr_parse("H2002/13579") |> as.data.frame()
  actual <- accdb_parse("800213579") |> as.data.frame()
  expect_equal(actual, expected)

  # Test case 7: Accession number starting with 'L' before 2000
  expected <- accnr_parse("L1995/54321") |> as.data.frame()
  actual <- accdb_parse("599554321") |> as.data.frame()
  expect_equal(actual, expected)

  # Test case 8: Accession number starting with 'P' after 2000
  expected <- accnr_parse("P2007/98765") |> as.data.frame()
  actual <- accdb_parse("900798765") |> as.data.frame()
  expect_equal(actual, expected)

  # Test case 9: Accession number starting with 'X' before 2000
  expected <- accnr_parse("X1994/67890") |> as.data.frame()
  actual <- accdb_parse("699467890") |> as.data.frame()
  expect_equal(actual, expected)
})
