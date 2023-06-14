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

test_that("accnr convert to db format", {
  # Test case 1: Accession number starting with 'A' before 2000
  parsed <- accnr_parse("A1999/12345")
  expect_equal(accnr_to_database_format(parsed), "199912345")

  # Test case 2: Accession number starting with 'B' after 2000
  parsed <- accnr_parse("B2005/67890")
  expect_equal(accnr_to_database_format(parsed), "200567890")

  # Test case 3: Accession number starting with 'C' before 2000
  parsed <- accnr_parse("C1998/45678")
  expect_equal(accnr_to_database_format(parsed), "399845678")

  # Test case 4: Accession number starting with 'D' after 2000
  parsed <- accnr_parse("D2003/98765")
  expect_equal(accnr_to_database_format(parsed), "400398765")

  # Test case 5: Accession number starting with 'G' before 2000
  parsed <- accnr_parse("G1997/24680")
  expect_equal(accnr_to_database_format(parsed), "799724680")

  # Test case 6: Accession number starting with 'H' after 2000
  parsed <- accnr_parse("H2002/13579")
  expect_equal(accnr_to_database_format(parsed), "800213579")

  # Test case 7: Accession number starting with 'L' before 2000
  parsed <- accnr_parse("L1995/54321")
  expect_equal(accnr_to_database_format(parsed), "599554321")

  # Test case 8: Accession number starting with 'P' after 2000
  parsed <- accnr_parse("P2007/98765")
  expect_equal(accnr_to_database_format(parsed), "900798765")

  # Test case 9: Accession number starting with 'X' before 2000
  parsed <- accnr_parse("X1994/67890")
  expect_equal(accnr_to_database_format(parsed), "699467890")
})

test_that("accnr convert from db format", {
  # Test case 1: Accession number starting with 'A' before 2000
  parsed <- accnr_parse("A1999/12345")
  expect_equal(accdb_parse_to_accnr("199912345"), parsed)

  # Test case 2: Accession number starting with 'B' after 2000
  parsed <- accnr_parse("B2005/67890")
  expect_equal(accdb_parse_to_accnr("200567890"), parsed)

  # Test case 3: Accession number starting with 'C' before 2000
  parsed <- accnr_parse("C1998/45678")
  expect_equal(accdb_parse_to_accnr("399845678"), parsed)

  # Test case 4: Accession number starting with 'D' after 2000
  parsed <- accnr_parse("D2003/98765")
  expect_equal(accdb_parse_to_accnr("400398765"), parsed)

  # Test case 5: Accession number starting with 'G' before 2000
  parsed <- accnr_parse("G1997/24680")
  expect_equal(accdb_parse_to_accnr("799724680"), parsed)

  # Test case 6: Accession number starting with 'H' after 2000
  parsed <- accnr_parse("H2002/13579")
  expect_equal(accdb_parse_to_accnr("800213579"), parsed)

  # Test case 7: Accession number starting with 'L' before 2000
  parsed <- accnr_parse("L1995/54321")
  expect_equal(accdb_parse_to_accnr("599554321"), parsed)

  # Test case 8: Accession number starting with 'P' after 2000
  parsed <- accnr_parse("P2007/98765")
  expect_equal(accdb_parse_to_accnr("900798765"), parsed)

  # Test case 9: Accession number starting with 'X' before 2000
  parsed <- accnr_parse("X1994/67890")
  expect_equal(accdb_parse_to_accnr("699467890"), parsed)
})

