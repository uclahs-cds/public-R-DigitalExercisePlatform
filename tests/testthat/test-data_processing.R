test_that('adherence data processing works', {
  test.data <- data.frame(
    A.Percent = seq(0, 1, length.out = 5),
    B.Percent = 0.5,
    C.Percent = 0.25,
    A = 1,
    B = 2,
    C = 3
    );
  test.data.long <- adherence.to.long(test.data);
  expect_equal(nrow(test.data.long), 5 * 3);

  test.data.warning <- test.data;

  test.data.warning$A.Percent <- test.data.warning$A.Percent * 2;
  expect_warning(adherence.to.long(test.data.warning));
  });
