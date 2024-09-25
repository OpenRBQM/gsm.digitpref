# splitByPlaces works for simple integers

    Code
      test_result
    Output
      # A tibble: 3 x 5
         given `10^2` `10^1` `10^0` `10^-1`
         <dbl>  <int>  <int>  <int>   <int>
      1 123.00      1      2      3      NA
      2 246.00      2      4      6      NA
      3 369.00      3      6      9      NA

# splitByPlaces works for doubles

    Code
      test_result
    Output
      # A tibble: 3 x 6
          given `10^2` `10^1` `10^0` `10^-1` `10^-2`
          <dbl>  <int>  <int>  <int>   <int>   <int>
      1 123.210      1      2      3       2       1
      2 246.420      2      4      6       4       2
      3 369.630      3      6      9       6       3

