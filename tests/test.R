run_seasonality_test(gcag_ts)
run_seasonality_test(sp500__ts, s=12)

run_seasonality_test(sp500__ts, trend="exponential", s=12)
run_seasonality_test(nq__ts, trend="exponential", s=12)
run_seasonality_test(dj__ts, trend="exponential", s=12)

run_seasonality_test(linear_seasonal, trend="linear", seasons_to_check=1:3, s=12)
run_seasonality_test(linear_non_seasonal, trend="linear", s=12)

run_seasonality_test(quadratic_seasonal, trend="quadratic", s=12)
run_seasonality_test(quadratic_non_seasonal, trend="quadratic", s=12)


run_seasonality_test(exponential_seasonal, trend="quadratic", s=12)
run_seasonality_test(exponential_non_seasonal, trend="quadratic", s=12)
