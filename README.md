# analyticsutilities

An R package with analytics utility functions for built for the RDAC Analytics unit.

## Functions

| Function | Description |
|---|---|
| `pxget_riceArea()` | Fetch rice area harvested from PSA PXWeb API |
| `pxget_riceProd()` | Fetch rice production volume from PSA PXWeb API |
| `pxget_riceStocks()` | Fetch monthly rice stocks inventory from PSA PXWeb API |
| `pxget_cornArea()` | Fetch corn area harvested from PSA PXWeb API |
| `pxget_cornPrice()` | Fetch farmgate corn prices from PSA PXWeb API |
| `sbget_payPrism()` | Query PRiSM PAY data from a Supabase REST API |
| `get_psgc_pop2024()` | Build tidy PSGC 2024 geographic tables from PSA Excel file |
| `psa_pxweb_urls()` | List of PSA OpenStat PXWeb API URLs |
| `extend_time_series()` | Extend a time series into the future |
| `import_from_gdrive()` | Import a file from Google Drive |
| `clean_location()` | Standardise Philippine region/province names |
| `calculate_period_days()` | Compute number of days in a reporting period |
| `end_of_month()` | Return the last day of each month in a date vector |

## Installation

```r
# Install from GitHub
remotes::install_github("nmfrancisco/analytics_utilities")
```

## Requirements

- R >= 4.1.0
