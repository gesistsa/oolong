# Obtain a locked oolong from a downloaded data file

To generate a locked oolong object with the original oolong object and
the RDS file. The RDS file should have been downloaded from a deployed
Shiny app.

## Usage

``` r
revert_oolong(oolong, rds_file)
```

## Arguments

- oolong:

  an oolong object used for deployment

- rds_file:

  path to the downloaded RDS file

## Value

a locked oolong object based on the data in the downloaded RDS file

## Author

Chung-hong Chan
