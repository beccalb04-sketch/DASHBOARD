# Deploy To shinyapps.io

## App folder

Deploy this folder as a whole:

- `app.R`
- `CMJ-2.csv`
- `RJT-2.csv`
- `imtp-2.csv`
- `IDs-2.xlsx`
- `Stature.xlsx`

The app is configured to read these files from the app directory, so no GitHub raw links are required.

## One-time setup in R


install.packages("rsconnect")
library(rsconnect)
```

Create a `shinyapps.io` account, then copy your account name, token, and secret from the dashboard.

```r
rsconnect::setAccountInfo(name='beccalb04',
			  token='D60A389A2C9009CEE80AC1A4F6CE1620',
			  secret='IixZ/hjw179ta9GkRfw5LJiftUo7jGoet7P4/pCn')
```

## Deploy

```r
rsconnect::deployApp("/Users/rebecca/MSc_DataAssessment")
```

## Redeploy after changes

```r
rsconnect::deployApp("/Users/rebecca/MSc_DataAssessment")
```

## Important checks before sharing

- confirm the dashboard contains no sensitive data you are not allowed to publish
- check the app opens correctly after deployment
- test the child view and coach view
- test `ETC 03` or another athlete with known cohort outputs

## Optional local override

If you ever want the app to read from a different folder locally, set:

```r
Sys.setenv(MSc_DATA_DIR = "/path/to/data")
```

Then restart the app.
