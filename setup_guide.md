# Mbuji-Mayi Biobank Dashboard - Setup Guide

## Quick Start

### 1. Project Structure

Create this folder structure:

```
mbuji-mayi-biobank/
├── app.R                 # Main application file
├── config.yml            # Configuration file
├── R/
│   ├── utils_parse.R     # Data parsing utilities
│   ├── utils_join.R      # Join utilities
│   └── mod_*.R           # Additional modules (optional)
├── testdata/             # Sample data for testing
└── renv.lock             # Package dependencies (created by renv)
```

### 2. Install Required Packages

```r
# Install packages
install.packages(c(
  "shiny", "bslib", "tidyverse", "readxl", "janitor", 
  "lubridate", "DT", "sf", "leaflet", "stringi", "bsicons",
  "yaml", "scales", "purrr"
))

# For production deployment
install.packages("renv")
```

### 3. Initialize Project

```r
# In RStudio, create a new project
# File > New Project > Existing Directory > mbuji-mayi-biobank

# Initialize renv for reproducibility
renv::init()

# Take a snapshot of current packages
renv::snapshot()
```

### 4. Configure Your Paths

Edit `config.yml` to match your system:

```yaml
paths:
  biobank_dir: "YOUR_PATH_HERE/01 - Biobanque"
  extractions_dir: "YOUR_PATH_HERE/02 - Extractions"
  # ... etc
```

### 5. Run the App Locally

```r
# In RStudio
shiny::runApp()

# Or from command line
Rscript -e "shiny::runApp('app.R')"
```

## Development Workflow

### Testing Changes

1. Make changes to `app.R` or utility files
2. Save the file
3. The app will automatically reload (if using RStudio's "Run App" button)
4. Test your changes

### Adding New Features

1. Create new module files in `R/mod_*.R`
2. Source them in `app.R`
3. Add UI and server components
4. Test thoroughly

### Debugging

Enable debugging in the app:

1. Go to the "Debug" tab in the app
2. Check configuration and data info
3. Use browser() in server code for breakpoints

## Deployment to shinyapps.io

### First-Time Setup

```r
# Install rsconnect
install.packages("rsconnect")

# Configure your account (get these from shinyapps.io)
rsconnect::setAccountInfo(
  name = "YOUR_ACCOUNT",
  token = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)
```

### Deploy

```r
# Deploy the app
rsconnect::deployApp(
  appName = "mbuji-mayi-biobank",
  appFiles = c(
    "app.R",
    "config.yml",
    "R/utils_parse.R",
    "R/utils_join.R"
  ),
  forceUpdate = TRUE
)
```

### Update Deployment

```r
# After making changes
rsconnect::deployApp(forceUpdate = TRUE)
```

## Deployment to Posit Connect

```r
# Configure Connect
rsconnect::addConnectServer(
  url = "https://your-connect-server.com",
  name = "your-connect"
)

# Deploy
rsconnect::deployApp(
  server = "your-connect",
  appName = "mbuji-mayi-biobank"
)
```

## System Dependencies

### For sf and leaflet (map features)

**Ubuntu/Debian:**
```bash
sudo apt-get install -y \
  libgdal-dev \
  libgeos-dev \
  libproj-dev \
  libudunits2-dev
```

**macOS:**
```bash
brew install gdal geos proj
```

**Windows:**
These are usually included with the R packages.

## Troubleshooting

### "Package not found" errors

```r
# Restore packages from lockfile
renv::restore()
```

### "Directory not found" errors

1. Check paths in `config.yml`
2. Make sure you're using forward slashes `/` or double backslashes `\\`
3. Test paths in R console:
   ```r
   dir.exists("YOUR_PATH_HERE")
   ```

### Deployment fails with "system dependency" error

Install the required system libraries (see above) on your deployment server.

### App is slow

1. Check if you're loading large files
2. Consider caching:
   ```r
   # In server function
   data_cached <- memoise::memoise(load_data_function)
   ```
3. Use server-side DataTables for large tables:
   ```r
   DT::datatable(data, server = TRUE)
   ```

### Map won't load

1. Check internet connection (if using online GRID3)
2. Try uploading a local shapefile instead
3. Check browser console for JavaScript errors

## Testing

### Create Test Data

```r
# Create minimal test dataset
test_data <- tibble(
  barcode = paste0("KPS", 1:50),
  lab_id = 1:50,
  date_sample = seq.Date(Sys.Date() - 90, Sys.Date(), length.out = 50),
  age = sample(1:80, 50, replace = TRUE),
  sex = sample(c("M", "F"), 50, replace = TRUE),
  study = sample(c("DA", "DP"), 50, replace = TRUE),
  province = sample(c("Kasai Oriental", "Lomami"), 50, replace = TRUE),
  zone = sample(c("Dipumba", "Kananga"), 50, replace = TRUE)
)

# Save to Excel
writexl::write_xlsx(test_data, "testdata/biobank_test.xlsx")
```

### Validation Script

Create `tests/validate.R`:

```r
# Test parsing functions
source("R/utils_parse.R")

# Test dates
dates <- c("01/01/2024", "44927", "2024-01-01", "NA")
result <- parse_any_date(dates)
stopifnot(sum(!is.na(result)) == 3)

# Test numbers
numbers <- c("2.5", "2,5", "10", "-5")
result <- parse_decimal_number(numbers)
stopifnot(all(result[1:3] > 0, na.rm = TRUE))

print("✓ All tests passed")
```

Run before deployment:
```r
source("tests/validate.R")
```

## Performance Optimization

### For Large Datasets (>100k rows)

1. **Use data.table instead of dplyr:**
   ```r
   library(data.table)
   setDT(df)
   ```

2. **Cache expensive operations:**
   ```r
   library(memoise)
   cached_function <- memoise(your_function)
   ```

3. **Load data incrementally:**
   ```r
   # In server function
   data <- reactiveFileReader(
     intervalMillis = 60000,  # Check every minute
     session,
     filePath = reactive(input$selected_file),
     readFunc = read_biobank_file
   )
   ```

4. **Use showPageSpinner for long operations:**
   ```r
   library(shinycssloaders)
   
   # In UI
   withSpinner(plotOutput("plot"))
   ```

## Security Considerations

### For Production Deployment

1. **Never commit sensitive credentials:**
   ```r
   # Use environment variables
   db_password <- Sys.getenv("DB_PASSWORD")
   ```

2. **Restrict file access:**
   ```r
   # Validate file paths
   if (!startsWith(normalizePath(path), normalizePath(base_dir))) {
     stop("Invalid path")
   }
   ```

3. **Sanitize user inputs:**
   ```r
   # Already done in utils_parse.R functions
   safe_input <- stringr::str_squish(input$user_text)
   ```

## Getting Help

- **App issues:** Check the Debug tab in the app
- **R errors:** Run code in R console to see full error messages
- **Deployment:** Check shinyapps.io logs or Connect logs
- **Package conflicts:** Try `renv::restore()` or rebuild renv

## Next Steps

1. **Add more modules:** Use the module pattern from `R/mod_extractions_qc.R`
2. **Add authentication:** Use `shinymanager` package
3. **Add scheduling:** Use GitHub Actions or cron jobs to refresh data
4. **Add email reports:** Use `blastula` package
5. **Add database backend:** Replace Excel files with SQL database

## Maintenance

### Regular Updates

```r
# Update packages
renv::update()

# Check for security issues
renv::snapshot()

# Test thoroughly after updates
shiny::runTests("tests/")
```

### Monitoring

Set up monitoring alerts for:
- App crashes
- Data loading failures
- Performance degradation

Use Posit Connect's built-in monitoring or set up custom alerts.
