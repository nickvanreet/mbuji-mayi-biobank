# setup.R
# Automated setup script for Mbuji-Mayi Biobank Dashboard
# Run this once to set up your development environment
# =============================================================================

cat("=== Mbuji-Mayi Biobank Dashboard Setup ===\n\n")

# 1. Check R version
cat("Checking R version...\n")
if (getRversion() < "4.0.0") {
  stop("R version 4.0.0 or higher is required. Current version: ", getRversion())
}
cat("✓ R version:", as.character(getRversion()), "\n\n")

# 2. Create directory structure
cat("Creating directory structure...\n")
dirs <- c("R", "testdata", "tests", "data", "logs")
for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("  Created:", dir, "\n")
  } else {
    cat("  Exists:", dir, "\n")
  }
}
cat("\n")

# 3. Install required packages
cat("Installing required packages...\n")
packages <- c(
  "shiny", "bslib", "tidyverse", "readxl", "janitor", 
  "lubridate", "DT", "sf", "leaflet", "stringi", "bsicons",
  "yaml", "scales", "purrr", "memoise"
)

missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("Installing:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, dependencies = TRUE)
} else {
  cat("✓ All required packages already installed\n")
}
cat("\n")

# 4. Install development packages
cat("Installing development packages...\n")
dev_packages <- c("renv", "rsconnect", "testthat", "shinycssloaders")
missing_dev <- dev_packages[!sapply(dev_packages, requireNamespace, quietly = TRUE)]

if (length(missing_dev) > 0) {
  cat("Installing:", paste(missing_dev, collapse = ", "), "\n")
  install.packages(missing_dev, dependencies = TRUE)
} else {
  cat("✓ All development packages already installed\n")
}
cat("\n")

# 5. Initialize renv (optional)
cat("Would you like to initialize renv for package management? (y/n): ")
response <- tolower(trimws(readLines("stdin", n = 1)))

if (response == "y") {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv")
  }
  renv::init()
  cat("✓ renv initialized\n")
} else {
  cat("Skipping renv initialization\n")
}
cat("\n")

# 6. Create config.yml if it doesn't exist
cat("Checking config.yml...\n")
if (!file.exists("config.yml")) {
  cat("Creating default config.yml...\n")
  
  default_config <- '# Mbuji-Mayi Biobank Dashboard Configuration

paths:
  biobank_dir: "YOUR_PATH_HERE/01 - Biobanque"
  extractions_dir: "YOUR_PATH_HERE/02 - Extractions"
  pcr_dir: "YOUR_PATH_HERE/03 - Biologie Moléculaire/0302 - Résultats qPCR"
  elisa_pe_dir: "YOUR_PATH_HERE/04 - ELISA indirect PE/0402 - Résultats ELISA indirect PE"
  elisa_vsg_dir: "YOUR_PATH_HERE/05 - ELISA indirect VSG/0502 - Résultats ELISA indirect VSG"
  ielisa_dir: "YOUR_PATH_HERE/06 - iELISA/0602 - Résultats iELISA"

map:
  use_grid3_online: true
  grid3_url: "https://services3.arcgis.com/BU6Aadhn6tbBEdyk/arcgis/rest/services/GRID3_COD_health_zones_v7_0/FeatureServer/0/query?where=1%3D1&outFields=province,zonesante&outSR=4326&f=geojson"
  province_field_regex: "(?i)prov"
  zone_field_regex: "(?i)zone|zs|zonesante"

qc:
  drs_target_ml: 2.0
  drs_accept_min_ml: 1.5
  drs_accept_max_ml: 2.5
  max_transport_field_hs_days: 30
  max_transport_hs_lsd_days: 30
  max_transport_lsd_inrb_days: 90

ui:
  theme_primary: "#2C3E50"
  theme_success: "#27AE60"
  theme_info: "#3498DB"
  theme_warning: "#F39C12"
  theme_danger: "#E74C3C"
  default_date_range_days: 180
'
  
  writeLines(default_config, "config.yml")
  cat("✓ Created config.yml - PLEASE UPDATE THE PATHS!\n")
} else {
  cat("✓ config.yml already exists\n")
}
cat("\n")

# 7. Create .gitignore if it doesn't exist
cat("Checking .gitignore...\n")
if (!file.exists(".gitignore")) {
  cat("Creating .gitignore...\n")
  
  gitignore_content <- '# R files
.Rproj.user
.Rhistory
.RData
.Ruserdata
*.Rproj

# Data files (don\'t commit sensitive data)
data/*.xlsx
data/*.csv
testdata/*.xlsx
testdata/*.csv

# Logs
logs/*.log

# renv
renv/library/
renv/local/
renv/cellar/
renv/lock/
renv/python/
renv/staging/

# Deployment
rsconnect/

# OS files
.DS_Store
Thumbs.db

# Temporary files
*.tmp
*~
'
  
  writeLines(gitignore_content, ".gitignore")
  cat("✓ Created .gitignore\n")
} else {
  cat("✓ .gitignore already exists\n")
}
cat("\n")

# 8. Create test data
cat("Creating test data...\n")
if (!file.exists("testdata/biobank_test.xlsx")) {
  if (requireNamespace("writexl", quietly = TRUE)) {
    test_data <- data.frame(
      barcode = paste0("KPS", sprintf("%04d", 1:50)),
      lab_id = 1:50,
      date_sample = seq.Date(Sys.Date() - 90, Sys.Date(), length.out = 50),
      age = sample(1:80, 50, replace = TRUE),
      sex = sample(c("M", "F"), 50, replace = TRUE),
      study = sample(c("DA", "DP"), 50, replace = TRUE),
      province = sample(c("Kasai Oriental", "Lomami"), 50, replace = TRUE),
      zone = sample(c("Dipumba", "Kananga", "Tshilenge"), 50, replace = TRUE),
      structure = sample(c("HGR Dipumba", "HGR Kananga", "CS Tshilenge"), 50, replace = TRUE)
    )
    
    writexl::write_xlsx(test_data, "testdata/biobank_test.xlsx")
    cat("✓ Created test data: testdata/biobank_test.xlsx\n")
  } else {
    cat("⚠ writexl package not available, skipping test data creation\n")
  }
} else {
  cat("✓ Test data already exists\n")
}
cat("\n")

# 9. Verify utility files
cat("Checking utility files...\n")
required_files <- c(
  "R/utils_parse.R",
  "R/utils_join.R"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  cat("⚠ Missing files:\n")
  for (f in missing_files) {
    cat("  -", f, "\n")
  }
  cat("Please create these files from the provided code.\n")
} else {
  cat("✓ All utility files present\n")
}
cat("\n")

# 10. Final checks
cat("Running final checks...\n")

# Check if app.R exists
if (!file.exists("app.R")) {
  cat("⚠ app.R not found! Please create it from the provided code.\n")
} else {
  cat("✓ app.R exists\n")
}

# Try to load required packages
cat("Verifying package installation...\n")
failed_packages <- character(0)
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    failed_packages <- c(failed_packages, pkg)
  }
}

if (length(failed_packages) > 0) {
  cat("⚠ Failed to load packages:", paste(failed_packages, collapse = ", "), "\n")
} else {
  cat("✓ All packages can be loaded\n")
}
cat("\n")

# 11. Summary
cat("=== Setup Summary ===\n\n")

cat("✓ Directory structure created\n")
cat("✓ Packages installed\n")
cat("✓ Configuration files created\n\n")

cat("NEXT STEPS:\n\n")
cat("1. Edit config.yml to set your data paths\n")
cat("2. Ensure you have created:\n")
cat("   - app.R (main application)\n")
cat("   - R/utils_parse.R (parsing utilities)\n")
cat("   - R/utils_join.R (join utilities)\n\n")
cat("3. Test the app locally:\n")
cat("   shiny::runApp()\n\n")
cat("4. If everything works, consider:\n")
cat("   - Setting up version control (git init)\n")
cat("   - Creating a snapshot (renv::snapshot())\n")
cat("   - Deploying to shinyapps.io or Posit Connect\n\n")

cat("For detailed instructions, see SETUP.md\n\n")

cat("=== Setup Complete ===\n")
