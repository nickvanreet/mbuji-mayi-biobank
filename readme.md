# Mbuji-Mayi Biobank Dashboard

A modern, production-ready Shiny dashboard for managing and analyzing biobank data from the CRT Dipumba project in Mbuji-Mayi, Democratic Republic of Congo.

## Features

### Core Functionality
- 📊 **Sample Management**: Track and filter biobank samples with advanced queries
- 🗺️ **Geographic Visualization**: Interactive maps showing health zone coverage
- 📈 **Quality Control**: DRS extraction volume monitoring and outlier detection
- 🔬 **Lab Results Integration**: PCR, ELISA (PE/VSG), and iELISA results tracking
- 🚚 **Transport Monitoring**: Multi-segment sample transport time analysis
- 📋 **Data Export**: Download filtered data and summaries as CSV

### Technical Features
- ⚡ Modular, maintainable code structure
- 🔧 YAML-based configuration
- 📦 Reproducible environment with renv
- 🎨 Modern UI with Bootstrap 5
- 🔒 Robust error handling and validation
- 📱 Responsive design for mobile and desktop

## Quick Start

### Prerequisites

- R 4.0.0 or higher
- RStudio (recommended)
- System dependencies for spatial packages (see SETUP.md)

### Installation

1. **Clone or download this project**

2. **Run the setup script:**
   ```r
   source("setup.R")
   ```

3. **Configure your data paths** in `config.yml`:
   ```yaml
   paths:
     biobank_dir: "YOUR_PATH_HERE/01 - Biobanque"
     extractions_dir: "YOUR_PATH_HERE/02 - Extractions"
     # ... etc
   ```

4. **Run the app:**
   ```r
   shiny::runApp()
   ```

See [SETUP.md](SETUP.md) for detailed installation instructions.

## Project Structure

```
mbuji-mayi-biobank/
├── app.R                 # Main application
├── config.yml            # Configuration
├── R/
│   ├── utils_parse.R     # Data parsing utilities
│   ├── utils_join.R      # Join utilities
│   └── mod_*.R           # Feature modules (optional)
├── testdata/             # Test datasets
├── tests/                # Validation tests
├── logs/                 # Application logs
└── renv/                 # Package management
```

## Usage

### Loading Data

1. Click "Load Data" in the sidebar
2. Select your biobank Excel file
3. The app will automatically:
   - Parse dates, numbers, and codes
   - Standardize column names
   - Validate data quality
   - Update filter options

### Filtering Data

Use the sidebar filters to narrow down your analysis:
- **Date Range**: Select sample collection period
- **Study**: Filter by DA (Active) or DP (Passive) studies
- **Province/Zone**: Filter by geographic location
- **Sex**: Filter by patient sex (M/F)

### Viewing Results

Navigate through the tabs:
- **Overview**: Key metrics and summary visualizations
- **Transport**: Sample transport time analysis
- **Demographics**: Age-sex pyramids by health zone
- **Geography**: Interactive health zone map
- **Extraction QC**: DRS volume quality control
- **Lab Results**: PCR and serological test results
- **Data**: Filterable table with export functionality

### Exporting Data

1. Navigate to the Data tab
2. Apply your desired filters
3. Click "Download CSV" to export

## Configuration

### config.yml Structure

```yaml
paths:
  # Specify full paths to your data directories
  biobank_dir: "..."
  extractions_dir: "..."
  pcr_dir: "..."
  elisa_pe_dir: "..."
  elisa_vsg_dir: "..."
  ielisa_dir: "..."

map:
  # Map data source configuration
  use_grid3_online: true
  grid3_url: "..."
  province_field_regex: "(?i)prov"
  zone_field_regex: "(?i)zone"

qc:
  # Quality control thresholds
  drs_target_ml: 2.0
  drs_accept_min_ml: 1.5
  drs_accept_max_ml: 2.5
  max_transport_field_hs_days: 30
  max_transport_hs_lsd_days: 30
  max_transport_lsd_inrb_days: 90

ui:
  # UI customization
  theme_primary: "#2C3E50"
  default_date_range_days: 180
```

### Customizing Thresholds

Edit `config.yml` to adjust:
- Acceptable DRS volume ranges
- Maximum transport time limits
- Default date ranges
- Theme colors

## Data Format Requirements

### Biobank File

Expected columns (flexible matching):
- `barcode` / `code_barres`
- `lab_id` / `numéro`
- `date_sample` / `date_prélèvement`
- `age`, `sex`/`sexe`
- `province`, `zone`
- `study`/`étude` (DA/DP)
- `structure`, `unit`

### Extraction Files

- Multiple Excel files in the extractions directory
- Standard columns from DRS extraction forms
- Automatically detected by filename pattern

### Lab Results Files

- PCR: Workbooks with "Replicates", target sheets
- ELISA: Result sheets with PP%, ΔOD
- iELISA: Result sheets with % inhibition

See documentation in the code for detailed column mappings.

## Troubleshooting

### Common Issues

**"No data loaded"**
- Check that your file path is correct in config.yml
- Verify the Excel file is not corrupted
- Check the Debug tab for column detection issues

**"No samples after filtering"**
- Expand your date range
- Check that study/province/zone filters match your data
- Review the Debug tab for data parsing issues

**Map won't display**
- Check internet connection (for online GRID3 data)
- Try uploading a local shapefile instead
- Verify the shapefile has province and zone columns

**Extraction QC shows no data**
- Click "Load extractions" button
- Verify the extraction directory path
- Check that Excel files exist in the directory

See [SETUP.md](SETUP.md) for more troubleshooting tips.

## Development

### Adding New Features

1. Create a new module file in `R/mod_your_feature.R`
2. Follow the module pattern:
   ```r
   mod_your_feature_ui <- function(id) { ... }
   mod_your_feature_server <- function(id, data) { ... }
   ```
3. Source the module in `app.R`
4. Add UI and server calls

### Testing

Run validation tests:
```r
source("tests/validate.R")
```

Create test data:
```r
source("tests/create_test_data.R")
```

### Code Style

- Use tidyverse conventions
- Document functions with roxygen2-style comments
- Keep functions focused and modular
- Handle errors gracefully with tryCatch()

## Deployment

### Deploy to shinyapps.io

```r
library(rsconnect)

# First time setup
rsconnect::setAccountInfo(
  name = "YOUR_ACCOUNT",
  token = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)

# Deploy
rsconnect::deployApp(
  appName = "mbuji-mayi-biobank",
  forceUpdate = TRUE
)
```

### Deploy to Posit Connect

```r
rsconnect::deployApp(
  server = "your-connect-server",
  appName = "mbuji-mayi-biobank"
)
```

See [SETUP.md](SETUP.md) for detailed deployment instructions.

## Performance

### Optimization Tips

For large datasets (>100k samples):
- Enable server-side DataTables: `server = TRUE`
- Use data.table for heavy operations
- Cache expensive computations with memoise
- Consider database backend instead of Excel files

### Monitoring

Track app performance:
- Response times
- Memory usage
- Error rates

Use Posit Connect's built-in monitoring or custom logging.

## Contributing

We welcome contributions! To contribute:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new features
5. Submit a pull request

Please follow the existing code style and add documentation.

## License

This project is licensed under [LICENSE] - see LICENSE file for details.

## Acknowledgments

- **Institute of Tropical Medicine, Antwerp**: Project oversight
- **CRT Dipumba**: Data collection and field operations
- **GRID3**: Health zone boundary data
- **R Shiny**: Framework and community

## Support

For issues and questions:
- Check the [SETUP.md](SETUP.md) documentation
- Review the Debug tab in the app
- Open an issue on GitHub
- Contact: [your contact information]

## Roadmap

### Planned Features
- [ ] User authentication with shinymanager
- [ ] Database backend (PostgreSQL/MySQL)
- [ ] Automated email reports
- [ ] Real-time data synchronization
- [ ] Advanced statistical analyses
- [ ] Longitudinal patient tracking
- [ ] Mobile data collection integration

### Future Enhancements
- Multi-language support (French/English)
- Advanced filtering and search
- Data quality dashboards
- Predictive analytics
- API for external tools

## Version History

### Version 2.0.0 (Current)
- Complete refactor with modular structure
- Improved parsing and validation
- YAML-based configuration
- Enhanced error handling
- Production-ready deployment

### Version 1.0.0
- Initial release
- Basic biobank management
- Manual configuration
- Single-file structure

## Citation

If you use this software in your research, please cite:

```
[Author names]. (2025). Mbuji-Mayi Biobank Dashboard. 
Institute of Tropical Medicine, Antwerp. 
Version 2.0.0. https://github.com/[your-repo]
```

---

**Last Updated**: November 2, 2025  
**Maintained By**: [Your Name/Team]  
**Contact**: [Your Contact Information]
