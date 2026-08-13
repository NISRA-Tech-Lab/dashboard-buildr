# NISRA Dashboard BuildR

`dashboardBuildR` is an R package containing a Shiny application for building NISRA data dashboards.

The application provides a user interface for configuring and editing dashboard content, including homepage cards, dashboard pages, charts, supporting information and user notes.

## Installation

The package can be installed directly from GitHub.

Using `remotes`:

```r
install.packages("remotes")
remotes::install_github("NISRA-Tech-Lab/dashboard-buildr")
```

Alternatively, using `pak`:

```r
install.packages("pak")
pak::pak("NISRA-Tech-Lab/dashboard-buildr")
```

## Running Dashboard BuildR

Once installed, launch the application with:

```r
dashboardBuildR::run_dashboard_buildr()
```

Dashboard BuildR will open in your web browser.

## Using Dashboard BuildR

When the application opens:

1. Select the directory containing the dashboard you want to edit.
2. Use **Dashboard settings** to review and configure the dashboard.
3. Use **Home page design** to edit the homepage, headline cards and dashboard information.
4. Use **Page design** to configure individual dashboard pages, including cards, charts and information boxes.
5. Use **User notes** to edit the supporting user-notes content.
6. Launch the dashboard from Dashboard BuildR to review your changes.

Changes made through Dashboard BuildR are written directly to the selected dashboard project.

## Development

Clone the repository:

```bash
git clone https://github.com/NISRA-Tech-Lab/dashboard-buildr.git
```

Open the project in RStudio and install the development dependencies as required.

Load the package during development with:

```r
devtools::load_all()
```

Run the application with:

```r
run_dashboard_buildr()
```

Package documentation can be regenerated with:

```r
devtools::document()
```

Run the full package checks with:

```r
devtools::check()
```

The package should pass `R CMD check` with no errors, warnings or notes before changes are released.

## Repository structure

Key package files and directories include:

```text
dashboard-buildr/
├── DESCRIPTION
├── NAMESPACE
├── R/
│   ├── app_ui.R
│   ├── app_server.R
│   ├── run_app.R
│   └── ...
├── man/
└── README.md
```

`app_ui()` defines the Shiny user interface, `app_server()` contains the server logic, and `run_dashboard_buildr()` is the public entry point for launching the application.

## Contributing

Changes should be made through the usual Git workflow:

1. Create a branch for the change.
2. Make and test the changes locally.
3. Run `devtools::document()` if package documentation has changed.
4. Run `devtools::check()`.
5. Commit and push the changes.
6. Open a pull request for review.

## Licence

See the repository licence for terms of use.