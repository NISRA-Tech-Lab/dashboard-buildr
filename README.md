# NISRA Dashboard BuildR

> ### 💀 _Part of the [NISRA Dashboard Skeleton](https://datavis.nisra.gov.uk/techlab/drpvze/dashboard-skeleton.html)_

`dashboardBuildR` is an R package containing a Shiny application for building and editing NISRA data dashboards based on the NISRA dashboard template.

The application provides a user interface for configuring dashboard content, including:

- dashboard settings and pages
- homepage cards
- page headline cards
- charts
- supporting information and info boxes
- user notes

Changes made through Dashboard BuildR are written directly to the dashboard project being edited.

## Installation

Dashboard BuildR can be installed directly from GitHub.

Using `remotes`:

```r
install.packages("remotes")

remotes::install_github(
  "NISRA-Tech-Lab/dashboard-buildr"
)
```

Alternatively, using `pak`:

```r
install.packages("pak")

pak::pak(
  "NISRA-Tech-Lab/dashboard-buildr"
)
```

## Running Dashboard BuildR

Once installed, launch the application with:

```r
dashboardBuildR::run_dashboard_buildr()
```

Dashboard BuildR will open in your web browser.

## Opening a dashboard project

### Automatic project detection

If Dashboard BuildR is launched from an R project based on the NISRA dashboard template, the dashboard directory will be detected automatically.

For example, open the dashboard's `.Rproj` file in RStudio and run:

```r
dashboardBuildR::run_dashboard_buildr()
```

Dashboard BuildR recognises a dashboard project from its project structure, including the presence of the dashboard's `index.html` and configuration files.

The active R project directory will then be used automatically as the dashboard directory.

### Selecting a dashboard manually

If the current R project is not recognised as a dashboard project, Dashboard BuildR can still be used in the usual way.

Click **Browse** and select the directory containing the dashboard you want to edit.

A manually selected directory replaces any automatically detected dashboard directory.

## Using Dashboard BuildR

Once a dashboard has been loaded:

1. Use **Dashboard settings** to review and configure the dashboard, including its pages and Data Portal tables.
2. Use **Home page design** to edit the homepage strapline, headline cards and dashboard information.
3. Use **Page design** to configure individual dashboard pages, including headline cards, charts and information boxes.
4. Use **User notes** to edit supporting information for dashboard users.
5. Use **Launch dashboard** to open the dashboard and review your changes.

Changes are written directly to the selected dashboard project.

It is recommended that dashboard projects are managed using Git so that changes made through Dashboard BuildR can be reviewed and reverted where necessary.

## Dashboard template

Dashboard BuildR is designed to work with dashboards based on the NISRA dashboard template:

`NISRA-Tech-Lab/dashboard-template`

The dashboard project contains the HTML, JavaScript, configuration and other files that make up the published dashboard. Dashboard BuildR provides an interface for editing those files without requiring users to make all changes manually in the source code.

## Development

Clone the Dashboard BuildR repository:

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
