# Fire Data

This is the main repository for all apps developed to be used together to track and report on administrative data in volunteer fire departments. Check out the presentation for this project at <a href = "https://reg.conf.posit.co/flow/posit/positconf24/publiccatalog/page/publiccatalog/session/1711986858836001vnVj" target="_blank">PositConf 2024</a>.

This was developed originally in two different repositories. The history of both repositories have been included in this one. If you want to see the original repositories, they are linked <a href = "https://github.com/JosephRichey/cfd_data_portal" target="_blank">here</a> and <a href = "https://github.com/JosephRichey/cfd_training_attendance" target="_blank">here</a>.

_Currently, the app is under active development. There are issues with each of the example apps, especially with recording times._

## Usage

This code is licensed under CC BY-NC-SA. The general idea of this project is it can be setup to run with minimal modification. The general steps are as follows:

1.  Clone the repository.

2.  Establish an AWS mySQL server and get the necessary credentials.

3.  Run the table setup scrips.

4.  Store database credentials, table names, and other specific values in an .Renviron file. Use the Renviron template file to start (note, you have to have one file for each app and it must be in the top level of each project).

5.  Publish to shinyapps.io.

## Apps

### Data Portal

This is the admin level app that allows for reporting, training and personal management, and soon managing and editing calls. Currently, the functionality is minimal. More coming soon.

Upcoming changes include:

-   Ability to modify check-in/check-out times

-   Incident Analysis

-   Full functionality to edit trainings, calls, etc.

See a fully functioning example app <a href = "https://fire-data.shinyapps.io/data-portal/" target="_blank">here</a>.

### Training Attendance

This is the most tried and tested app currently. This allows people to check in and out of trainings for training hours. The admin password to sign everyone out is "123".

Upcoming features include:

-   Color distinct modals that emphasize if you are checking in or out.

-   General UI improvements.

See a fully functioning example app <a href = "https://fire-data.shinyapps.io/training-attendance/" target="_blank">here</a>.

### Call Attendance

The first version of this was released in v0.3.0. This allows departments to track who attended calls, what units were used, amount of time spent on the call, etc. This will eventually integrate with a backend server that can be set up to email reports with data and visuals as needed.

See a fully functioning example app <a href = "https://fire-data.shinyapps.io/incident-response/" target="_blank">here</a>.

### Equipment Management

This app will be released with v1.0.0. This allows for checks to be made on a regular cadence. Equipment can be added and managed in the data portal, and then checked in this application. (Current prototype doesn't support this- use equipment that is already loaded.)

See a fully function example app <a href = "https://fire-data.shinyapps.io/equipment_management/" target="_blank">here</a>

<a href="https://www.buymeacoffee.com/josephrichey" target="_blank"><img src="https://cdn.buymeacoffee.com/buttons/default-blue.png" alt="Buy Me A Coffee" height="41" width="174"></a>
