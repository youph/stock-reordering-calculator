# Stock reordering calculator

The calculator uses user's data on sales history of stock items, as well as the current stock levels (Stock On Hand, SOH) 
to forecast the future sales and estimate the times-to-last for each stock item. If any of the estimated times-to-last are below 
the user-specified minimum required number of days, a stock reorder is calculated, subject to a constraint that the minimum order size
is equal to, or exceeds the user-specified minimum order size (e.g., for the order to attract a bulk discount).

## How to launch

* The easiest way is to use the cloud-deployed version of this calculator at https://ytyshetskiy.shinyapps.io/stock_reordering_calculator/
* To run the calculator on your machine, open the ```/shiny_app/ui.R``` in RStudio (required), and click the "Run App" button.

## How to use

Just upload a .csv data file describing sales history and current SOH levels of your stock (example data file is in the ```/data``` directory, your data files should adhere to the same format), and hit the "Calculate re-order" button. The new order will be displayed in the "Stock re-order" tab. Adjust the numbers in the control panel on the left, if necessary, and re-calculate the order.
