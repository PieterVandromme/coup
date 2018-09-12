# COUP Data Analyst Coding Challenge

Please find here the results of the coding challenge:

[R Markdown assignment](assignment.Rmd)

[HTML output](assignment.html)

Coding Challenge:

## Data

Please take a look at the following datasets

### [Users](users.csv)

Data of new users starting COUP signup process since July 1st in one of our markets

* `user_id`: Unique user identifier
* `signup_started`: Time (Berlin timezone) when user started COUP app signup process
* `signup_completed`: Time (Berlin timezone) when user completed COUP app signup process
* `acquisition_channel`: Acquisition channel of the user
  

### [Rentals](rentals.csv)

COUP Scooter rental data since July 1st

* `rental_id`: Unique rental identifier
* `rental_created_at`: Time (UTC) when new rental transaction was created
* `rental_ended_at`: Time (UTC) when rental transaction was ended
* `user_id`: Unique user identifier (user creating the rental)
* `rental_end_event`: Rental can have four different end events:
	* `Cancel`: Rental is cancelled by user
	* `Expire`: Rental is expired. This happens when a user does not unlock the scooter within a certain time period from the time the rental was created
	* `Remote_cancel`: Rental is cancelled by COUP customer service
	* `Close`: Rental is ended by the user and is not cancelled nor expired. Only rentals with `end_event` `close` are considered `rides`. 


### [Temperatures](temperatures.csv)

Hourly temperature data of the given market

* `observation_time`: Time of the observed temperature
* `temp_c`: Observed temperature (Celcius) at the given time


## Task 1: Cohort Analysis

Calculate weekly retention rates based on the rides the new [users](users.csv) have been making. 
Cohorts for this analysis should be the weeks when user started their signup process.

## Task 2: Durations

* Analyze the durations of the signup process (`signup_started` > `signup_completed`). Can you identify some issues/problem cases based on these durations?
* Given the [Rentals](rentals.csv) data can you derive the expiry time (seconds) of a rental?

## Task 3: Temperature and Rides

* Analyze the effect of temperature on rides. Based on the given data can you identify other factors that could be affecting the rides?

You can use any tools you find suitable for this task, however, your solution should be reproducible -
please provide short instruction how it can be reproduced. If the solution is scripted, please provide
source code of the scripts.
