EA Validation Tool
About ACAI has developed a dashboard that will be used to curate project activity data and provide real-time summary of results from field activities as entered by extension agents. The Akilimo Dashboard is a suite of code in R language combined with the Open Data Kit (ODK) data collection tools to form a data processing engine that delivers reports in easy-to-read pdf format, CSV files, and an interactive web interface. The dashboard helps ACAI management in monitoring the activities around the validation of the ACAI decision support tools (DSTs). Coordinators of the activities can check the status and get real-time information on planned and ongoing activities in their respective regions at the click of a button. From this information, the project team and interested parties can view the number and details of the households who have received ACAI recommendation for follow-up and further observations. Extension Agents (EA) working with ACAI on validation activities are compensated through a reward system, based on the amount of work done and data delivered. The EA earns points against which the remuneration is made on a monthly basis. The dashboard will provide a summary of all activities conducted, in order to facilitate payment for the EAs.
The steps to follow:
1. Source updated data from Ona. This can be found here: /nfs/extra_storage/ona_data. Download this data to the following folder: /home/akilimo/projects/AKILIMO_Dashboards/EAtool/Input/data
2. Run the “functions_getData.R” (you will need to change the paths in the script).
In the script you will:
i. Source("clean_IDs.R") in order to conduct some preliminary data cleaning exercise based on country data and specifications sometimes provided by country coordinators.
ii. Source ("functions. R") to apply the functions to the script
iii. Read all data files that are saved in /home/akilimo/projects/AKILIMO_Dashboards/EAtool/Input/data
iv. Modify this data to get resized outputs (.RDS format) which are saved here:
/home/akilimo/projects/AKILIMO_Dashboards/EAtool/Input/sourceData
3. Grant access to the dashboard by adding the usernames and passwords on the “EA_MonitoringAccess.csv” which is found here: /home/akilimo/projects/AKILIMO_Dashboards/EAtool/data
4. On the “server. R” on line 41, update the “userslist” to include all the users that require access to the dashboard.
5. Run the getEAandHH_shiny.R script which will then read the RDS files you had saved here: /home/akilimo/projects/AKILIMO_Dashboards/EAtool/Input/sourceData
as well as the EA_MonitoringAccess.csv. Additionally, two csvs from CAVAII and OYSCGA EAs will also be read.
6. Run the server.R file to be able to see how the dashboard looks like.
The dashboard comprises of:
1. The validation tab comprising of
a) the ‘Event plot’: This shows the time of execution of events per household per EA. Events include: planting, gapping, fertilizer application, thinning, reseeding, replanting, weeding and harvesting. Events have actual dates, predicted dates, and are color coded. The color codes indicate the timing of the events in reference to planned dates in the trial protocol.
b) The ‘Event calendar’ which maps events based on the actual week, month and year when it was executed. The calendar can be filtered out by season and offers a timely alert on events that are due soon, based on predicted dates as per the trial protocol.
2. The EA points tab which provides a tabular view of total EA accumulated points based on submission dates of events conducted in the field. Additionally, it provides a monthly summary of points for easier point calculations during EA compensation.
3. The Recommendations tab: This provides site specific recommendations given for every HH served by the selected EA.
