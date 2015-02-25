all: scenarios.csv

scenarios.csv: scenarios.xlsx saveAllTabsAsCSV.vbs
	cscript saveAllTabsAsCSV.vbs scenarios.xlsx
	rm scenarios_readme.csv
	rm scenarios_descriptions.csv
	mv scenarios_scenarios.csv scenarios.csv