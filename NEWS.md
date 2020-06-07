# dmtools 0.2.5

* add functions for MedDRA API
* `short()` don't stop if an error happens

# dmtools 0.2.4

* add `short()`
* add `get_result()`
* add parameter `name_to_find` for `lab()`, `wbc()`
* delete parameter `test` and add paramater `func` for `test_sites()`
* add example for `rename_dataset()`

# dmtools 0.2.3

* add `test_sites` for choose test in the different sites
* add `get_date` for the date object

## Bug fixes

* wbc checks non-numeric arguments

# dmtools 0.2.2

* add opportunity for check sites
* delete depended and filtered columns

## Bug fixes

* parameter for clinical significant estimate

# dmtools 0.2.1

* add <- -> for dates, which is out
* add function for calculation WBCs count
* OOP structure

# dmtools 0.1.4

* throw error if the final result result is empty
* throw warning if don`t find item in file

## Bug fixes

* parameter for sex: substring in regex e.g. "male" in "female"

# dmtools 0.1.3

* add a parameter for the clinical significant estimate

# dmtools 0.1.2

* add a function for dates validation

# dmtools 0.1.1

* first public release of the package
