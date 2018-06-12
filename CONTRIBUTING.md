## Suggestions/bug reports/feature requests
Please file an [issue](https://github.com/PublicHealthEngland/hcaidcs/issues)

## Contributing your own functions
Please use git to make any changes to the code. 
To do this, please do the following:

 1. Download a copy of the repository
 1. Create a new branch, ideally using a name that describes the change you intend to make. 
 1. Create a merge request, providing the details of the changes you have made and the purpose of the change. 
 1. The project owner will review and accept as s/he sees fit. This is the owners prerogative. There may be requests for further explanations or changes.

### Favoured packages

Please use [testthat](https://github.com/r-lib/testthat) for testing and [assertthat](https://cran.r-project.org/web/packages/assertthat/index.html) for assertions.

Each function should have a corresponding set of tests and should probably include assertions to ensure that error messages are meaningful and erroneous input is correctly handled. 
Coverage of unit tests should be as complete as possible.

## Code style

camel_case not snakeCase.

Underscore separators, not dots. 

Anything else, please refer to the [tidyverse styleguide](http://style.tidyverse.org/)
