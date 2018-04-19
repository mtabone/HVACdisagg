# HVACdisagg

This repository contains a general algorithm for disaggregating energy consumed for 
heating and cooling loads from whole-home energy consumption measured at the utility meter. 
The general algorithm allows for different model forms as described in the accompanying publication
and explained in this overview. 

## Getting Started

The file functions/HVACdisagg.R contains two functions: classModelFit and validateFit. 
Simply source this file to access these functions. 

TrialRun.R is an example script for using the functions. PecanDataLoad.R fetches and processes pecan street 
data for testing the algorithm. This requires an adacemic license to the pecan street dataport, which is avaiable from http://www.pecanstreet.org/.
Write your username and password for the dataport into a file names pecankeys.keys in the home directory, with your username
on the first line and your password on the second. Alternatively one can enter their password directly into the PecanDataLoad.R

### Prerequisites

There are no prerequisites for the mdoel fitting functions. 
TrialRun.R requires ggplot2. PecanDataLoad.R requires RPostgreSQL. 

## Using the model
![Example of Model Forms](figures/examples.png)

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Billie Thompson** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone who's code was used
* Inspiration
* etc
