# Team Mangalyaan: Analyzing the biases in Machine Learning: How fair can we be?

  ## Goal
  To develop a postprocessing pipeline to equalize PPV, NPV and Accuracy Profiles of protected groups. 
  
  ## Getting Started
  These instructions describe the prerequisites and steps to get the project up and running.

  ### Setup
 
 To setup a Virtual Environment with all the prerequisite packages used in the project, do the following:
  1. Install [Conda](https://docs.conda.io/projects/conda/en/latest/user-guide/install/) using the `conda_install.sh` file in the 'scripts' directory using the command: `$ bash scripts/conda_install.sh`
  2. Create a conda environment from the included `environment.yml` file using the following command:
     
     `$ conda env create -f environment.yml`
  3. Activate the environment
     
     `$ conda activate mangalyaan`

  ### Usage
  To run the code, the user can navigate to the folder containing the file 'team-mangalyaan.py', and run it using the command: `$ python team-mangalyaan.py --options`. The user can get a description of the options by using the command: `$ python team-mangalyaan.py -help`.
  
   
  ### Output
  Upon running the command in the ‘Usage’ section, barplots will be created that illustrate the effects of the three postprocessing techniques used here to deal with machine bias on the three different datasets based on gender and race.

 ### Ethical Implications
 These models and techniques are designed to address the issue of machine bias and how it could have grave implications in different settings. 

## Contributors
* See [Contributors](CONTRIBUTORS.md) file for more details.

## License
This project is licensed under the **MIT License**. See [LICENSE](LICENSE) for more details.
