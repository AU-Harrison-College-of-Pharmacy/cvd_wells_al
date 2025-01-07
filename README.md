# Data analysis folder template

The goal of this repository is to serve as a template for all analyses within the Loop Group! Using this directory structure will aid in cross-collaboration between different projects, as the structure will be familiar to all.

This template also helps people to "get going" on a project because there are clear places to start cleaning data or fitting a model.

Finally, the `.gitignore` file is crucial because it helps to prevent data being sent to our private GitHub repository. If you see a file type that you think should be added, feel free to submit a pull request!

## File structure

Your data analysis should always follow the following folder structure

```
data/
  raw/  # the data in its original form goes here
  clean/  # write clean data out to here
code/
  r/  # this is where r functions you write would go
  python/  # this is where python functions you write would go
docs/
figs/
output/
```
