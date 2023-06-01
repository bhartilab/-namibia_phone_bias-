# Data and code for the figures in "Disparities in mobile phone ownership reflect inequities in access to healthcare"

# Structure

```markdown
├── data
│   ├── data_fig2.rda
│   └── data_fig3AC.rda
└── R code
    ├── d_manage.R
    └── figures.R

```

# Data

## *data_fig2*

data_fig2.rda includes the data necessary to reproduce Figure 2. Loading it will make a list called *data_fig2* in the environment.

This list includes two lists:
* *clinic_time*
* *Ndest*

Those two lists have two vectors:
* phone
* nphone

*clinic_time* includes the vectors of the travel time to healthcare from mobile phone owners (*phone*) and non-owners (*nphone*).

*Ndest* includes the vectors of the number of recent travel destinations from mobile phone owners (*phone*) and non-owners (*nphone*).

## *data_fig3AC*

data_fig3AC.rda includes the data necessary to reproduce Figure 3. Loading it will make a list called *data_fig3AC* in the environment.

It includes two lists:
* *data_fig3A*
* *data_fig3C*

Those lists includes elements necessary to reproduce Figure 3.

### *data_fig3A*

*data_fig3A* includes two lists:
* *imputedN*
* *smoothed_pm*

#### *imputedN*

*imputedN* includes the 15 distribution (counts) of imputed values for travel time to healthcare. It is available for mobile phone owners (*phone*) and non-owners (*nphone*), men (*men*) and women (*women*).

Those four sets of distributions have a similar structure: a data.frame with 16 variables.
* The first variable includes the values for *clinic_time*.
* *N1* to *N15* include the count for those values in the 15 imputed data sets.

#### *smoothed_pm*

*smoothed_pm* includes the 15 distribution (probability mass) of imputed values for travel time to healthcare. It is available for mobile phone owners (*phone*) and non-owners (*nphone*), men (*men*) and women (*women*).

Those four sets of distributions have a similar structure: a data.frame with 17 variables.
* The first variable includes the values for *clinic_time*.
* *pm1* to *pm15* include the smoothed probability masses for those values in the 15 imputed data sets.
* *pm* is the pooled smoothed probability mass. It is the one used for Figure 3A.

The data necessary to plot Figure 3A allow to generate the data for Figure 3B and 3D:
* Figure 3B plots weighted averages of the four pooled smoothed proability masses plotted in Figure 3A. The weights are avaialble in Figure 3.
* Figure 3D is the difference between the two distributions plotted in Figure 3B.

### *data_fig3C*

This list has two elements:
* *mean_clinic_time*
* *imput_boot*

#### *mean_clinic_time*

It is a data.frame with the average time to healthcare  for mobile phone owners and the general population. The code for this calculation is available in *R code*.

#### *imput_boot*

It includes two vectors:
* *phone*
* *total*

They include the distribution of the two average travel times to healthcare. They were estimated by bootstrap. The calculation method and the percentile confidence interval estimation are further detailed in *R code*.

# R code

## *d_manageR*

This script includes three functions used in the data management of the raw data.

## *figures.R*

This script includes the code of all the figures in the manuscript and the supplementary material that were generated on R.

Figure 1 was created on illustrator so no code is available.

The required data for Figure 2 is available in *data*.

The required data for Figure 3 is available in *data*.

The required data for Figure 4 is available in a Table in the supplementary material.
