# Version 3

**Question 1 [1 mark]:** In fieldwork you often tape the circumference of a tree at breast height but need diameter (DBH) to use a height–DBH model. What is the correct conversion from measured circumference C to diameter d?

a. d = √(C/π)
b. d = C² / π
c. d = 2C / π
d. d = C / π
e. d = πC

<!-- Answer D -- The lecture refreshed circle relationships used in tree measurements: circumference C = πd, so d = C/π. -->

**Question 2 [1 mark]:** You are comparing two models that predict a vegetation variable Y from a sensor index X. Your priority is to minimise absolute prediction error in interpretable units of Y. Which metric should guide your choice?

a. p-value of the slope
b. Root Mean Squared Error (RMSE)
c. R²
d. Adjusted R²
e. Correlation coefficient r

<!-- Answer B -- The lecture contrasted R² (0–1 proportion of variance explained) with RMSE, which is in the units of Y and widely used in remote sensing to quantify absolute residual error. -->

**Question 3 [1 mark]:** You can easily measure DBH but not height in a forest survey. Which workflow best matches the lecture’s definition of a model and the purpose of prediction?

a. Map crowns in an equal-area projection and use crown area as height proxies.
b. Assume a fixed height:diameter ratio from a paper and multiply all DBHs by that constant.
c. Use Pythagoras with GPS coordinates to compute tree height from planimetric distances.
d. Fit a statistical model of height from DBH using a training set, then use it to predict heights for the remaining trees.
e. Measure a few heights, take the mean, and assign it to all trees.

<!-- Answer D -- The lecture defined a model as a statistical model of Y from X (e.g., height from DBH) used for prediction when Y is hard to measure. -->


**Question 4 [1 mark]:** Why did the lecture emphasize using simple formulae carefully before moving into software tools?

a. Formulae are only relevant for drawing figures, not analysis.
b. Coding removes the need to think about units or measurement.
c. Quantitative interpretation still depends on understanding what measurements and formulae represent.
d. Software can only run if every formula is typed from memory.
e. Environmental science avoids mathematical calculations once data are collected.

<!-- Answer C -- The lecture connects coding practice with understanding the quantitative ideas behind measurements and calculations. -->

**Question 5 [1 mark]:** In the lecture, what was the purpose of discussing RStudio setup before the labs?

a. To demonstrate that coding is separate from environmental science.
b. To discourage students from using local computers.
c. To replace all later lectures with self-guided programming tasks.
d. To make sure students could use the required computing environment for practical work.
e. To assess students before they had seen the course material.

<!-- Answer D -- The lecture frames RStudio setup as preparation for the upcoming lab work. -->

**Question 6 [1 mark]:** Which statement best describes the role of a cloud workaround such as Google Colab in the lecture?

a. It is required before students can learn RStudio.
b. It is used only for watching lecture videos.
c. It is a backup option for students who cannot run the local software environment.
d. It is the default tool for all students regardless of their computer setup.
e. It automatically calculates all answers without coding.

<!-- Answer C -- The lecture describes cloud tools as a workaround if a student cannot run RStudio locally. -->
