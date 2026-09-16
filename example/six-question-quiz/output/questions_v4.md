# Version 4

**Question 1 [1 mark]:** In fieldwork you often tape the circumference of a tree at breast height but need diameter (DBH) to use a height–DBH model. What is the correct conversion from measured circumference C to diameter d?

a. d = 2C / π
b. d = πC
c. d = √(C/π)
d. d = C² / π
e. d = C / π

<!-- Answer E -- The lecture refreshed circle relationships used in tree measurements: circumference C = πd, so d = C/π. -->

**Question 2 [1 mark]:** You are comparing two models that predict a vegetation variable Y from a sensor index X. Your priority is to minimise absolute prediction error in interpretable units of Y. Which metric should guide your choice?

a. R²
b. Correlation coefficient r
c. p-value of the slope
d. Root Mean Squared Error (RMSE)
e. Adjusted R²

<!-- Answer D -- The lecture contrasted R² (0–1 proportion of variance explained) with RMSE, which is in the units of Y and widely used in remote sensing to quantify absolute residual error. -->

**Question 3 [1 mark]:** You can easily measure DBH but not height in a forest survey. Which workflow best matches the lecture’s definition of a model and the purpose of prediction?

a. Use Pythagoras with GPS coordinates to compute tree height from planimetric distances.
b. Measure a few heights, take the mean, and assign it to all trees.
c. Map crowns in an equal-area projection and use crown area as height proxies.
d. Assume a fixed height:diameter ratio from a paper and multiply all DBHs by that constant.
e. Fit a statistical model of height from DBH using a training set, then use it to predict heights for the remaining trees.

<!-- Answer E -- The lecture defined a model as a statistical model of Y from X (e.g., height from DBH) used for prediction when Y is hard to measure. -->


**Question 4 [1 mark]:** Why did the lecture emphasize using simple formulae carefully before moving into software tools?

a. Quantitative interpretation still depends on understanding what measurements and formulae represent.
b. Environmental science avoids mathematical calculations once data are collected.
c. Formulae are only relevant for drawing figures, not analysis.
d. Coding removes the need to think about units or measurement.
e. Software can only run if every formula is typed from memory.

<!-- Answer A -- The lecture connects coding practice with understanding the quantitative ideas behind measurements and calculations. -->

**Question 5 [1 mark]:** In the lecture, what was the purpose of discussing RStudio setup before the labs?

a. To replace all later lectures with self-guided programming tasks.
b. To assess students before they had seen the course material.
c. To demonstrate that coding is separate from environmental science.
d. To discourage students from using local computers.
e. To make sure students could use the required computing environment for practical work.

<!-- Answer E -- The lecture frames RStudio setup as preparation for the upcoming lab work. -->

**Question 6 [1 mark]:** Which statement best describes the role of a cloud workaround such as Google Colab in the lecture?

a. It is a backup option for students who cannot run the local software environment.
b. It automatically calculates all answers without coding.
c. It is required before students can learn RStudio.
d. It is used only for watching lecture videos.
e. It is the default tool for all students regardless of their computer setup.

<!-- Answer A -- The lecture describes cloud tools as a workaround if a student cannot run RStudio locally. -->
