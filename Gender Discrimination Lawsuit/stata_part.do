import delimited "C:\Users\Yogya Hridey Sareen\Downloads\BC2406 Analytics 1 Course Materials for Students\BC2406 Analytics 1 Course Materials for Students\Special Session 1 Gender Discrimination Lawsuit\Lawsuit.csv", clear 

*Creating table to show how Gender plays an important role in determining the rank of the person

eststo A: regress rank gender cert exper

eststo B: regress rank gender cert exper prate

eststo C: regress rank gender cert exper prate prate

esttab A B C, se r2
