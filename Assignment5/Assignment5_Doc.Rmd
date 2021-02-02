---
title: "Assignment 5 - Econometrics II"
author: Floris Holstege, Stanislav Avdeev
output: pdf_document
---

## 1

This approach is a difference-in-difference approach, which can be written in OLS form as: 

\begin{align*}
  y_{g1} - y_{g0} &= (a_1 - a_0) + \delta D_g + (U_{g1} - U_{g0}) \\
                  &= B_0 + \delta D_g + U_g
\end{align*}

Where $y_{g1}, y_{g0}$ is the dependent variable (grade point average) at after the first year ($t = 0$), and after the second year ($t = 1$). $B_0$ is the trend and $\delta$ is the effect of treatment $D_g$, in this case providing housing if one has a GPA higher than 8. $U_g$ are the errrors of this model. 

We know that OLS is consistent as long as $D_g$ and $U_g$ are uncorrelated. In order for this to hold, the treatment needs to be independent of the outcome variable. That is not the case here, as the outcome variable consists partially of first year grades ($y_{g0}$), which also determine whether or not someone receives the treatment. Hence, we would not expect this approach to give a consistent estimator. 


## 2 

A bivariate model of sex ratio and \% out-of-wedlock births, using only observations from the year before the war, finds no statistically significant effect from sex ratio on \% out-of-wedlock births (robust standard errors), suggesting that position of men in the marriage market does not improve with a reduction in de sex ratio. 


\begin{table}[!htbp] \centering 
  \caption{} 
  \label{} 
\begin{tabular}{@{\extracolsep{5pt}}lc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{1}{c}{\textit{Dependent variable:}} \\ 
\cline{2-2} 
\\[-1.8ex] & \% of out-of-wedlock births \\ 
\hline \\[-1.8ex] 
 Sex Ratio & $-$0.089 \\ 
  & (5.050) \\ 
  & \\ 
 Constant & 6.772 \\ 
  & (5.786) \\ 
  & \\ 
\hline \\[-1.8ex] 
Observations & 87 \\ 
Adjusted R$^{2}$ & $-$0.012 \\ 
\hline 
\hline \\[-1.8ex] 
\textit{Note:}  & \multicolumn{1}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
\end{tabular} 
\end{table} 

However, there are some obvious problems with this approach. The problem with this approach is that there might be department-specific attributes that could mean the the \% of births out of wedlock are structually higher or lower. Using OLS, while ignoring these department-specific effects results in biased estimations. 

We can improve upon this approach by using a difference-in-difference approach, with the military mortality rate as a treatment. In the French army, the vast majority of armed forces were men. A high military mortality rate should thus decrease the ratio of men to women, increasing the position of men in the market after the war, subsequently leading (if you believe the argument made in the exercise) to more out-of-wedlock births. 

If we use difference-in-difference, we avoid the previously mentioned problem, if one believes that the department-specific attributes are consistent over the two periods. 



## 3
\begin{table}[]
\begin{tabular}{rrrr}
                  & Before the war & After the war & Difference
 Above the median&  5.09 & 6.15 & 1.06    \\
 Below the median&  7.96 & 8.45 & 0.49   \\
 \vline \\
 Difference-in-Difference & & & 0.57 
\end{tabular}
\end{table}

