# Missing Data Imputation — Comparative Analysis in R

> A reproducible pipeline comparing multiple imputation methods (mice, missForest, and Amelia) using the nhanes dataset, evaluating their impact on statistical relationships such as the correlation between BMI and Age.

---

## 🎯 Objective

This repository demonstrates a practical approach to handling missing data using real-world imputation packages in R. The goal is to:

✔ Explore and visualize missingness patterns

✔ Apply multiple imputation methods and generate complete datasets

✔ Analyze the effect of imputation on variable correlation

✔ Compare results across imputation strategies

✔ Automate and organize analysis into modular scripts

---

## 🧰 Technologies

- **Language**: R  
- **Core Packages**:  
  - `mice` – Multivariate Imputation by Chained Equations
  - `missForest` – Random Forest-based Imputation  
  - `Amelia` – EM Bootstrap-based Imputation
  - `VIM` – Missing data visualization  
  - `dplyr, ggplot2, tidyr` – Data manipulation and plotting
    
---

## 📊 Analysis Workflow

1. **Data Loading**
   - Load the nhanes dataset from the mice package
   - Save as .csv for reproducibility

2. **Missing Data Exploration**
   - Use VIM::aggr() to visualize missingness patterns
   - Compute summary statistics for affected variables

3. **Imputation Methods**
   - mice: Predictive Mean Matching (PMM), 5 imputations
   - missForest: Nonparametric Random Forest-based imputation
   - Amelia: EM-based single imputation (m = 1)

4. **Post-Imputation Analysis**
   - Calculate correlation between bmi and age for each imputed dataset
   - Compare to correlation in original (incomplete) data
     
---

## 📝 Example Output

### Missing Data Pattern and Imputation Comparison

![Posterior Plot](analise_missing_mice_mf_amelia.png)

> The left panel shows the proportion of missing data per variable in the dataset, while the right panel displays the missingness patterns. Variables like chl, bmi, and hyp present substantial missingness, while age is fully observed.

---

# Impact of Imputation on Correlation (BMI vs Age)

> After applying three different imputation methods, we evaluate how each method affects the correlation between bmi and age:

| Method       | Correlation |
|--------------|-------------|
| Original     | -0.3719     |
| mice         | -0.3999     |
| missForest   | -0.4951     |
| Amelia       | -0.4919     |

Note: Imputation methods slightly strengthened the correlation between bmi and age, especially with missForest and Amelia. This reflects how different algorithms infer missing values based on variable relationships.

---

## 🔄 Possible Extensions

- 📉 Add more evaluation metrics (e.g., RMSE, distributional comparison)
- 🧠 Visualize variable distributions pre vs. post-imputation
- 📊 Include dimensionality reduction (e.g., PCA) after imputation
- 📁 Export results in a report using rmarkdown for reproducibility
- 🌐 Integrate into a Shiny App for interactive exploration of imputed datasets
---

## 📚 Use Case

This project is ideal for:
- Data analysts and scientists learning imputation techniques
- Students exploring statistical implications of missing data 
- Researchers validating preprocessing pipelines  
- Portfolio demonstration of R data cleaning and pipeline automation

---

> 📎 _This project is for educational and demonstration purposes only. It simulates hypothetical data and is not based on real clinical trial results._
