# GrowCast: GH Effect Predictor for Idiopathic Short Stature Patients

GrowCast is a web-based R-Shiny application designed to predict the growth hormone (GH) treatment effect in patients with idiopathic short stature (ISS).  
By entering basic patient information and GH treatment parameters, the app provides visual predictions of future height and percentile scores.  
The model incorporates a first-order Taylor approximation approach to generate accurate and interpretable forecasts.

---

## Features

- Web-based (R-Shiny)
- Predicts future height and percentile scores based on patient inputs
- Incorporates statistical variability with prediction intervals
- Visual and interactive output using `plotly`
- Instructional image guide included in the interface

---

## Required R Packages

- `shiny`
- `ggplot2`
- `plotly`
- `dplyr`
- `MASS`

---

## Instruction

### 📁 Upload file tab

- `www/control_0418.csv`: Reference dataset used to calculate percentile scores  
- `www/instruction2.png`: Instructional image shown in the first tab

---

### 🧾 Input Panel

The following user inputs are required from the left sidebar:

- **Age at First Treatment** (months)
- **Daily GH Dose**
- **Weight** (kg)
- **Height** (cm)
- **Father’s Height** (cm)
- **IGFBP3 Level** (ng/mL)
- **Sex** (`Male = 0`, `Female = 1`)

---

### ▶️ Prediction Steps

1. After entering all values, click the **"Predict"** button.
2. Switch to the **"Predictions"** tab on the right panel to view results:
   - **Predicted Height**: Graph showing estimated future height with uncertainty ribbons.
   - **Predicted Percentile Score**: Graph showing estimated percentile scores over time.
3. The **"Instruction"** tab provides a visual guide for using the app.

---

## 📈 Output Panels

Two main interactive plots are provided:

- **Predicted Height**: Mean height trajectory and 10–90% prediction intervals.
- **Predicted Percentile Score**: Mean percentile score and uncertainty intervals.

---

## 📂 Folder Structure

