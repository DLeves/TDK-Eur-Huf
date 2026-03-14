# The Relationship between the EUR/HUF Exchange Rate and the Media Presence of Hungarian Economic Policy Makers

### Authors:
- **Levente Dittrich** (Actuarial and Financial Mathematics)
- **Ákos Virág** (Social Data Science)

### Supervisor:
- **László Kovács**

---

### Project Overview
This repository contains the data, scraping algorithms, and modeling scripts for an empirical analysis of the EUR/HUF exchange rate between **May 31, 2018, and February 15, 2025**. The study investigates how economic fundamentals and the media sentiment of key Hungarian economic actors influence exchange rate levels and volatility.

**Methodological Framework:**
* **Sentiment Analysis:** Articles were scraped from *Magyar Hang*, translated via DeepL, and scored using **FinBERT**, a model pre-trained on financial corpora.
* **Econometrics:** Linear **ARIMAX-GARCHX** (specifically **eGARCH**) models were utilized to examine impacts on the conditional mean and variance.
* **Deep Learning:** **LSTM** (Long Short-Term Memory) neural networks were implemented to capture non-linear interactions and state-dependent effects.

---

### Repository Structure

```text
.
├── ARIMAX-GARCHX/          # R scripts for statistical modeling
│   ├── arimax_garchx.R     # Main econometric modeling script
│   ├── final_fx_.R         # Finalized FX analysis routines
│   └── IC_es_egyutthatotablak.xlsx  # Information criteria and coefficients
├── LSTM/                   # Python notebooks and scripts for Deep Learning
│   ├── LSTM.ipynb          # Main LSTM training and evaluation
│   ├── lstm_functions.py   # Helper functions for the neural network
│   ├── LSTM_optimal_lags.ipynb # Lag optimization via elbow method
│   └── [results files]     # RMSE, fits, and ceteris paribus CSV/XLSX outputs
├── Sentiment analysis/     # Text processing pipeline
│   ├── sentiment_analysis.ipynb # FinBERT implementation
│   └── [processed data]    # Translated articles and sentiment scores
├── plots/                  # Visualizations used in the thesis
│   ├── acf_pacf_vertical.png # Correlograms for stationarity checks
│   ├── coef_mean.svg       # Coefficient robustness (Mean equation)
│   ├── coef_var.svg        # Coefficient robustness (Variance equation)
│   ├── VaR_alpha001_top01.svg # Value-at-Risk reliability plot
│   └── valtozo_gyakorisag_mean_vs_var.svg # Frequency of variables in top models
├── webscrape/              # Data collection scripts
│   ├── tdk_webscrape.Rmd   # RMarkdown script for article harvesting
│   └── [raw data]          # Articles and policy rate CSV files
├── full_dataset_20250830.csv # Integrated dataset (Financial + Sentiment)
└── README.md
```

### Key Variables
* **Dependent Variable**: EUR/HUF daily log-returns.
* **Policymakers**: Sentiment scores for Mihály Varga (VM), Márton Nagy (NM), and György Matolcsy (MGY).
* **Communication**: Cabinet Briefing (KI) sentiment scores.
* **Controls**:
    * **Regional Currencies**: EUR/PLN, EUR/CZK, and EUR/RON returns.
    * **Policy Rates**: MNB base rate, FED funds rate, and ECB policy rate.
    * **Market Indices**: BUX (Hungarian) and CETOP (Central European) stock indices.

---

### Principal Findings
* **Level Determinants**: The exchange rate level is primarily driven by regional currency co-movements (PLN, CZK) and historical price inertia.
* **Volatility Drivers**: Domestic policy signals (MNB) and Cabinet Briefings function as uncertainty-generating shocks that increase market volatility.
* **Machine Learning Performance**: LSTM models significantly outperform baseline linear models by capturing non-linear sentiment interactions and state-dependent effects.
* **Asymmetric Volatility**: Volatility follows an asymmetric eGARCH process where negative innovations (bad news) exert a larger impact on risk than positive news.

---

### Usage
* **Data Collection**: Use `webscrape/tdk_webscrape.Rmd` to extract article bodies from the *Magyar Hang* archives via specific metadata tags.
* **Econometrics**: Execute `ARIMAX-GARCHX/arimax_garchx.R` to perform the ARIMAX-eGARCH(1,1) estimations and frequency analysis across the top-performing mixed specifications.
* **Deep Learning**: Run `LSTM/LSTM.ipynb` for the neural network training pipeline (8-neuron layers, ReLU activation) and XAI (Explainable AI) visualization.

---

### License
This repository is for academic use. Direct citation of the underlying article is required for any publication or derivative work using the processed sentiment dataset.
