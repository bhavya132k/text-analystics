# Tarzan Text Analytics Project

This repository contains the text analytics project analyzing the book "Tarzan of the Apes" by Edgar Rice Burroughs. The project was conducted for the Big Data and Analytics course (CS4907/CS6444).

## Project Overview

The project aimed to analyze text data from "Tarzan of the Apes," focusing on chapters I-XV. The analysis included:
- **Data Preparation:** Extracting and processing the text to prepare it for analysis.
- **Text Analytics:** Applying various analytical techniques to extract insights from the text.
- **Visualization:** Creating visual representations to understand data patterns and relationships.

## Contents
- `plots/`: Contains visualizations, including word clouds, dendrograms, and sentiment percentage values for all 15 chapters.
- `text-analytics.R`: R script containing the working source code used for text analytics.
- `text-analytics-doc.pdf`: Comprehensive documentation detailing the methods, results, and analyses.
- `chapters/`: Contains individual chapters extracted from the text, created by the R program.
- `TarzanOfTheApes.txt`: The text of "Tarzan of the Apes" by Edgar Rice Burroughs.

## Report Summary
### Analytical Methods
- **Word Frequency Analysis:** Identify the most frequent words in each chapter.
- **Dendrograms:** Visualize the hierarchical clustering of chapters.
- **Word Clouds:** Visualize the word frequency for each chapter.
- **Sentiment Analysis:** Analyze the emotional tones across the text.

## Usage
To replicate this project on your system:
1. Clone the repository: `git clone <repository-url>`
2. Install the required R packages: `dplyr`, `ggplot2`, `readr`, `stringr`, `wordcloud`, `tm`, `quanteda`, `syuzhet`
3. Run the `text-analytics.R` script to reproduce the analyses.

## License
This project is licensed under the MIT License. See the `LICENSE` file for details.
