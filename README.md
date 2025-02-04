# Homogeneity Bias as Differential Sampling Uncertainty in Language Models
 
The abstract for the paper is as follows:

> Prior research show that Large Language Models (LLMs) and Vision-Language Models (VLMs) represent marginalized groups more homogeneously than dominant groups. However, the mechanisms underlying this homogeneity bias remain relatively unexplored. We propose that this bias emerges from systematic differences in the probability distributions from which tokens are sampled at inference-time. Analyzing three measures of uncertainty in token sampling distributions-entropy, perplexity, and probability of differentiation-we find that in some models, specifically GPT-4 Turbo and Llama-3.2, tokens are sampled more deterministically when generating texts about marginalized groups (i.e., Black Americans and women) compared to their dominant group counterparts (i.e., White Americans and men). While these findings may help explain homogeneity bias in certain models, the patterns did not replicate across all VLMs tested, suggesting multiple mechanisms may contribute to homogeneity bias in AI.


## Data Availability Statement

All code and data except for the facial stimuli used for data collection–GANFD (Marsden et al., 2024)–are made available on this repository. To access the facial stimuli, visit their OSF repository [here](https://osf.io/7auyw/). 
