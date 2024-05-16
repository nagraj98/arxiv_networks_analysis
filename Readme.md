# Analysing clique behavior in academic settings

Understanding relationships among a real-world collaboration network of researchers who have published General Relativity work in Arxiv.

## Introduction
The main aim of our study is to understand the relationships between this real-world network of researchers who have published work in General Relativity and try to model this network using ERGM to gain more insights from it and compare our findings with the findings from the network of researchers who have published work in High Energy Physics.

## Hypotheses
We hypothesize the following:
1. If author i and author j work together, and author j and author k work together, the odds of author i working with author k are high.
2. There will form many small clusters of researchers for example in specific universities
3. There will be a few key opinion leaders who are well connected (have written papers with many other researchers) showing “superstar” phenomena.
4. We also hypothesized small world phenomena should apply in the network. As the academia is known for its tight community and highly connected, we also assume the node needed to create small world phenomena can be lower than the rule of 5.
5. Between different fields of academia, such as High Energy Physics and General Relativity, there will be similarities in network structure patterns. The project is to identify the correctness of the hypotheses mentioned above.

## Analysis Roadmap

### Network Descriptive Analysis
- The data was presented as a directed network, showing duplicate edges, so the team modified and cleaned the data to be used as an undirected network. 
- The network descriptive analysis by analyzing the centrality, betweenness, transitivity, geodesic, and community detection was based on the cleaned data. 
- Specific analysis will be conducted here to test the hypotheses outlined.

### Network Simulation
- The team will conduct network simulation such as ERGM and small-world network analysis. 
- Specific analysis will be conducted here to test the hypotheses outlined, specifically the small-world phenomena hypothesis.