# Green-Party-Vote-Share-Model
A model was created to help predict Greens/EFA seat share in each country in the EU based on polls of values of people in these countries. This could help predict future elections or predict Greens/EFA seat share if new EU members are admitted
Dataset can be found here: https://www.worldvaluessurvey.org/WVSEVSjoint2017.jsp

<html>
<head>
<style>
  body {
    font-family: Arial, sans-serif;
    max-width: 800px;
    margin: auto;
  }
  h1, h2, h3 {
    color: green;
  }
  img {
    width: 100%;
  }
  table {
    border-collapse: collapse;
    width: 100%;
  }
  th, td {
    border: 1px solid black;
    padding: 10px;
    text-align: left;
  }
</style>
</head>
<body>
<h1>Predicting Greens/EFA Support in the European Parliament</h1>
<p>This project uses a regression model to predict the Greens/EFA seat share in each country in the EU based on polls of various political issues. The project aims to understand the reasons why people may support or oppose green parties in the EU.</p>
<h2>Background</h2>
<ul>
  <li><b>Research Question:</b> What factors contribute to Greens/EFA parties gaining seats in European Parliamentary elections?</li>
  <li><b>Importance:</b> Green Parties are rapidly expanding throughout Europe as concerns over climate change grow. Understanding the values that these voters have can help predict future Green Party vote share before the elections happen.</li>
  <li><b>Background Focus:</b> Previous Research suggests that Green support comes from younger, more educated people who have a higher social standing and live in less densely populated areas. </li>
</ul>
<h2>Data and Methods</h2>
<ul>
  <li><b>Data:</b> Values of 135000 individuals from 2017 to 2020 were collected using global polls. This was subsetted to only include the individuals who resided in nations which were part of the EU. The average opinions were found by country based on the responses by the individuals from those countries.</li>
  <li><b>Approach:</b> A regression model was used with the Greens/EFA percentage of seats held by varying independent variables. Stepwise regression was utilize to narrow down possible indpendent variables.</li>
</ul>
<h2>Results</h2>
More information about Results can be found here: <a href=Poster.pdf>Poster.pdf</a>
<table>
  <tr>
    <th>Statistical Data</th>
    <th>Value</th>
  </tr>
  <tr>
    <td>P-value</td>
    <td>0.0002569</td>
  </tr>
  <tr>
    <td>Adjusted R-squared</td>
    <td>0.7397</td>
  </tr>
</table>
<h2>Conclusions</h2>
<p>The model was significant at predicting election results but may still be refined for more precise outcomes. One serious issue could come from overfitting. This could be tested further by seaprating data into testing in training data, or by applying the model to future EU elections. Additionally, more research can be conducted to create different buckets of parties rather than the European Parliamentary Groups, which are highly fluid and ununified.</p>
<h2>Author</h2>
<p>Michael Ryvin, Rutgers University</p>
<h2>Source</h2>
<p>The polling data used in this study came from the World Values Survey and is available at <a href="https://www.worldvaluessurvey.org/WVSEVSjoint2017.jsp">https://www.worldvaluessurvey.org/WVSEVSjoint2017.jsp</a></p>
</body>
</html>
