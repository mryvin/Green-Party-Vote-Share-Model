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
<p>This project uses a regression model to predict the Greens/EFA seat share in each country in the EU based on polls of various political issues. The project aims to understand if there is a new "green student" effect, where more EU students are aligned with green parties.</p>
<h2>Background</h2>
<ul>
  <li><b>Research Question:</b> Are gains in seats for Greens/EFA parties within specific European Parliamentary geographies?</li>
  <li><b>Importance:</b> Green parties are rapidly expanding throughout Europe, changing the values that drive votes and bringing new electoral challenges.</li>
  <li><b>Background Focus:</b> Research suggests that Green support comes from younger, more educated people who have a higher social standing and live in cities known for education, income improvement, and political progressiveness.</li>
</ul>
<h2>Data and Methods</h2>
<ul>
  <li><b>Data:</b> Values of various individuals from 2017 to 2020 were collected using global polls.</li>
  <li><b>Sample Size:</b> 1500 individuals, ensuring representation of women across each age group (18-31).</li>
  <li><b>Approach:</b> A regression model was used with the Greens/EFA percentage of seats held by varying independent variables.</li>
</ul>
<h2>Results</h2>
<p>The map shows predicted Green Seat Share by country as per the model's predictions. The initial model was statistically significant at predicting the 2019 European Parliament election results.</p>
<img src="map.png" alt="Predicted Green Seat Share by Country">
<table>
  <tr>
    <th>Regression Model Variables</th>
    <th>Correlation</th>
  </tr>
  <tr>
    <td>Trust people easily or alternatively be somewhat suspicious about other people’s intentions.</td>
    <td>Negative</td>
  </tr>
  <tr>
    <td>Employers do something to help when they see stress symptoms manifest among their employees.</td>
    <td>Negative</td>
  </tr>
  <tr>
    <td>A democratic political system is bad/undesirable (implicit) compared with other forms of government like authoritarianism or totalitarianism.</td>
    <td>Positive</td>
  </tr>
  <tr>
    <td>Confidence in Parliament.</td>
    <td>Positive</td>
  </tr>
</table>
<table>
  <tr>
    <th>Statistical Data</th>
    <th>Value</th>
  </tr>
  <tr>
    <td>P-value</td>
    <td><0.000***</td>
  </tr>
  <tr>
    <td>Adjusted R-squared</td>
    <td>0.7317</td>
  </tr>
</table>
<h2>Conclusions</h2>
<p>The initial model was significant at predicting election results but may still be refined for more precise outcomes. Further study could diversify between gains and specific parties both as an input and output of the Greens/EFA Party in The European Parliament.</p>
<h2>Author</h2>
<p>Michael Ryvkin, Rutgers University</p>
<h2>Source</h2>
<p>This readme file is based on the research poster by the same author, available at <a href="https://bit.ly/3xZ2qL4">https://bit.ly/3xZ2qL4</a></p>
</body>
</html>
