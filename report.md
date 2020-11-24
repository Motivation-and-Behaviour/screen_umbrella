---
title: "Report on Effect Size Translation"
ipsum_meta:
  twitter_card: "Summary info for the Twitter Card"
  twitter_site: "\\@sitehandle"
  twitter_creator: "\\@creatorhandle"
  og_url: "https\\://example.com/open/graph/finalURLfor/this"
  og_description: "A modest size description of the content"
  og_image: "https\\://example.com/open/graph/imageURLfor/this"
output: 
  hrbrthemes::ipsum:
    toc: true
---





# Assumptions made

I made the following assumptions which may or may not be reasonable:

1.  That $\beta$ is equivilent to $r$ for $\beta < |.5|$ and undefined otherwise (never the case here)
2.  That, given the large sample sizes involved *Hedge's g* and *Cohen's d* where essentially equivalent and that sample sizes used to make up these effects were approximately equal in treatment and control groups.
3.  In the absence of information that could be used to construct a 2x2 contingency table that ORs and Relative Risk Ratios were equivalent and thus Relative Risk Ratios were converted to $r$ as if they were Odds ratios.
4.  External to any other information that all forms of SMD, weighted means etc. were equivilent to Cohen's d.

# Correspondence

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#wxxjhffgfp .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#wxxjhffgfp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wxxjhffgfp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#wxxjhffgfp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#wxxjhffgfp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wxxjhffgfp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wxxjhffgfp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#wxxjhffgfp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#wxxjhffgfp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wxxjhffgfp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wxxjhffgfp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#wxxjhffgfp .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#wxxjhffgfp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#wxxjhffgfp .gt_from_md > :first-child {
  margin-top: 0;
}

#wxxjhffgfp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wxxjhffgfp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#wxxjhffgfp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#wxxjhffgfp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wxxjhffgfp .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#wxxjhffgfp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wxxjhffgfp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wxxjhffgfp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wxxjhffgfp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wxxjhffgfp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wxxjhffgfp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#wxxjhffgfp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wxxjhffgfp .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#wxxjhffgfp .gt_left {
  text-align: left;
}

#wxxjhffgfp .gt_center {
  text-align: center;
}

#wxxjhffgfp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wxxjhffgfp .gt_font_normal {
  font-weight: normal;
}

#wxxjhffgfp .gt_font_bold {
  font-weight: bold;
}

#wxxjhffgfp .gt_font_italic {
  font-style: italic;
}

#wxxjhffgfp .gt_super {
  font-size: 65%;
}

#wxxjhffgfp .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="wxxjhffgfp" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="3" class="gt_heading gt_title gt_font_normal" style>Coversion of effect sizes</th>
    </tr>
    <tr>
      <th colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>New Effect Name - Old Effect Name</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">New Name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Old Name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">n</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">r</td>
      <td class="gt_row gt_left">r</td>
      <td class="gt_row gt_center">62</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">d</td>
      <td class="gt_row gt_left">cohen's d</td>
      <td class="gt_row gt_center">26</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">d</td>
      <td class="gt_row gt_left">g+</td>
      <td class="gt_row gt_center">13</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">or</td>
      <td class="gt_row gt_left">or</td>
      <td class="gt_row gt_center">12</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">b</td>
      <td class="gt_row gt_left">beta</td>
      <td class="gt_row gt_center">8</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">z</td>
      <td class="gt_row gt_left">z fisher</td>
      <td class="gt_row gt_center">7</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">d</td>
      <td class="gt_row gt_left">hedges' g</td>
      <td class="gt_row gt_center">6</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">or</td>
      <td class="gt_row gt_left">pooled odds ratio</td>
      <td class="gt_row gt_center">6</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">or</td>
      <td class="gt_row gt_left">odds ratio iv</td>
      <td class="gt_row gt_center">3</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">or</td>
      <td class="gt_row gt_left">pooled fixed effect -odd ratio</td>
      <td class="gt_row gt_center">3</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">d</td>
      <td class="gt_row gt_left">adjusted smd</td>
      <td class="gt_row gt_center">1</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">d</td>
      <td class="gt_row gt_left">weighted mean</td>
      <td class="gt_row gt_center">1</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">or</td>
      <td class="gt_row gt_left">odd ratio</td>
      <td class="gt_row gt_center">1</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">or</td>
      <td class="gt_row gt_left">relative risk</td>
      <td class="gt_row gt_center">1</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">z</td>
      <td class="gt_row gt_left">z</td>
      <td class="gt_row gt_center">1</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->

# Effect Size Summary: Point Estimates

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#wjzbrqzhmi .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#wjzbrqzhmi .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wjzbrqzhmi .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#wjzbrqzhmi .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#wjzbrqzhmi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wjzbrqzhmi .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wjzbrqzhmi .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#wjzbrqzhmi .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#wjzbrqzhmi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wjzbrqzhmi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wjzbrqzhmi .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#wjzbrqzhmi .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#wjzbrqzhmi .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#wjzbrqzhmi .gt_from_md > :first-child {
  margin-top: 0;
}

#wjzbrqzhmi .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wjzbrqzhmi .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#wjzbrqzhmi .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#wjzbrqzhmi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wjzbrqzhmi .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#wjzbrqzhmi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wjzbrqzhmi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wjzbrqzhmi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wjzbrqzhmi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wjzbrqzhmi .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wjzbrqzhmi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#wjzbrqzhmi .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wjzbrqzhmi .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#wjzbrqzhmi .gt_left {
  text-align: left;
}

#wjzbrqzhmi .gt_center {
  text-align: center;
}

#wjzbrqzhmi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wjzbrqzhmi .gt_font_normal {
  font-weight: normal;
}

#wjzbrqzhmi .gt_font_bold {
  font-weight: bold;
}

#wjzbrqzhmi .gt_font_italic {
  font-style: italic;
}

#wjzbrqzhmi .gt_super {
  font-size: 65%;
}

#wjzbrqzhmi .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="wjzbrqzhmi" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="5" class="gt_heading gt_title gt_font_normal" style>Effect Size Summary</th>
    </tr>
    <tr>
      <th colspan="5" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Descriptives by Effect Size Type</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">std_eff_name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">SD</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Min</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Max</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">b</td>
      <td class="gt_row gt_right">&minus;0.053</td>
      <td class="gt_row gt_right">0.054</td>
      <td class="gt_row gt_right">&minus;0.120</td>
      <td class="gt_row gt_right">0.010</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">d</td>
      <td class="gt_row gt_right">0.091</td>
      <td class="gt_row gt_right">0.107</td>
      <td class="gt_row gt_right">&minus;0.139</td>
      <td class="gt_row gt_right">0.359</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">or</td>
      <td class="gt_row gt_right">0.117</td>
      <td class="gt_row gt_right">0.101</td>
      <td class="gt_row gt_right">&minus;0.048</td>
      <td class="gt_row gt_right">0.359</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">r</td>
      <td class="gt_row gt_right">0.031</td>
      <td class="gt_row gt_right">0.096</td>
      <td class="gt_row gt_right">&minus;0.157</td>
      <td class="gt_row gt_right">0.212</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">z</td>
      <td class="gt_row gt_right">0.322</td>
      <td class="gt_row gt_right">0.276</td>
      <td class="gt_row gt_right">0.178</td>
      <td class="gt_row gt_right">0.999</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->

# Project Structure
<img src="figure/unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" width="504" />

