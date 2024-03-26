Predicting Inventory Needs
================

In this repository/directory you should see the following items:

- `README.md` - this document.
- `mini-competition.Rmd` - an empty RMarkdown file for you to keep track
  of your explorations and provide detailed notes.
- `data` - a folder that contains the Excel datafile

## The “rules”

This is a culminating experience for STA 631’s second learning module:
Generalized Linear Models. Therefore, you must fit, assess, and
interpret a (or several) GLMs to a surprise dataset (revealed below).

Then, **by the start of our class meeting on Tue, Mar 28** you and your
team must have created a Google Slide deck tor present your results. At
least one member from each team will provide a 5-minute maximum
presentation of these slides. This presentation can only contain three
total slides:

1.  One title slide (with only your team member’s names and presentation
    title),
2.  A reflection of the process your team took to explore and decide on
    your final model (go beyond stating the steps you took - explain
    why), and
3.  Your final model and a description of how “good” it fits the data
    (you will need to explain how you determined “good-ness”).

I may ask clarifying questions at the end of each presentation and we
will hopefully have time at the end to have a closing conversation. To
assist in creating slides quickly (so you can focus on the modeling
task), we will use Google Slides instead of creating [slides in
RMarkdown](https://rmarkdown.rstudio.com/lesson-11.html). Upload your
slides to the appropriate Google folder linked below before the start of
our class meeting:

- [Section 01 (4 - 5:15
  pm)](https://drive.google.com/drive/folders/1jraxLZkPZwV1CndX1UI3iQG95KrcTJoS?usp=drive_link)
- [Section 02 (2:30 - 3:45
  pm)](https://drive.google.com/drive/folders/1GZp6xS7JY8TXNv9WCv-zGEF7r-vKUscM?usp=drive_link)

Presentation order will be randomly determined at the start of class and
edits to your slides will not be allowed.

While this is a friendly competition, the main purpose is to share your
ideas/process and learn from your peers and their ideas/process. I
(Bradford) will determine the “best” group based *only* on your
presentation by assessing each of the following categories on a
“missing/attempted/sufficient”-scale (or 0/0.5/1 point-scale). I will be
very [stingy](https://www.merriam-webster.com/dictionary/stingy) with
awarding “1”s except for the last category.

- Clarity of your presentation (Did you seem prepared and have a plan of
  what you were going to say?),
- Soundness of your approach (Does the direction your team went make
  sense based on what we have covered in class?),
- Aesthetically pleasing presentation (Did you do something beyond the
  default output to make your presentation easy for your audience to
  view your work?), and
- Maximum of 5-minutes and title + two slides (only 0 or 1 possible).

The best group based on these categories will receive a prize/gift.

### Your task

You and your team have been “hired” by GVSU’s [K-12
Connect](https://www.gvsu.edu/k12connect/) to assess factors that might
contribute to students’ academic success. Your primary goal is to
analyze data from the [Parent and Family Involvement
(PFI)](https://www.census.gov/programs-surveys/nhes.html) in Education
Surveys to provide insights on factors impacting K-12 education. More
specifically, you need to determine the “best” model to provide
recommendations to GVSU’s K-12 Connect on the impacts of school choice
and family engagement in school activities and homework. This is a more
broad research topic (you are deciding on the response *and* explanatory
variables). The only requirement is that you fit GLM(s).

While we have not explored assessing generalized linear models for count
data in class, it is part of your task to research and implement
assessing the “goodness” of your predictions.

### Data

GVSU’s K-12 Connect obtained data from the Parent and Family Involvement
(PFI) in Education dataset that is collected by the US Census Bureau for
the Department of Education. You are provided with two years worth of
data (2016 and 2019) in the `data` folder. Note that this is one Excel
file with each year as separate tab in the data file.

This is a relatively large (i.e., wide) data file and I encourage you to
use the [Parent and Family Involvement
(PFI)](https://www.census.gov/programs-surveys/nhes.html) resource page
to answer questions about variables. You will likely need to do some
digging.

As this might be your first experience using academic data, here are
some examples. Note that the following is copied from This is
Statistic’s Fall Data Challenge announcement page. I am not providing
you a direct link because students’ presentations are included and I do
not want you to feel tempted to simply copy these. Also, their research
task was slightly different than yours - you must use a statistical
modeling method while they did not need to. Also also, the presentations
from the Fall Data Challenge were done by undergraduate students - you
have a more advanced skill level than what they had. Happy to provide
you with a direct link after the competition.

#### In the News

Today’s students will lead the future, so it’s no wonder the question of
how to advance their success is a popular topic in media coverage. With
the recent years of remote and hybrid learning, and many returning to
in-person classrooms, interest in the topic has surged.

Here are some recent headlines that highlight the status of familial
involvement in their children’s K-12 education:

K-12 Dive: [Parents encouraged to talk to schools about students’ math
progress](https://www.k12dive.com/news/parents-encouraged-to-talk-to-schools-about-students-math-progress/629353/)

The NWEA research organization found that student achievement in
mathematics for the 2021-2022 school year has significantly declined
since the pandemic. While schools explore different learning recovery
opportunities to boost academic achievement in K-12 students, one
campaign recommends the involvement of parents and guardians to help
practice their math skills with their children. This article explores
how parents can aid the improvement of their children’s student success.

Public School Review: [Parental Involvement is Key to Student
Success](https://www.publicschoolreview.com/blog/parental-involvement-is-key-to-student-success)

Research has shown that reading at home can improve children’s reading
skills. This article shares how parental and guardian involvement in
their children’s K-12 education could have a positive impact on a
child’s academic performance and boost the morale of their teachers.

The 74: [The Biggest Blind Spot in Education: Parents’ Role in Their
Children’s
Learning](https://www.the74million.org/article/the-biggest-blind-spot-in-education-parents-role-in-their-childrens-learning/)

Family members can have a larger impact on a student’s public school
academic performance than teachers, according to this analysis. The 2020
national test results revealed the pandemic had a significant effect on
student education, leading to an overall decline in test scores in the
past decade. This article explores a connection to family involvement
for student performance.

#### Tools & Research

For the 2022 Fall Data Challenge, students will apply their statistical
skills to analyze real-world K-12 educational experience data and
provide recommendations and insights supported by their analysis.

In addition to the provided PFI dataset, students have the option to
utilize additional datasets on the topic to inform their analysis. Here
are some examples of tools and resources that explore the topic from
different perspectives.

[NAEP Data Explorers](https://www.nationsreportcard.gov/ndecore/landing)

This dataset tool from the National Assessment of Educational Progress
(NAEP) and the National Center for Education Statistics provides results
and factors related to student education from The Nation’s Report Card
assessments throughout the years.

[The Urban Institute Data
Catalog](https://datacatalog.urban.org/search/type/dataset)

This collection of open data contributed by researchers and data
scientists of the Urban Institute allows individuals access to public
datasets that explore various societal issues.

## Suggestions to get started

- Make appropriate exploratory graphs and numerical summary tables for
  each variable and pairs of variables. Note that there are some
  qualitative variables in the dataset. Also, do you need to consider
  any potential interaction terms or polynomial terms?
- Explore if you need to do anything to ensure the reference groups
  listed in the variable description table.
- What methods have we explored to assess a model’s “good-ness”? As a
  group, come up with this list and what each tells us. In [Additional
  Methods](#additional-methods) below, I provide some further
  descriptive (i.e., not statistical inference focused) methods to check
  for “good-ness”.
- Fit your candidate models.
- Assess which of your candidates is “best”.

## Additional resources

We have not directly practiced methods for handling this type of
response variable. However, you were encouraged to read Sections 4.6 and
4.7 from *ISL* that show similar methods.

- I have shared Emil Hvitfeldt’s *ISLR tidymodels labs* which uses
  `{tidymodels}` to cover topics that *ISL* does in base R. This will
  take you to his chapter corresponding to the [Classficiation
  labs](https://emilhvitfeldt.github.io/ISLR-tidymodels-labs/04-classification.html).
- UCLA’s Advanced Research Computing team provides great examples for
  various [statistical learning
  problems](https://stats.oarc.ucla.edu/other/dae/).
- The University of Wisconsin - Madison’s Social Science Computing
  Cooperative provides a good overview of [Generalized Linear Models in
  R](https://sscc.wisc.edu/sscc/pubs/glm-r/index.html).
- *Tidy Modeling with R* by Max Kuhn and Julia Silge has great
  examples/discussion on working with [generalized linear
  models](https://www.tmwr.org/inferential.html).

## What is next?

Next week will explore methods that are more computationally heavy for
assessing models and selecting model features beginning with resampling
methods (Chapter 5).
