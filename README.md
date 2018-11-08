# Project: Can you unscramble a blurry image? 
![image](figs/example.png)

### [Full Project Description](doc/project3_desc.md)

Term: Fall 2018

+ Team #3
+ Team members
	+ Jiaming Yan
	+ Jiaxi Wu
	+ Sarah Faye Wu
	+ Shaolong Lin
	+ Zhirong Li


+ Project summary: In this project, we create a classification model to enhance the resolution of images. We use gradient boosting model as our baseline model and DSRCNN as our improved model. By cross validation, we create our baseline using 'gbm' package. Then faster and better result is obtained by XGBoost. However, taking 8 features performs better than 24 features in XGBoost might be due to the lack of assigning weights. Finally the DSRCNN outperforms gradient boosting models and the computation is accelerated using TPU.
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md))
+ Feature and super-resolution construction: Jiaxi Wu
+ Baseline Model: Jiaxi Wu(main), Zhirong Li, Sarah Faye Wu
+ XGBoost part: Jiaxi Wu
+ DSRCNN model: Shaolong Lin(main), Jiaming Yan
+ Presentation: Jiaming Yan
+ Organizing github and submission: Jiaxi Wu, Jiaming Yan, Zhirong Li
All team members approve our work presented in our GitHub repository including this contribution statement.

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
