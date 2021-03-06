<style>
.section .reveal .state-background {
    background: black;}
.section .reveal p {
    color: white;font-size: 25pt;position: fixed;font-family:'Courier',cursive;
    top: 60%;}  
.section .reveal h1{
    color: red;font-family:'Magneto',cursive;font-size: 120pt;text-align:centre; position: fixed;
    top: 40%;}
.section .reveal a:link{color: red;}
.section .reveal a:visited{color: white;}
</style>

<style>
.footer {
    color: red;
    background: white;
    position: fixed;
    <!-- top: 90%; -->
    text-align:left;
    width:100%;
}
</style>

MindReader 
========================================================
author: Nathalie Descusse-Brown
date: 30th June 2019
autosize: true



<style>
.reveal .state-background {
    background: black;}
.reveal h1{color: red;}
.reveal a{color: red;}
.reveal .slides section .slideContent h2{
    font-size: 30pt;color: red;font-family:'Courier',cursive;}
p.small {font-size: 15pt;color: red;font-family:'Courier',cursive;}
.reveal p {
    color: white;font-family:'Courier',cursive;font-size: 15pt;text-align:justify;}
</style>

What does the TextPredict app do?
========================================================
title: false
<h2>
What does the MindReader app do?
</h2>

The purpose of the MindReader app is to predict the user's next word.<br>
The user enters a sentence and the app returns the most likely next word.<br><br>

The app was trained on an entire corpus - or collection - of 3 US-originated documents, namely a blog file, a news file and a twitter file. The corpus was tidied up to filter out most 'bad words' (e.g. rude or offensive words). <br><br>
White spaces were removed and the entire corpus was converted to lower case for standardisation.<br><br>
An analysis of bigrams - or sequence of 2 words -, and trigrams - or sequence of 3 words - was performed and all bigrams and trigrams that were found to have a frequency higher than 1 across the entire corpus were stored in a data frame, which forms the dictionary the app looks up when the user enters an input.


What does the TextPredict app look like?
========================================================
title:false
<h2>What does the TextPredict app look like?</h2>

The left hand side of the app is reserved for the user input, where the user is prompted to enter any sentence.
<br><br>
The next predicted word is then displayed in red in the main panel. Et voila!

![](MindReader_screenshot.png)

<br><br>
<br><br>


<style>
.reveal a:link{color: red;}
.reveal a:visited{color: green;}
</style>

How to use the MindReader app?
========================================================
title:false
<h2>How to use the MindReader app?</h2>

The user can enter any sentence and the app will return the most likely word based on the analysis performed of the corpus.
The function used to look up the dictionary and match it to the user input is "textpredictbitrigram.R".<br><br>
The below code shows what is returned when the function is called within the app.

<font size="4.5">

```r
source('textpredictbitrigram.R')
DTngram <- fread("DTbitrisplit.csv",quote="")
textpredictbitrigram("you mean the world to",DTngram)
```

```
[1] "me"
```
</font>

<h2>How does it perform?</h2>

Benchmarking was performed with the benchmark.R dataset provided by <a>https://github.com/hfoffani/dsci-benchmark</a>. The results are reported below:
![](algorithm_performance.png)

<style>
.reveal ul, 
.reveal ol {
    font-size: 50px;
    color: white;
    font-family: 'Courier';
    text-align:justify;
}


</style>

How does the MindReader app work?
========================================================
title:false
<h2>How does the MindReader app work?</h2>

The app works as such:
<ol >
<li style="font-size:15pt">
First search the app dictionary for all trigrams starting with the last two words of the input and return the last word of the trigram with the highest frequency.</li>
<li style="font-size:15pt">If no matching trigram is found then the app looks up all bigrams starting with the last word of the input and and return the second word of the bigram with the highest frequency.</li>
<li style="font-size:15pt">If no matching bigram is found then the most common unigram (single word), "the", is returned by the app.</li>
</ol>


What does the future hold for the MindReader app?
========================================================
title:false
<h2>What does the future hold for the MindReader app?</h2>

A few considerations for improvement of the current MindReader app:
<ol >
<li style="font-size:15pt">The accuracy of the prediction could be further improved by using quad and 5-grams - or sequences of 4 and 5 words, respectively - also when looking up the dictionary.
However the memory requirements will need to be addressed as even with the efficient read R function 'fread', looking up a dictionary containing quad and 5-grams currently significantly slows down the app. This is something that will need to be looked into.</li>
<li style="font-size:15pt">Although most 'bad words' have been removed, some words containing symbols have been left in and further investigation of the meaningfulness of these specific words will have to be further assessed to judge whether their removal would affect accuracy of the prediction.</li>




