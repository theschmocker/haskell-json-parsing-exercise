# Parsing JSON with Applicative Parser Combinators

Naive JSON parser combinators and a pretty (incomplete) printer

The code here is very much based on what was built in [this video](https://www.youtube.com/watch?v=N9RUqGYuGfw). I watched it the day before I created this repo and I wanted to see if I learned enough from it to implement it myself without looking back at the video or the posted repo. Turns out I did!

Outside of the simple examples (`(+) <$> Just 1 <*> Just 2`), Applicative still makes my brain hurt a little bit, but my intuition for how they can be used to sequence actions while keeping/manipulating some context behind the scenes &mdash; the remaining parser input, in this case &mdash; is a bit better now.