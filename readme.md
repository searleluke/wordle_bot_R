# WordleBot

A Wordle bot with a few functions, all run from the integrate.R script.

Data is pulled directly (although manually) from [NY Times Wordle](https://www.nytimes.com/games/wordle/index.html) source code, and has been updated since the NYTimes removed 'obscure' answers and 'rude' guesses.

The bot is pretty good at Wordle. Performance is not as good as some of the best Wordle bots available, but is still not bad. Current best strategy (starting with 'reast') averages 3.46 turns to finish, and never takes more than 5 turns.

All algorithm processing results have been saved for easy access. Processing is vectorised and quite fast, but are still non-trivial to run, especially the raw simulations.

The algorithm could probably be improved by pushing more towards 'minimise expected turns' and away from 'maximise expected information'. The two are related but not perfectly so.

### The WordleBot's fun tricks:

#### Play Wordle

An extremely simple little recreation of the game. Run play_wordle() to play in the console. Picks a random word from the NY Times Wordle answer list, and lets you guess. Outputs results with colours. 

Does not even try to help you play optimally. Not even a little.


#### Find 'optimal' strategy

##### 1: Get best opening word (first layer)

Uses the [Shannon Entropy Method](https://en.wikipedia.org/wiki/Entropy_(information_theory)) to get a hueristic for 'best'. Check out [3Blue1Brown's video](https://www.youtube.com/watch?v=v68zYyaEmEA) for intuition.

Goes 1 layer deep for all possible guesses.

##### 2: Get best opening word and second guess for all possible results (second layer)

Goes 2 layers deep using the same method, but processing times start to get a little bit high. Only applies to the top 100 words from the first layer. Could do more, but I frankly didn't see the point.

##### 3: Simulate results (the main algorithm)

Combines the above into a 'get next best guess' function. Then plays a lot of wordle with it to find the top performing openers. Makes a bunch of graphs about it.

I've done this for the best 50 words after going 2 layers deep. You could do more, but the heuristic is pretty accurate so far.

#### Cheat at Wordle

Lets you input your results from playing the real game. Lists which words are remaining after your guesses and results. Uses the 'get next best guess' function to suggest what you should pick next. Will play better than you, or your money back.


