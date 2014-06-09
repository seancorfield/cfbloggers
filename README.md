# cfbloggers

Some scratch Clojure code to pull down the list of RSS feeds from [ColdFusionBloggers.org](http://coldfusionbloggers.org) and analyze it, and produce an [HTML page of active, broken, and dead CFML blogs](http://corfield.org/articles/cfbloggers.html).

## Usage

lein run -m cfbloggers.core {N}

Where N is the number of feeds you want to analyze. It defaults to 30 but there are currently about 700 feeds on that site so you only get the full results with N large enough to check them all.

The code writes an HTML file and a JSON file to specific locations that are hard-coded in the code. Yeah, I know, that sucks, but really no one else is going to be running this...

## License

Copyright © 2014 Sean Corfield

Distributed under the Eclipse Public License either version 1.0 like pretty much all Clojure code.
