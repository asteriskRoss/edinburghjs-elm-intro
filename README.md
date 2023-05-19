# Introduction to Elm for JavaScript Developers

## Overview

This is a single page web application written in [Elm](https://elm-lang.org).  It displays the slides for a presentation I delivered at the EdinburghJS meetup on Thursday 18 May 2023.  The intention was that the audience would have an opportunity to look at a simple but real-world app to accompany the presentation.

If you are interested in learning Elm I highly recommend working through the [official Elm guide](https://guide.elm-lang.org/).

## Installing Elm

You can:
- Follow the [official instructions for installing Elm](https://guide.elm-lang.org/install/elm.html)
- Install the [npm package](https://www.npmjs.com/package/elm) with `npm install -g elm@latest-0.19.1`
- Install your OS's port/package, for example:
  - [Debian](https://packages.debian.org/bookworm/elm-compiler): `sudo apt install elm-compiler`
  - [FreeBSD](https://www.freshports.org/lang/elm/): `pkg install lang/elm`
  - [MacOS with Homebrew](https://formulae.brew.sh/formula/elm): `brew install elm`.

## Running the Elm app

You can use the Elm development server, called reactor:
`elm reactor`

By default this will start a [local web server](http://localhost:8000/src/Main.elm) and you can navigate to src/Main.elm.

## Building the Elm app

Should you wish to deploy it to production(!):
`elm make src/Main.elm --optimize`

The Elm guide also has [a suggestion for how to use uglifyjs to minify code](https://guide.elm-lang.org/optimization/asset_size.html) that I use for deploying production apps.

## Known issues

The slides aren't properly responsive and are styled for a 1080p landscape screen. Many of them won't render properly on smartphones etc.
