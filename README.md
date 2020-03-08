# hacker-news-reader

A simple Hacker News reader command line application experiment written over a weekend in Haskell.

## About

For a while I had wanted to experiment with the following technologies:
- [Polysemy](https://github.com/polysemy-research/polysemy): An algebraic effects library that allows you to combine effects in a clear and concise manner.
- [Servant](https://docs.servant.dev/en/stable/): A web app library that allows the user to specify type safe servers and clients via their APIs.
- [Brick](https://github.com/jtdaugherty/brick): A library for creating simple declarative terminal guis.

With all of this in mind, I decided to implement a simple Hacker News reader that would allow me to read the comments on some of the top posts from the command line. I kept the scope of the application small throughout its development to really focus on writing a decent DSL with Polysemy and interpreting those effects with Brick and Servant. This means that features like arbitrary item/user lookup and stories displayed beyond the top 30 are missing (but could easily be implemented at another time).

This simple application served as a great tool to play around with the technologies that I was interested in, and made for a fun weekend.

## Usage
Compile and run with `stack run` if you're using stack.

#### Inputs:
- Arrow keys to move the cursor up/down.
- 'u' displays the user info of the selected item.
- Enter displays the selected post.
- 'q' returns to the previous screen, or exits the application if there is none.
- Escape exits the application.
