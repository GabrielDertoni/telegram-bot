# Telegram Bot
A simple telegram bot that integrates with the wolfram API to provide step-by-step math question solutions.

## Features
This bot is implemented in Haskell wich means it is completely functional by design and therefore very crash safe.
The project is an attempt to implement clean architecture which gives it more flexibility.

## Instalation
1. Make sure to have the stack build tool already installed as well as GHC in order to compile the project locally.
2. Dowload or clone the project from the git repo.
3. In the command line run `stack install`
4. Crate a .env file with the folowing API keys:
   ```shell
   TELEGRAM_API_KEY=<telegram api key>
   WOLFRAM_APP_ID=<wolfram app id>
   ```

## TODO
- Fix problem with /kill