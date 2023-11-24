# How to run:
## DB setup
- Install PostGresql on your machine.
- After installing PostGres, a new user will be created on the machine named 'postgres'
- The psql commandline is owned by user 'postgres'
- Log in to user 'postgres' in your preffered shell. Type this command: `sudo -i -u postgres`. Give your superuser password
- Run `psql`
- Now you will be in postgres command line tool
- Run: `ALTER USER postgres PASSWORD 'postgres';` (Or use your preffered password/settings)
- Run the db script in /src/migrations/ddl.sql

## Run the service
- Install GHCup, Cabal and necessary Haskell tools. https://www.haskell.org/ghcup/install/
- Go to the base directory from your shell. Run:
  ```bash
  cabal update
  cabal install
  cabal run
  ```
- If you need to open up the ghci for the project run: `cabal repl`
