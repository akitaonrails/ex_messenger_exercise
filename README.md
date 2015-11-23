# ExMessenger Package

This is an adaptation of the original ExMessenger example written by [Drew Kerrigan](http://drew.kerrigan.io/ditributed-elixir/).

## How to Run

From different terminals:

  1. start the server like this:

        > cd apps/ex_messenger
        > iex --sname server --cookie chocolate-chip -S mix run

  2. then start the clients like this:

        > cd apps/ex_messenger_client
        > server=server@Hal9000u nick=john elixir --sname client -S mix run

Notice that the domain will be different (likely your machine's name), so change the command line accordingly.