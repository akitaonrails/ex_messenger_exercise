defmodule ExMessengerClient do
  use Application
  alias ExMessengerClient.CLI
  alias ExMessengerClient.ServerProcotol

  def start(_type, _args) do
    server = System.get_env("server")
      |> String.rstrip
      |> String.to_atom

    IO.puts "Connecting to #{server} from #{Node.self} ..."
    Node.set_cookie(Node.self, :"chocolate-chip")
    case Node.connect(server) do
      true -> :ok
      reason ->
        IO.puts "Could not connect to server, reason: #{reason}"
        System.halt(0)
    end

    ExMessengerClient.MessageHandler.start_link(server)
    IO.puts "Connected"

    nick = System.get_env("nick") |> String.rstrip

    case ServerProcotol.connect([server, nick]) do
      {:ok, users} ->
        IO.puts "* Joined the chatroom *"
        IO.puts "* Users in the room: #{users} *"
        IO.puts "* Type /help for options *"
      reason ->
        IO.puts "Could not join chatroom, reason: #{reason}"
        System.halt(0)
    end

    CLI.input_loop([server, nick])
  end
end