defmodule ExMessengerClient do
  use Application
  alias ExMessengerClient.CLI
  alias ExMessengerClient.ServerProcotol

  def start(_type, _args) do
    get_env
      |> connect
      |> start_message_handler
      |> join_chatroom
      |> CLI.input_loop
  end

  defp get_env do
    server = System.get_env("server")
      |> String.rstrip
      |> String.to_atom

    nick = System.get_env("nick")
      |> String.rstrip

    {server, nick}
  end

  defp connect({server, nick}) do
    IO.puts "Connecting to #{server} from #{Node.self} ..."
    Node.set_cookie(Node.self, :"chocolate-chip")
    case Node.connect(server) do
      true -> :ok
      reason ->
        IO.puts "Could not connect to server, reason: #{reason}"
        System.halt(0)
    end
    {server, nick}
  end

  defp start_message_handler({server, nick}) do
    ExMessengerClient.MessageHandler.start_link(server)
    IO.puts "Connected"
    {server, nick}
  end

  defp join_chatroom({server, nick}) do
    case ServerProcotol.connect({server, nick}) do
      {:ok, users} ->
        IO.puts "* Joined the chatroom *"
        IO.puts "* Users in the room: #{users} *"
        IO.puts "* Type /help for options *"
      reason ->
        IO.puts "Could not join chatroom, reason: #{reason}"
        System.halt(0)
    end
    {server, nick}
  end
end